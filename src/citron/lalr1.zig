const std = @import("std");
const types = @import("types.zig");
const util = @import("util.zig");

pub fn Builder(comptime g: types.Grammar) type {
    comptime {
        @setEvalBranchQuota(200_000_000);
        if (g.terms.len == 0) @compileError("grammar requires at least one terminal");
        if (g.nterms.len == 0) @compileError("grammar requires at least one nonterminal");
        if (g.rules.len == 0) @compileError("grammar requires at least one rule");
        if (g.root >= g.nterms.len) @compileError("grammar.root out of range");
    }

    const term_count_user: usize = g.terms.len;
    const nterm_count_user: usize = g.nterms.len;
    const eof_idx: u16 = @intCast(term_count_user);
    const err_idx: u16 = @intCast(term_count_user + 1);
    const term_count: usize = term_count_user + 2;
    const fake_root_idx: u16 = @intCast(nterm_count_user);
    const nterm_count: usize = nterm_count_user + 1;
    const root_rule_idx: u16 = @intCast(g.rules.len);
    const rule_count: usize = g.rules.len + 1;
    const symbol_count: usize = nterm_count + term_count;

    const max_rhs_len = comptime blk: {
        var m: usize = 1;
        for (g.rules) |r| {
            if (r.rhs.len > m) m = r.rhs.len;
        }
        break :blk m;
    };
    const dot_size = max_rhs_len + 1;
    const core_space = rule_count * dot_size;
    const item_space = rule_count * dot_size * term_count;

    const limits = g.limits;
    const state_cap = limits.state_cap;
    const max_items_per_state = limits.max_items_per_state;

    comptime {
        if (state_cap < 2) @compileError("limits.state_cap must be >= 2");
        if (max_items_per_state == 0) @compileError("limits.max_items_per_state must be > 0");
    }

    const TermSet = util.Bitset(term_count);
    const CoreSet = util.Bitset(core_space);
    const ItemSet = util.Bitset(item_space);
    const ItemVec = util.BoundedVec(u32, max_items_per_state);

    return struct {
        const Self = @This();

        pub const TERM_COUNT_USER = term_count_user;
        pub const NTERM_COUNT_USER = nterm_count_user;
        pub const EOF_IDX = eof_idx;
        pub const ERR_IDX = err_idx;
        pub const TERM_COUNT = term_count;
        pub const NTERM_COUNT = nterm_count;
        pub const RULE_COUNT = rule_count;
        pub const SYMBOL_COUNT = symbol_count;
        pub const CORE_SPACE = core_space;
        pub const ITEM_SPACE = item_space;

        pub const ActionKind = enum(u8) {
            err,
            shift,
            shift_error_token,
            reduce,
            accept,
        };

        pub const Action = struct {
            kind: ActionKind = .err,
            arg: u16 = 0,
            has_sr_conflict: bool = false,
        };

        pub const State = struct {
            items: ItemSet = .{},
            kernel: ItemSet = .{},
            vec: ItemVec = .{},
        };

        pub const Built = struct {
            state_count: usize,
            states: [state_cap]State,
            transitions: [state_cap][symbol_count]u16,
            actions: [state_cap][term_count]Action,
            gotos: [state_cap][nterm_count]u16,
        };

        pub const ItemInfo = struct {
            rule_idx: u16,
            dot: u16,
            lookahead: u16,
        };

        pub const built = build();

        pub fn termName(idx: u16) []const u8 {
            if (idx < term_count_user) return g.terms[idx].name;
            if (idx == eof_idx) return "<eof>";
            return "<error>";
        }

        pub fn ntermName(idx: u16) []const u8 {
            if (idx < nterm_count_user) return g.nterms[idx].name;
            return "<root>";
        }

        pub fn ruleLhs(rule_idx: u16) u16 {
            if (rule_idx < g.rules.len) return g.rules[rule_idx].lhs;
            return fake_root_idx;
        }

        pub fn ruleRhsLen(rule_idx: u16) usize {
            if (rule_idx < g.rules.len) return g.rules[rule_idx].rhs.len;
            return 1;
        }

        pub fn ruleRhsSymbol(rule_idx: u16, pos: usize) types.Symbol {
            if (rule_idx < g.rules.len) return g.rules[rule_idx].rhs[pos];
            return types.symN(g.root);
        }

        fn makeItem(rule_idx: u16, dot: u16, lookahead: u16) u32 {
            return @as(u32, rule_idx) * @as(u32, dot_size) * @as(u32, term_count) +
                @as(u32, dot) * @as(u32, term_count) +
                @as(u32, lookahead);
        }

        fn makeCore(rule_idx: u16, dot: u16) u32 {
            return @as(u32, rule_idx) * @as(u32, dot_size) + @as(u32, dot);
        }

        pub fn decodeItem(item_idx: u32) ItemInfo {
            const lookahead: u16 = @intCast(item_idx % term_count);
            const tmp = item_idx / term_count;
            const dot: u16 = @intCast(tmp % dot_size);
            const rule_idx: u16 = @intCast(tmp / dot_size);
            return .{ .rule_idx = rule_idx, .dot = dot, .lookahead = lookahead };
        }

        fn addItem(state: *State, item_idx: u32, to_kernel: bool) void {
            if (state.items.set(item_idx)) {
                state.vec.push(item_idx) catch @compileError("max_items_per_state exceeded; increase limits.max_items_per_state");
                if (to_kernel) _ = state.kernel.set(item_idx);
            }
        }

        fn stateKernelCore(state: State) CoreSet {
            var out: CoreSet = .{};
            var i: usize = 0;
            while (i < state.vec.len) : (i += 1) {
                const item_idx = state.vec.data[i];
                if (!state.kernel.has(item_idx)) continue;
                const info = decodeItem(item_idx);
                _ = out.set(makeCore(info.rule_idx, info.dot));
            }
            return out;
        }

        fn mergeStateItems(dest: *State, src: State) void {
            var i: usize = 0;
            while (i < src.vec.len) : (i += 1) {
                const item_idx = src.vec.data[i];
                addItem(dest, item_idx, src.kernel.has(item_idx));
            }
        }

        fn firstNullable() struct {
            nullable: [nterm_count]bool,
            first: [nterm_count]TermSet,
        } {
            var nullable: [nterm_count]bool = [_]bool{false} ** nterm_count;
            var first: [nterm_count]TermSet = undefined;
            for (&first) |*f| f.* = .{};

            var changed = true;
            while (changed) {
                changed = false;
                var r: u16 = 0;
                while (r < rule_count) : (r += 1) {
                    const lhs = ruleLhs(r);
                    const rhs_len = ruleRhsLen(r);

                    var rhs_nullable = true;
                    var accum: TermSet = .{};
                    var i: usize = 0;
                    while (i < rhs_len) : (i += 1) {
                        const sm = ruleRhsSymbol(r, i);
                        if (sm.is_term) {
                            _ = accum.set(sm.idx);
                            rhs_nullable = false;
                            break;
                        } else {
                            _ = accum.unionWith(first[sm.idx]);
                            if (!nullable[sm.idx]) {
                                rhs_nullable = false;
                                break;
                            }
                        }
                    }

                    if (first[lhs].unionWith(accum)) changed = true;
                    if (rhs_nullable and !nullable[lhs]) {
                        nullable[lhs] = true;
                        changed = true;
                    }
                }
            }

            return .{ .nullable = nullable, .first = first };
        }

        fn firstOfSuffix(rule_idx: u16, start: usize, nullable: [nterm_count]bool, first: [nterm_count]TermSet) struct { terms: TermSet, nullable: bool } {
            var out: TermSet = .{};
            var seq_nullable = true;
            var i = start;
            while (i < ruleRhsLen(rule_idx)) : (i += 1) {
                const sm = ruleRhsSymbol(rule_idx, i);
                if (sm.is_term) {
                    _ = out.set(sm.idx);
                    seq_nullable = false;
                    break;
                }
                _ = out.unionWith(first[sm.idx]);
                if (!nullable[sm.idx]) {
                    seq_nullable = false;
                    break;
                }
            }
            return .{ .terms = out, .nullable = seq_nullable };
        }

        fn closure(state: *State, nullable: [nterm_count]bool, first: [nterm_count]TermSet) void {
            var i: usize = 0;
            while (i < state.vec.len) : (i += 1) {
                const info = decodeItem(state.vec.data[i]);
                const rhs_len = ruleRhsLen(info.rule_idx);
                if (info.dot >= rhs_len) continue;

                const next = ruleRhsSymbol(info.rule_idx, info.dot);
                if (next.is_term) continue;

                const fs = firstOfSuffix(info.rule_idx, info.dot + 1, nullable, first);
                var r: u16 = 0;
                while (r < rule_count) : (r += 1) {
                    if (ruleLhs(r) != next.idx) continue;

                    var t: usize = 0;
                    while (t < term_count) : (t += 1) {
                        if (fs.terms.has(t)) addItem(state, makeItem(r, 0, @intCast(t)), false);
                    }
                    if (fs.nullable) addItem(state, makeItem(r, 0, info.lookahead), false);
                }
            }
        }

        fn stateEql(a: State, b: State) bool {
            return ItemSet.eql(a.items, b.items);
        }

        fn symbolForCol(col: usize) types.Symbol {
            if (col < nterm_count) return types.symN(@intCast(col));
            return types.symT(@intCast(col - nterm_count));
        }

        fn findState(states: *const [state_cap]State, state_count: usize, target: State) ?u16 {
            var i: usize = 0;
            while (i < state_count) : (i += 1) {
                if (stateEql(states[i], target)) return @intCast(i);
            }
            return null;
        }

        fn rulePrecedence(rule_idx: u16) struct { prec: i32, assoc: types.Assoc } {
            if (rule_idx >= g.rules.len) return .{ .prec = 0, .assoc = .no_assoc };
            const r = g.rules[rule_idx];
            if (r.precedence_override) |p| {
                return .{ .prec = p, .assoc = r.assoc_override orelse .no_assoc };
            }
            var i: usize = r.rhs.len;
            while (i > 0) {
                i -= 1;
                const sm = r.rhs[i];
                if (sm.is_term and sm.idx < term_count_user) {
                    const t = g.terms[sm.idx];
                    return .{ .prec = t.precedence, .assoc = t.assoc };
                }
            }
            return .{ .prec = 0, .assoc = .no_assoc };
        }

        fn termPrecAssoc(term_idx: u16) struct { prec: i32, assoc: types.Assoc } {
            if (term_idx < term_count_user) {
                const t = g.terms[term_idx];
                return .{ .prec = t.precedence, .assoc = t.assoc };
            }
            return .{ .prec = 0, .assoc = .no_assoc };
        }

        fn setAction(actions: *[term_count]Action, term_idx: u16, new_action: Action) void {
            const idx = @as(usize, term_idx);
            var slot = &actions[idx];
            if (slot.kind == .err) {
                slot.* = new_action;
                return;
            }

            const old = slot.*;
            if ((old.kind == .shift or old.kind == .shift_error_token) and new_action.kind == .reduce) {
                const tp = termPrecAssoc(term_idx);
                const rp = rulePrecedence(new_action.arg);
                if (tp.prec > rp.prec) {
                    slot.has_sr_conflict = true;
                    return;
                }
                if (tp.prec < rp.prec) {
                    slot.* = new_action;
                    slot.has_sr_conflict = true;
                    return;
                }
                switch (rp.assoc) {
                    .left => {
                        slot.* = new_action;
                        slot.has_sr_conflict = true;
                    },
                    .right => {
                        slot.has_sr_conflict = true;
                    },
                    .no_assoc => @compileError(std.fmt.comptimePrint("unresolved S/R conflict on term {s}", .{termName(term_idx)})),
                }
                return;
            }

            if (old.kind == .reduce and (new_action.kind == .shift or new_action.kind == .shift_error_token)) {
                setAction(actions, term_idx, old);
                return;
            }

            if (old.kind == .reduce and new_action.kind == .reduce) {
                if (old.arg != new_action.arg) {
                    @compileError(std.fmt.comptimePrint("R/R conflict on term {s}: rules {d} and {d}", .{ termName(term_idx), old.arg, new_action.arg }));
                }
                return;
            }

            if ((old.kind == .shift or old.kind == .shift_error_token) and (new_action.kind == .shift or new_action.kind == .shift_error_token)) {
                if (old.arg != new_action.arg) {
                    @compileError("shift/shift conflict with different targets");
                }
                return;
            }

            if (old.kind != new_action.kind or old.arg != new_action.arg) {
                @compileError("unsupported parse table conflict");
            }
        }

        fn build() Built {
            @setEvalBranchQuota(200_000_000);
            const sentinel = std.math.maxInt(u16);

            var canonical_states: [state_cap]State = undefined;
            for (&canonical_states) |*s| s.* = .{};

            var canonical_transitions: [state_cap][symbol_count]u16 = undefined;
            for (&canonical_transitions) |*row| {
                for (row) |*v| v.* = sentinel;
            }

            const fnf = firstNullable();

            addItem(&canonical_states[0], makeItem(root_rule_idx, 0, eof_idx), true);
            closure(&canonical_states[0], fnf.nullable, fnf.first);

            var canonical_state_count: usize = 1;
            var current: usize = 0;
            while (current < canonical_state_count) : (current += 1) {
                var col: usize = 0;
                while (col < symbol_count) : (col += 1) {
                    const sm = symbolForCol(col);
                    var candidate: State = .{};
                    var i: usize = 0;
                    while (i < canonical_states[current].vec.len) : (i += 1) {
                        const info = decodeItem(canonical_states[current].vec.data[i]);
                        if (info.dot >= ruleRhsLen(info.rule_idx)) continue;
                        const next = ruleRhsSymbol(info.rule_idx, info.dot);
                        if (next.is_term != sm.is_term or next.idx != sm.idx) continue;
                        addItem(&candidate, makeItem(info.rule_idx, info.dot + 1, info.lookahead), true);
                    }

                    if (candidate.items.isEmpty()) continue;
                    closure(&candidate, fnf.nullable, fnf.first);

                    if (findState(&canonical_states, canonical_state_count, candidate)) |existing| {
                        canonical_transitions[current][col] = existing;
                    } else {
                        if (canonical_state_count >= state_cap) @compileError("limits.state_cap exceeded");
                        canonical_states[canonical_state_count] = candidate;
                        canonical_transitions[current][col] = @intCast(canonical_state_count);
                        canonical_state_count += 1;
                    }
                }
            }

            var states: [state_cap]State = undefined;
            for (&states) |*s| s.* = .{};

            var transitions: [state_cap][symbol_count]u16 = undefined;
            for (&transitions) |*row| {
                for (row) |*v| v.* = sentinel;
            }

            var kernels: [state_cap]CoreSet = undefined;
            for (&kernels) |*k| k.* = .{};

            var canonical_to_lalr: [state_cap]u16 = [_]u16{sentinel} ** state_cap;
            var state_count: usize = 0;

            var c: usize = 0;
            while (c < canonical_state_count) : (c += 1) {
                const kernel = stateKernelCore(canonical_states[c]);
                var merged_idx: ?u16 = null;
                var s: usize = 0;
                while (s < state_count) : (s += 1) {
                    if (CoreSet.eql(kernels[s], kernel)) {
                        merged_idx = @intCast(s);
                        break;
                    }
                }

                const to = if (merged_idx) |idx| idx else blk: {
                    if (state_count >= state_cap) @compileError("limits.state_cap exceeded while merging LALR(1) states");
                    kernels[state_count] = kernel;
                    const fresh: u16 = @intCast(state_count);
                    state_count += 1;
                    break :blk fresh;
                };

                canonical_to_lalr[c] = to;
                mergeStateItems(&states[to], canonical_states[c]);
            }

            c = 0;
            while (c < canonical_state_count) : (c += 1) {
                const from = canonical_to_lalr[c];
                var col: usize = 0;
                while (col < symbol_count) : (col += 1) {
                    const can_to = canonical_transitions[c][col];
                    if (can_to == sentinel) continue;
                    const to = canonical_to_lalr[can_to];

                    const slot = &transitions[from][col];
                    if (slot.* == sentinel) {
                        slot.* = to;
                    } else if (slot.* != to) {
                        @compileError("inconsistent transition while merging LALR(1) states");
                    }
                }
            }

            var actions: [state_cap][term_count]Action = undefined;
            var gotos: [state_cap][nterm_count]u16 = undefined;
            for (&actions) |*row| {
                for (row) |*a| {
                    a.* = .{};
                }
            }
            for (&gotos) |*row| {
                for (row) |*v| {
                    v.* = sentinel;
                }
            }

            var s: usize = 0;
            while (s < state_count) : (s += 1) {
                var nt: usize = 0;
                while (nt < nterm_count) : (nt += 1) {
                    const to = transitions[s][nt];
                    if (to != sentinel) gotos[s][nt] = to;
                }

                var t: usize = 0;
                while (t < term_count) : (t += 1) {
                    const col = nterm_count + t;
                    const to = transitions[s][col];
                    if (to != sentinel) {
                        const kind: ActionKind = if (t == err_idx) .shift_error_token else .shift;
                        setAction(&actions[s], @intCast(t), .{ .kind = kind, .arg = to });
                    }
                }

                var i: usize = 0;
                while (i < states[s].vec.len) : (i += 1) {
                    const info = decodeItem(states[s].vec.data[i]);
                    if (info.dot != ruleRhsLen(info.rule_idx)) continue;

                    if (info.rule_idx == root_rule_idx and info.lookahead == eof_idx) {
                        setAction(&actions[s], eof_idx, .{ .kind = .accept, .arg = 0 });
                    } else {
                        setAction(&actions[s], info.lookahead, .{ .kind = .reduce, .arg = info.rule_idx });
                    }
                }
            }

            return .{
                .state_count = state_count,
                .states = states,
                .transitions = transitions,
                .actions = actions,
                .gotos = gotos,
            };
        }
    };
}
