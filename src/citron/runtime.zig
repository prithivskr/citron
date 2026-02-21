const std = @import("std");
const types = @import("types.zig");
const regex_subset = @import("regex_subset.zig");

pub fn Runtime(comptime g: types.Grammar, comptime B: type) type {
    return struct {
        const Token = struct {
            term_idx: u16,
            start: usize,
            end: usize,
            sp: types.SourcePoint,
        };

        fn colForTerm(term_idx: u16) usize {
            return @as(usize, term_idx);
        }

        fn actionAt(state_idx: usize, term_idx: u16) B.Action {
            return B.built.actions[state_idx][colForTerm(term_idx)];
        }

        fn gotoAt(state_idx: usize, nt_idx: u16) u16 {
            return B.built.gotos[state_idx][@as(usize, nt_idx)];
        }

        fn trace(opts: types.ParseOptions, comptime fmt: []const u8, args: anytype) void {
            if (opts.verbose) std.debug.print(fmt, args);
        }

        fn bumpSourcePoint(sp: *types.SourcePoint, bytes: []const u8) void {
            for (bytes) |c| {
                sp.index += 1;
                if (c == '\n') {
                    sp.line += 1;
                    sp.column = 1;
                } else {
                    sp.column += 1;
                }
            }
        }

        fn isWhitespace(c: u8, skip_newline: bool) bool {
            return switch (c) {
                ' ', '\t', '\r', 0x0B, 0x0C => true,
                '\n' => skip_newline,
                else => false,
            };
        }

        fn skipWhitespace(opts: types.ParseOptions, input: []const u8, pos: *usize, sp: *types.SourcePoint) void {
            if (!opts.skip_whitespace) return;
            while (pos.* < input.len and isWhitespace(input[pos.*], opts.skip_newline)) {
                bumpSourcePoint(sp, input[pos.* .. pos.* + 1]);
                pos.* += 1;
            }
        }

        fn matchTerm(term: types.TermSpec, input: []const u8, pos: usize) usize {
            return switch (term.kind) {
                .literal => |lit| if (std.mem.startsWith(u8, input[pos..], lit)) lit.len else 0,
                .matcher => |mf| mf(input, pos),
                .regex => |pattern| regex_subset.matchPrefix(pattern, input, pos),
            };
        }

        fn nextToken(opts: types.ParseOptions, input: []const u8, pos: *usize, sp: *types.SourcePoint) types.ParseFailure!Token {
            while (true) {
                skipWhitespace(opts, input, pos, sp);
                if (pos.* >= input.len) {
                    return .{
                        .term_idx = B.EOF_IDX,
                        .start = pos.*,
                        .end = pos.*,
                        .sp = sp.*,
                    };
                }

                var best_len: usize = 0;
                var best_idx: ?u16 = null;
                for (g.terms, 0..) |term, i| {
                    const idx: u16 = @intCast(i);
                    const m = matchTerm(term, input, pos.*);
                    if (m > best_len or (m == best_len and m > 0 and (best_idx == null or idx < best_idx.?))) {
                        best_len = m;
                        best_idx = idx;
                    }
                }
                if (best_idx == null or best_len == 0) return types.ParseFailure.LexError;

                const idx = best_idx.?;
                const start = pos.*;
                const end = pos.* + best_len;
                const token_sp = sp.*;
                bumpSourcePoint(sp, input[start..end]);
                pos.* = end;
                if (g.terms[idx].skip) continue;

                return .{
                    .term_idx = idx,
                    .start = start,
                    .end = end,
                    .sp = token_sp,
                };
            }
        }

        fn tokenNode(term_idx: u16, input: []const u8, token: Token) types.AstNode {
            return .{
                .symbol = types.symT(term_idx),
                .token = .{
                    .lexeme = input[token.start..token.end],
                    .sp = token.sp,
                },
            };
        }

        pub fn parse(allocator: std.mem.Allocator, input: []const u8, opts: types.ParseOptions, ctx: ?*anyopaque) types.ParseFailure!types.Ast {
            _ = ctx;
            var cursor = std.ArrayListUnmanaged(u16){};
            defer cursor.deinit(allocator);
            var node_stack = std.ArrayListUnmanaged(usize){};
            defer node_stack.deinit(allocator);
            var nodes = std.ArrayListUnmanaged(types.AstNode){};
            defer nodes.deinit(allocator);
            var edges = std.ArrayListUnmanaged(usize){};
            defer edges.deinit(allocator);

            try cursor.append(allocator, 0);

            var pos: usize = 0;
            var sp: types.SourcePoint = .{};
            var look = try nextToken(opts, input, &pos, &sp);
            var recovery_mode = false;
            var consume_mode = false;

            while (true) {
                const st = cursor.items[cursor.items.len - 1];
                const look_term: u16 = if (recovery_mode) B.ERR_IDX else look.term_idx;
                const action = actionAt(st, look_term);

                if (action.kind == .err) {
                    if (consume_mode) {
                        if (look.term_idx == B.EOF_IDX) return types.ParseFailure.ParseError;
                        trace(opts, "{d}:{d}: recovery consume token {s}\n", .{ look.sp.line, look.sp.column, B.termName(look.term_idx) });
                        look = try nextToken(opts, input, &pos, &sp);
                        continue;
                    }

                    if (!recovery_mode) {
                        trace(opts, "{d}:{d}: syntax error, unexpected {s}\n", .{ look.sp.line, look.sp.column, B.termName(look.term_idx) });
                        recovery_mode = true;
                    }

                    var shifted_recovery = false;
                    while (cursor.items.len > 0) {
                        const top = cursor.items[cursor.items.len - 1];
                        const rec_action = actionAt(top, B.ERR_IDX);
                        if (rec_action.kind == .shift_error_token or rec_action.kind == .shift) {
                            const node_idx = nodes.items.len;
                            try nodes.append(allocator, .{
                                .symbol = types.symT(B.ERR_IDX),
                            });
                            try cursor.append(allocator, rec_action.arg);
                            try node_stack.append(allocator, node_idx);
                            recovery_mode = false;
                            consume_mode = true;
                            shifted_recovery = true;
                            trace(opts, "recovery: shifted <error> to state {d}\n", .{rec_action.arg});
                            break;
                        }
                        _ = cursor.pop();
                        if (node_stack.items.len > 0) _ = node_stack.pop();
                    }
                    if (!shifted_recovery) return types.ParseFailure.ParseError;
                    continue;
                }

                if (consume_mode) consume_mode = false;

                switch (action.kind) {
                    .shift => {
                        trace(opts, "{d}:{d}: shift {s} -> state {d}\n", .{ look.sp.line, look.sp.column, B.termName(look.term_idx), action.arg });
                        const node_idx = nodes.items.len;
                        try nodes.append(allocator, tokenNode(look.term_idx, input, look));
                        try cursor.append(allocator, action.arg);
                        try node_stack.append(allocator, node_idx);
                        look = try nextToken(opts, input, &pos, &sp);
                    },
                    .shift_error_token => {
                        trace(opts, "shift synthetic <error> -> state {d}\n", .{action.arg});
                        const node_idx = nodes.items.len;
                        try nodes.append(allocator, .{
                            .symbol = types.symT(B.ERR_IDX),
                        });
                        try cursor.append(allocator, action.arg);
                        try node_stack.append(allocator, node_idx);
                        recovery_mode = false;
                        consume_mode = true;
                    },
                    .reduce => {
                        const r_idx = action.arg;
                        const rhs_len = B.ruleRhsLen(action.arg);
                        if (node_stack.items.len < rhs_len or cursor.items.len < rhs_len + 1) return types.ParseFailure.ParseError;

                        const child_start = node_stack.items.len - rhs_len;
                        const edge_start = edges.items.len;
                        try edges.appendSlice(allocator, node_stack.items[child_start..]);

                        node_stack.items.len = child_start;
                        cursor.items.len -= rhs_len;

                        const lhs = B.ruleLhs(action.arg);
                        const goto_state = gotoAt(cursor.items[cursor.items.len - 1], lhs);
                        if (goto_state == std.math.maxInt(u16)) return types.ParseFailure.ParseError;

                        const node_idx = nodes.items.len;
                        try nodes.append(allocator, .{
                            .symbol = types.symN(lhs),
                            .rule_idx = r_idx,
                            .first_child = edge_start,
                            .child_count = rhs_len,
                        });
                        try cursor.append(allocator, goto_state);
                        try node_stack.append(allocator, node_idx);
                        trace(opts, "reduce rule {d}, goto {d}\n", .{ r_idx, goto_state });
                    },
                    .accept => {
                        trace(opts, "accept\n", .{});
                        if (node_stack.items.len == 0) return types.ParseFailure.ParseError;

                        const owned_nodes = try nodes.toOwnedSlice(allocator);
                        errdefer allocator.free(owned_nodes);
                        const owned_edges = try edges.toOwnedSlice(allocator);

                        return .{
                            .nodes = owned_nodes,
                            .edges = owned_edges,
                            .root = node_stack.items[node_stack.items.len - 1],
                            .allocator = allocator,
                        };
                    },
                    .err => unreachable,
                }
            }
        }

        pub fn writeDiagnostics(writer: anytype) !void {
            try writer.print("states: {d}\n", .{B.built.state_count});
            var s: usize = 0;
            while (s < B.built.state_count) : (s += 1) {
                try writer.print("state {d}\n", .{s});
                var i: usize = 0;
                while (i < B.ITEM_SPACE) : (i += 1) {
                    if (B.built.states[s].items.has(i)) {
                        const info = B.decodeItem(@intCast(i));
                        try writer.print("  [{d}] {s} ->", .{ i, B.ntermName(B.ruleLhs(info.rule_idx)) });
                        var p: usize = 0;
                        while (p < B.ruleRhsLen(info.rule_idx)) : (p += 1) {
                            if (p == info.dot) try writer.writeAll(" •");
                            const sm = B.ruleRhsSymbol(info.rule_idx, p);
                            if (sm.is_term) {
                                try writer.print(" {s}", .{B.termName(sm.idx)});
                            } else {
                                try writer.print(" {s}", .{B.ntermName(sm.idx)});
                            }
                        }
                        if (info.dot == B.ruleRhsLen(info.rule_idx)) try writer.writeAll(" •");
                        try writer.print(", {s}]\n", .{B.termName(info.lookahead)});
                    }
                }
            }
        }
    };
}
