const std = @import("std");

const invalid_state = std.math.maxInt(u16);

const RegexError = std.mem.Allocator.Error || error{
    InvalidRegex,
};

const StateKind = enum {
    epsilon,
    split,
    byte,
    dot,
    class,
    match,
};

const State = struct {
    kind: StateKind,
    out1: u16 = invalid_state,
    out2: u16 = invalid_state,
    byte: u8 = 0,
    class_idx: u16 = invalid_state,
};

const CharClass = struct {
    negate: bool = false,
    bits: [32]u8 = [_]u8{0} ** 32,

    fn set(self: *CharClass, c: u8) void {
        const slot = @as(usize, c) / 8;
        const bit: u3 = @intCast(c % 8);
        self.bits[slot] |= @as(u8, 1) << bit;
    }

    fn addRange(self: *CharClass, from: u8, to: u8) void {
        var lo = from;
        var hi = to;
        if (lo > hi) std.mem.swap(u8, &lo, &hi);

        var i: u16 = lo;
        while (i <= hi) : (i += 1) {
            self.set(@intCast(i));
        }
    }

    fn contains(self: CharClass, c: u8) bool {
        const slot = @as(usize, c) / 8;
        const bit: u3 = @intCast(c % 8);
        const has = (self.bits[slot] & (@as(u8, 1) << bit)) != 0;
        return if (self.negate) !has else has;
    }
};

const Frag = struct {
    start: u16,
    end: u16,
};

const Escape = union(enum) {
    byte: u8,
    digit,
    word,
    space,
};

const Builder = struct {
    allocator: std.mem.Allocator,
    states: std.ArrayListUnmanaged(State) = .{},
    classes: std.ArrayListUnmanaged(CharClass) = .{},

    fn deinit(self: *Builder) void {
        self.states.deinit(self.allocator);
        self.classes.deinit(self.allocator);
    }

    fn addState(self: *Builder, st: State) RegexError!u16 {
        if (self.states.items.len >= std.math.maxInt(u16)) return error.InvalidRegex;
        try self.states.append(self.allocator, st);
        return @intCast(self.states.items.len - 1);
    }

    fn patchEnd(self: *Builder, end_state: u16, target: u16) RegexError!void {
        const idx = @as(usize, end_state);
        if (idx >= self.states.items.len) return error.InvalidRegex;
        var st = &self.states.items[idx];
        if (st.kind != .epsilon or st.out1 != invalid_state) return error.InvalidRegex;
        st.out1 = target;
    }

    fn addClass(self: *Builder, cls: CharClass) RegexError!u16 {
        if (self.classes.items.len >= std.math.maxInt(u16)) return error.InvalidRegex;
        try self.classes.append(self.allocator, cls);
        return @intCast(self.classes.items.len - 1);
    }

    fn makeEmpty(self: *Builder) RegexError!Frag {
        const end = try self.addState(.{ .kind = .epsilon });
        const start = try self.addState(.{
            .kind = .epsilon,
            .out1 = end,
        });
        return .{ .start = start, .end = end };
    }

    fn makeByte(self: *Builder, b: u8) RegexError!Frag {
        const end = try self.addState(.{ .kind = .epsilon });
        const start = try self.addState(.{
            .kind = .byte,
            .byte = b,
            .out1 = end,
        });
        return .{ .start = start, .end = end };
    }

    fn makeDot(self: *Builder) RegexError!Frag {
        const end = try self.addState(.{ .kind = .epsilon });
        const start = try self.addState(.{
            .kind = .dot,
            .out1 = end,
        });
        return .{ .start = start, .end = end };
    }

    fn makeClass(self: *Builder, cls_idx: u16) RegexError!Frag {
        const end = try self.addState(.{ .kind = .epsilon });
        const start = try self.addState(.{
            .kind = .class,
            .class_idx = cls_idx,
            .out1 = end,
        });
        return .{ .start = start, .end = end };
    }

    fn concat(self: *Builder, a: Frag, b: Frag) RegexError!Frag {
        try self.patchEnd(a.end, b.start);
        return .{ .start = a.start, .end = b.end };
    }

    fn alternate(self: *Builder, a: Frag, b: Frag) RegexError!Frag {
        const end = try self.addState(.{ .kind = .epsilon });
        try self.patchEnd(a.end, end);
        try self.patchEnd(b.end, end);
        const start = try self.addState(.{
            .kind = .split,
            .out1 = a.start,
            .out2 = b.start,
        });
        return .{ .start = start, .end = end };
    }

    fn star(self: *Builder, f: Frag) RegexError!Frag {
        const end = try self.addState(.{ .kind = .epsilon });
        const start = try self.addState(.{
            .kind = .split,
            .out1 = f.start,
            .out2 = end,
        });
        try self.patchEnd(f.end, start);
        return .{ .start = start, .end = end };
    }

    fn plus(self: *Builder, f: Frag) RegexError!Frag {
        const end = try self.addState(.{ .kind = .epsilon });
        const split = try self.addState(.{
            .kind = .split,
            .out1 = f.start,
            .out2 = end,
        });
        try self.patchEnd(f.end, split);
        return .{ .start = f.start, .end = end };
    }

    fn optional(self: *Builder, f: Frag) RegexError!Frag {
        const end = try self.addState(.{ .kind = .epsilon });
        try self.patchEnd(f.end, end);
        const start = try self.addState(.{
            .kind = .split,
            .out1 = f.start,
            .out2 = end,
        });
        return .{ .start = start, .end = end };
    }
};

const Parser = struct {
    pattern: []const u8,
    i: usize = 0,
    b: *Builder,

    fn parse(self: *Parser) RegexError!u16 {
        const frag = try self.parseAlternation();
        if (self.i != self.pattern.len) return error.InvalidRegex;

        const accept = try self.b.addState(.{ .kind = .match });
        try self.b.patchEnd(frag.end, accept);
        return frag.start;
    }

    fn parseAlternation(self: *Parser) RegexError!Frag {
        var left = try self.parseConcat();
        while (self.i < self.pattern.len and self.peek() == '|') {
            _ = self.consume();
            const right = try self.parseConcat();
            left = try self.b.alternate(left, right);
        }
        return left;
    }

    fn parseConcat(self: *Parser) RegexError!Frag {
        var left: ?Frag = null;
        while (self.i < self.pattern.len) {
            const c = self.peek();
            if (c == ')' or c == '|') break;
            const right = try self.parsePostfix();
            if (left) |l| {
                left = try self.b.concat(l, right);
            } else {
                left = right;
            }
        }
        return left orelse self.b.makeEmpty();
    }

    fn parsePostfix(self: *Parser) RegexError!Frag {
        var frag = try self.parseAtom();
        while (self.i < self.pattern.len) {
            switch (self.peek()) {
                '*' => {
                    _ = self.consume();
                    frag = try self.b.star(frag);
                },
                '+' => {
                    _ = self.consume();
                    frag = try self.b.plus(frag);
                },
                '?' => {
                    _ = self.consume();
                    frag = try self.b.optional(frag);
                },
                else => break,
            }
        }
        return frag;
    }

    fn parseAtom(self: *Parser) RegexError!Frag {
        if (self.i >= self.pattern.len) return error.InvalidRegex;
        return switch (self.peek()) {
            '(' => blk: {
                _ = self.consume();
                const sub = try self.parseAlternation();
                if (self.i >= self.pattern.len or self.consume() != ')') return error.InvalidRegex;
                break :blk sub;
            },
            '[' => self.parseClass(),
            '.' => blk: {
                _ = self.consume();
                break :blk try self.b.makeDot();
            },
            '\\' => blk: {
                _ = self.consume();
                const esc = try self.parseEscape();
                break :blk switch (esc) {
                    .byte => |v| try self.b.makeByte(v),
                    else => try self.makeEscapedClass(esc),
                };
            },
            ')', '|', '*', '+', '?' => error.InvalidRegex,
            else => blk: {
                const c = self.consume();
                break :blk try self.b.makeByte(c);
            },
        };
    }

    fn parseClass(self: *Parser) RegexError!Frag {
        if (self.consume() != '[') return error.InvalidRegex;

        var cls = CharClass{};
        if (self.i < self.pattern.len and self.peek() == '^') {
            _ = self.consume();
            cls.negate = true;
        }

        var has_item = false;
        while (true) {
            if (self.i >= self.pattern.len) return error.InvalidRegex;
            if (self.peek() == ']' and has_item) {
                _ = self.consume();
                break;
            }

            const first = try self.parseClassUnit();
            has_item = true;
            if (first == .byte and
                first.byte != '-' and
                self.i < self.pattern.len and
                self.peek() == '-' and
                self.i + 1 < self.pattern.len and
                self.pattern[self.i + 1] != ']')
            {
                _ = self.consume();
                const second = try self.parseClassUnit();
                if (second != .byte) return error.InvalidRegex;
                cls.addRange(first.byte, second.byte);
                continue;
            }
            addEscapeToClass(&cls, first);
        }

        const cls_idx = try self.b.addClass(cls);
        return self.b.makeClass(cls_idx);
    }

    fn parseClassUnit(self: *Parser) RegexError!Escape {
        if (self.i >= self.pattern.len) return error.InvalidRegex;
        if (self.peek() != '\\') {
            return .{ .byte = self.consume() };
        }
        _ = self.consume();
        return self.parseEscape();
    }

    fn parseEscape(self: *Parser) RegexError!Escape {
        if (self.i >= self.pattern.len) return error.InvalidRegex;
        const c = self.consume();
        return switch (c) {
            'n' => .{ .byte = '\n' },
            'r' => .{ .byte = '\r' },
            't' => .{ .byte = '\t' },
            'd' => .digit,
            'w' => .word,
            's' => .space,
            'x' => blk: {
                if (self.i + 1 >= self.pattern.len) return error.InvalidRegex;
                const hi = hexValue(self.pattern[self.i]) orelse return error.InvalidRegex;
                const lo = hexValue(self.pattern[self.i + 1]) orelse return error.InvalidRegex;
                self.i += 2;
                break :blk .{ .byte = (hi << 4) | lo };
            },
            else => .{ .byte = c },
        };
    }

    fn makeEscapedClass(self: *Parser, esc: Escape) RegexError!Frag {
        var cls = CharClass{};
        addEscapeToClass(&cls, esc);
        const cls_idx = try self.b.addClass(cls);
        return self.b.makeClass(cls_idx);
    }

    fn peek(self: *Parser) u8 {
        return self.pattern[self.i];
    }

    fn consume(self: *Parser) u8 {
        const c = self.pattern[self.i];
        self.i += 1;
        return c;
    }
};

fn hexValue(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => 10 + c - 'a',
        'A'...'F' => 10 + c - 'A',
        else => null,
    };
}

fn addEscapeToClass(cls: *CharClass, esc: Escape) void {
    switch (esc) {
        .byte => |b| cls.set(b),
        .digit => cls.addRange('0', '9'),
        .word => {
            cls.addRange('a', 'z');
            cls.addRange('A', 'Z');
            cls.addRange('0', '9');
            cls.set('_');
        },
        .space => {
            cls.set(' ');
            cls.set('\t');
            cls.set('\r');
            cls.set('\n');
            cls.set(0x0B);
            cls.set(0x0C);
        },
    }
}

fn addClosure(
    states: []const State,
    active: *std.ArrayListUnmanaged(u16),
    seen: []bool,
    allocator: std.mem.Allocator,
    idx: u16,
) RegexError!void {
    if (idx == invalid_state) return;
    const i = @as(usize, idx);
    if (i >= states.len) return error.InvalidRegex;
    if (seen[i]) return;
    seen[i] = true;

    const st = states[i];
    switch (st.kind) {
        .epsilon => {
            if (st.out1 != invalid_state) try addClosure(states, active, seen, allocator, st.out1);
        },
        .split => {
            if (st.out1 != invalid_state) try addClosure(states, active, seen, allocator, st.out1);
            if (st.out2 != invalid_state) try addClosure(states, active, seen, allocator, st.out2);
        },
        else => try active.append(allocator, idx),
    }
}

fn containsAccept(states: []const State, list: []const u16) bool {
    for (list) |idx| {
        if (states[idx].kind == .match) return true;
    }
    return false;
}

pub fn matchPrefix(pattern: []const u8, input: []const u8, pos: usize) usize {
    if (pos > input.len) return 0;

    var scratch = std.heap.stackFallback(4096, std.heap.page_allocator);
    const allocator = scratch.get();

    var builder = Builder{ .allocator = allocator };
    defer builder.deinit();

    var parser = Parser{
        .pattern = pattern,
        .b = &builder,
    };
    const start_state = parser.parse() catch return 0;

    if (builder.states.items.len == 0) return 0;

    var current = std.ArrayListUnmanaged(u16){};
    defer current.deinit(allocator);
    var next = std.ArrayListUnmanaged(u16){};
    defer next.deinit(allocator);

    const seen_a = allocator.alloc(bool, builder.states.items.len) catch return 0;
    defer allocator.free(seen_a);
    const seen_b = allocator.alloc(bool, builder.states.items.len) catch return 0;
    defer allocator.free(seen_b);

    @memset(seen_a, false);
    addClosure(builder.states.items, &current, seen_a, allocator, start_state) catch return 0;

    var curr_seen = seen_a;
    var next_seen = seen_b;
    var best_len: usize = if (containsAccept(builder.states.items, current.items)) 0 else 0;

    var i = pos;
    while (i < input.len and current.items.len > 0) : (i += 1) {
        next.clearRetainingCapacity();
        @memset(next_seen, false);
        const c = input[i];

        for (current.items) |state_idx| {
            const st = builder.states.items[state_idx];
            switch (st.kind) {
                .byte => {
                    if (c == st.byte) {
                        addClosure(builder.states.items, &next, next_seen, allocator, st.out1) catch return 0;
                    }
                },
                .dot => {
                    if (c != '\n') {
                        addClosure(builder.states.items, &next, next_seen, allocator, st.out1) catch return 0;
                    }
                },
                .class => {
                    if (st.class_idx == invalid_state) return 0;
                    if (builder.classes.items[st.class_idx].contains(c)) {
                        addClosure(builder.states.items, &next, next_seen, allocator, st.out1) catch return 0;
                    }
                },
                .match => {},
                .epsilon, .split => unreachable,
            }
        }

        if (next.items.len == 0) break;

        std.mem.swap(std.ArrayListUnmanaged(u16), &current, &next);
        std.mem.swap([]bool, &curr_seen, &next_seen);
        if (containsAccept(builder.states.items, current.items)) best_len = i - pos + 1;
    }

    return best_len;
}

test "subset regex matchPrefix basics" {
    try std.testing.expectEqual(@as(usize, 3), matchPrefix("[0-9]+", "123abc", 0));
    try std.testing.expectEqual(@as(usize, 2), matchPrefix("ab|cd", "cdx", 0));
    try std.testing.expectEqual(@as(usize, 5), matchPrefix("a(b|c)*d", "abccd!", 0));
    try std.testing.expectEqual(@as(usize, 1), matchPrefix("\\d", "9x", 0));
    try std.testing.expectEqual(@as(usize, 3), matchPrefix("\\w+", "abc-", 0));
    try std.testing.expectEqual(@as(usize, 0), matchPrefix("a+", "bbb", 0));
}
