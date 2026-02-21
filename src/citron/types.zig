const std = @import("std");

pub const Assoc = enum {
    no_assoc,
    left,
    right,
};

pub const Symbol = struct {
    is_term: bool,
    idx: u16,
};

pub fn symT(idx: u16) Symbol {
    return .{ .is_term = true, .idx = idx };
}

pub fn symN(idx: u16) Symbol {
    return .{ .is_term = false, .idx = idx };
}

pub const Limits = struct {
    state_cap: usize = 128,
    max_items_per_state: usize = 4096,
};

pub const NTermSpec = struct {
    name: []const u8,
};

pub const MatchFn = *const fn (input: []const u8, pos: usize) usize;

pub const TermKind = union(enum) {
    literal: []const u8,
    matcher: MatchFn,
    regex: []const u8,
};

pub const TermSpec = struct {
    name: []const u8,
    kind: TermKind,
    precedence: i32 = 0,
    assoc: Assoc = .no_assoc,
    skip: bool = false,
};

pub const SourcePoint = struct {
    index: usize = 0,
    line: usize = 1,
    column: usize = 1,
};

pub const TokenValue = struct {
    lexeme: []const u8,
    sp: SourcePoint,
};

pub const AstNode = struct {
    symbol: Symbol,
    rule_idx: ?u16 = null,
    token: ?TokenValue = null,
    first_child: usize = 0,
    child_count: usize = 0,
};

pub const Ast = struct {
    nodes: []AstNode,
    edges: []usize,
    root: usize,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Ast) void {
        self.allocator.free(self.edges);
        self.allocator.free(self.nodes);
        self.* = undefined;
    }

    pub fn rootNode(self: *const Ast) *const AstNode {
        return &self.nodes[self.root];
    }

    pub fn children(self: *const Ast, node_idx: usize) []const usize {
        const node = self.nodes[node_idx];
        return self.edges[node.first_child .. node.first_child + node.child_count];
    }
};

pub const Value = union(enum) {
    none,
    int: i64,
    token: TokenValue,
};

pub const ReducerFn = *const fn (ctx: ?*anyopaque, args: []const Value) anyerror!Value;

pub const RuleSpec = struct {
    lhs: u16,
    rhs: []const Symbol,
    reducer: ?ReducerFn = null,
    precedence_override: ?i32 = null,
    assoc_override: ?Assoc = null,
};

pub const Grammar = struct {
    root: u16,
    terms: []const TermSpec,
    nterms: []const NTermSpec,
    rules: []const RuleSpec,
    limits: Limits = .{},
};

pub const ParseOptions = struct {
    verbose: bool = false,
    skip_whitespace: bool = true,
    skip_newline: bool = true,
};

pub const ParseFailure = std.mem.Allocator.Error || error{
    LexError,
    ParseError,
    ReduceError,
    InvalidSemanticValue,
};

pub fn matchNumber(input: []const u8, pos: usize) usize {
    var i = pos;
    if (i >= input.len or !std.ascii.isDigit(input[i])) return 0;
    while (i < input.len and std.ascii.isDigit(input[i])) : (i += 1) {}
    return i - pos;
}

pub fn reduceIdentity(_: ?*anyopaque, args: []const Value) ParseFailure!Value {
    if (args.len == 0) return Value{ .none = {} };
    return args[0];
}

pub fn tokenToInt(v: Value) ParseFailure!i64 {
    return switch (v) {
        .token => |t| std.fmt.parseInt(i64, t.lexeme, 10) catch ParseFailure.InvalidSemanticValue,
        .int => |n| n,
        else => ParseFailure.InvalidSemanticValue,
    };
}
