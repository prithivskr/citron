const std = @import("std");
const types = @import("citron/types.zig");
const lalr1 = @import("citron/lalr1.zig");
const runtime = @import("citron/runtime.zig");

pub const Assoc = types.Assoc;
pub const Symbol = types.Symbol;
pub const Limits = types.Limits;
pub const NTermSpec = types.NTermSpec;
pub const MatchFn = types.MatchFn;
pub const TermKind = types.TermKind;
pub const TermSpec = types.TermSpec;
pub const SourcePoint = types.SourcePoint;
pub const TokenValue = types.TokenValue;
pub const AstNode = types.AstNode;
pub const Ast = types.Ast;
pub const Value = types.Value;
pub const ReducerFn = types.ReducerFn;
pub const RuleSpec = types.RuleSpec;
pub const Grammar = types.Grammar;
pub const ParseOptions = types.ParseOptions;
pub const ParseFailure = types.ParseFailure;

pub const symT = types.symT;
pub const symN = types.symN;
pub const matchNumber = types.matchNumber;
pub const reduceIdentity = types.reduceIdentity;
pub const tokenToInt = types.tokenToInt;

pub fn Parser(comptime g: Grammar) type {
    const Builder = lalr1.Builder(g);
    const RuntimeImpl = runtime.Runtime(g, Builder);

    return struct {
        pub const EOF_TERM: u16 = Builder.EOF_IDX;
        pub const ERR_TERM: u16 = Builder.ERR_IDX;

        pub fn parse(allocator: std.mem.Allocator, input: []const u8, opts: ParseOptions, ctx: ?*anyopaque) ParseFailure!Ast {
            return RuntimeImpl.parse(allocator, input, opts, ctx);
        }

        pub fn writeDiagnostics(writer: anytype) !void {
            try RuntimeImpl.writeDiagnostics(writer);
        }
    };
}
