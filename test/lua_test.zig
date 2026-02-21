const std = @import("std");
const citron = @import("citron");

const Grammar = citron.Grammar;
const RuleSpec = citron.RuleSpec;
const Ast = citron.Ast;
const TermSpec = citron.TermSpec;
const NTermSpec = citron.NTermSpec;
const Parser = citron.Parser;
const symT = citron.symT;
const symN = citron.symN;

const T = struct {
    const kw_if: u16 = 0;
    const kw_then: u16 = 1;
    const kw_else: u16 = 2;
    const kw_function: u16 = 3;
    const kw_local: u16 = 4;
    const kw_return: u16 = 5;
    const kw_end: u16 = 6;
    const name: u16 = 7;
    const number: u16 = 8;
    const ellipsis: u16 = 9;
    const eq: u16 = 10;
    const semi: u16 = 11;
    const comma: u16 = 12;
    const lparen: u16 = 13;
    const rparen: u16 = 14;
    const plus: u16 = 15;
    const star: u16 = 16;
    const lt: u16 = 17;
};

const NT = struct {
    const chunk: u16 = 0;
    const block: u16 = 1;
    const stat: u16 = 2;
    const laststat: u16 = 3;
    const funcbody: u16 = 4;
    const namelist: u16 = 5;
    const explist: u16 = 6;
    const exp: u16 = 7;
    const functioncall: u16 = 8;
    const stat_list: u16 = 9;
    const stat_semi_opt: u16 = 10;
    const laststat_opt: u16 = 11;
    const else_opt: u16 = 12;
    const explist_opt: u16 = 13;
    const namelist_tail: u16 = 14;
    const explist_tail: u16 = 15;
    const params_opt: u16 = 16;
    const parlist: u16 = 17;
    const vararg_opt: u16 = 18;
    const args_opt: u16 = 19;
    const exp_rel_opt: u16 = 20;
    const exp_add: u16 = 21;
    const exp_add_tail: u16 = 22;
    const exp_mul: u16 = 23;
    const exp_mul_tail: u16 = 24;
    const exp_atom: u16 = 25;
};

fn isNameStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

fn isNameContinue(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

fn matchName(input: []const u8, pos: usize) usize {
    var i = pos;
    if (i >= input.len or !isNameStart(input[i])) return 0;
    i += 1;
    while (i < input.len and isNameContinue(input[i])) : (i += 1) {}
    return i - pos;
}

fn matchNumber(input: []const u8, pos: usize) usize {
    var i = pos;
    if (i >= input.len or !std.ascii.isDigit(input[i])) return 0;
    i += 1;
    while (i < input.len and std.ascii.isDigit(input[i])) : (i += 1) {}
    return i - pos;
}

const lua_terms = [_]TermSpec{
    .{ .name = "if", .kind = .{ .literal = "if" } },
    .{ .name = "then", .kind = .{ .literal = "then" } },
    .{ .name = "else", .kind = .{ .literal = "else" } },
    .{ .name = "function", .kind = .{ .literal = "function" } },
    .{ .name = "local", .kind = .{ .literal = "local" } },
    .{ .name = "return", .kind = .{ .literal = "return" } },
    .{ .name = "end", .kind = .{ .literal = "end" } },
    .{ .name = "Name", .kind = .{ .matcher = matchName } },
    .{ .name = "Number", .kind = .{ .matcher = matchNumber } },
    .{ .name = "...", .kind = .{ .literal = "..." } },
    .{ .name = "=", .kind = .{ .literal = "=" } },
    .{ .name = ";", .kind = .{ .literal = ";" } },
    .{ .name = ",", .kind = .{ .literal = "," }, .precedence = 1, .assoc = .left },
    .{ .name = "(", .kind = .{ .literal = "(" } },
    .{ .name = ")", .kind = .{ .literal = ")" } },
    .{ .name = "+", .kind = .{ .literal = "+" } },
    .{ .name = "*", .kind = .{ .literal = "*" } },
    .{ .name = "<", .kind = .{ .literal = "<" } },
};

const lua_nterms = [_]NTermSpec{
    .{ .name = "chunk" },
    .{ .name = "block" },
    .{ .name = "stat" },
    .{ .name = "laststat" },
    .{ .name = "funcbody" },
    .{ .name = "namelist" },
    .{ .name = "explist" },
    .{ .name = "exp" },
    .{ .name = "functioncall" },
    .{ .name = "stat_list" },
    .{ .name = "stat_semi_opt" },
    .{ .name = "laststat_opt" },
    .{ .name = "else_opt" },
    .{ .name = "explist_opt" },
    .{ .name = "namelist_tail" },
    .{ .name = "explist_tail" },
    .{ .name = "params_opt" },
    .{ .name = "parlist" },
    .{ .name = "vararg_opt" },
    .{ .name = "args_opt" },
    .{ .name = "exp_rel_opt" },
    .{ .name = "exp_add" },
    .{ .name = "exp_add_tail" },
    .{ .name = "exp_mul" },
    .{ .name = "exp_mul_tail" },
    .{ .name = "exp_atom" },
};

const lua_rules = [_]RuleSpec{
    .{ .lhs = NT.chunk, .rhs = &.{ symN(NT.stat_list), symN(NT.laststat_opt) } },
    .{ .lhs = NT.block, .rhs = &.{symN(NT.chunk)} },

    .{ .lhs = NT.stat_list, .rhs = &.{ symN(NT.stat), symN(NT.stat_semi_opt), symN(NT.stat_list) } },
    .{ .lhs = NT.stat_list, .rhs = &.{} },
    .{ .lhs = NT.stat_semi_opt, .rhs = &.{symT(T.semi)} },
    .{ .lhs = NT.stat_semi_opt, .rhs = &.{} },

    .{ .lhs = NT.laststat_opt, .rhs = &.{ symN(NT.laststat), symN(NT.stat_semi_opt) } },
    .{ .lhs = NT.laststat_opt, .rhs = &.{} },
    .{ .lhs = NT.laststat, .rhs = &.{ symT(T.kw_return), symN(NT.explist_opt) } },
    .{ .lhs = NT.explist_opt, .rhs = &.{symN(NT.explist)} },
    .{ .lhs = NT.explist_opt, .rhs = &.{} },

    .{ .lhs = NT.stat, .rhs = &.{ symT(T.kw_local), symN(NT.namelist), symT(T.eq), symN(NT.explist) } },
    .{ .lhs = NT.stat, .rhs = &.{ symT(T.kw_function), symT(T.name), symN(NT.funcbody) } },
    .{ .lhs = NT.stat, .rhs = &.{ symT(T.kw_if), symN(NT.exp), symT(T.kw_then), symN(NT.block), symN(NT.else_opt), symT(T.kw_end) } },

    .{ .lhs = NT.else_opt, .rhs = &.{ symT(T.kw_else), symN(NT.block) } },
    .{ .lhs = NT.else_opt, .rhs = &.{} },

    .{ .lhs = NT.funcbody, .rhs = &.{ symT(T.lparen), symN(NT.params_opt), symT(T.rparen), symN(NT.block), symT(T.kw_end) } },
    .{ .lhs = NT.params_opt, .rhs = &.{symN(NT.parlist)} },
    .{ .lhs = NT.params_opt, .rhs = &.{} },
    .{ .lhs = NT.parlist, .rhs = &.{symT(T.name)} },
    .{ .lhs = NT.parlist, .rhs = &.{ symT(T.name), symT(T.comma), symN(NT.parlist) } },
    .{ .lhs = NT.parlist, .rhs = &.{symT(T.ellipsis)} },

    .{ .lhs = NT.namelist, .rhs = &.{ symT(T.name), symN(NT.namelist_tail) } },
    .{ .lhs = NT.namelist_tail, .rhs = &.{ symT(T.comma), symT(T.name), symN(NT.namelist_tail) } },
    .{ .lhs = NT.namelist_tail, .rhs = &.{} },

    .{ .lhs = NT.explist, .rhs = &.{ symN(NT.exp), symN(NT.explist_tail) } },
    .{ .lhs = NT.explist_tail, .rhs = &.{ symT(T.comma), symN(NT.exp), symN(NT.explist_tail) } },
    .{ .lhs = NT.explist_tail, .rhs = &.{} },

    .{ .lhs = NT.functioncall, .rhs = &.{ symT(T.name), symT(T.lparen), symN(NT.args_opt), symT(T.rparen) } },
    .{ .lhs = NT.args_opt, .rhs = &.{symN(NT.explist)} },
    .{ .lhs = NT.args_opt, .rhs = &.{} },

    .{ .lhs = NT.exp, .rhs = &.{ symN(NT.exp_add), symN(NT.exp_rel_opt) } },
    .{ .lhs = NT.exp_rel_opt, .rhs = &.{ symT(T.lt), symN(NT.exp_add) } },
    .{ .lhs = NT.exp_rel_opt, .rhs = &.{} },

    .{ .lhs = NT.exp_add, .rhs = &.{ symN(NT.exp_mul), symN(NT.exp_add_tail) } },
    .{ .lhs = NT.exp_add_tail, .rhs = &.{ symT(T.plus), symN(NT.exp_mul), symN(NT.exp_add_tail) } },
    .{ .lhs = NT.exp_add_tail, .rhs = &.{} },

    .{ .lhs = NT.exp_mul, .rhs = &.{ symN(NT.exp_atom), symN(NT.exp_mul_tail) } },
    .{ .lhs = NT.exp_mul_tail, .rhs = &.{ symT(T.star), symN(NT.exp_atom), symN(NT.exp_mul_tail) } },
    .{ .lhs = NT.exp_mul_tail, .rhs = &.{} },

    .{ .lhs = NT.exp_atom, .rhs = &.{symT(T.number)} },
    .{ .lhs = NT.exp_atom, .rhs = &.{symT(T.name)} },
    .{ .lhs = NT.exp_atom, .rhs = &.{symN(NT.functioncall)} },
    .{ .lhs = NT.exp_atom, .rhs = &.{ symT(T.lparen), symN(NT.exp), symT(T.rparen) } },
};

const lua_grammar = Grammar{
    .root = NT.chunk,
    .terms = &lua_terms,
    .nterms = &lua_nterms,
    .rules = &lua_rules,
    .limits = .{
        .state_cap = 4096,
        .max_items_per_state = 16384,
    },
};

fn printAstNode(ast: *const Ast, node_idx: usize, depth: usize) void {
    const node = ast.nodes[node_idx];
    var i: usize = 0;
    while (i < depth) : (i += 1) std.debug.print("  ", .{});

    if (node.symbol.is_term) {
        const term_name = lua_terms[node.symbol.idx].name;
        if (node.token) |tok| {
            std.debug.print("T({s}) \"{s}\"\n", .{ term_name, tok.lexeme });
        } else {
            std.debug.print("T({s})\n", .{term_name});
        }
        return;
    }

    const nterm_name = lua_nterms[node.symbol.idx].name;
    if (node.rule_idx) |rule_idx| {
        std.debug.print("N({s}) [rule {d}]\n", .{ nterm_name, rule_idx });
    } else {
        std.debug.print("N({s})\n", .{nterm_name});
    }

    for (ast.children(node_idx)) |child_idx| {
        printAstNode(ast, child_idx, depth + 1);
    }
}

fn printAst(ast: *const Ast) void {
    printAstNode(ast, ast.root, 0);
}

test "lua parser parses chunk and prints AST" {
    const P = Parser(lua_grammar);
    const allocator = std.testing.allocator;

    const input =
        \\local x = 1 + 2 * 3;
        \\function foo(a, b, ...)
        \\  if a < b then
        \\    return a
        \\  else
        \\    return b
        \\  end
        \\end
        \\return foo(x, 4, 5, 10)
    ;

    var ast = try P.parse(allocator, input, .{}, null);
    defer ast.deinit();

    printAst(&ast);
    try std.testing.expect(ast.nodes.len > 0);
}
