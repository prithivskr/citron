const std = @import("std");
const citron = @import("citron");

const ParseFailure = citron.ParseFailure;
const Grammar = citron.Grammar;
const RuleSpec = citron.RuleSpec;
const Symbol = citron.Symbol;
const Ast = citron.Ast;
const TermSpec = citron.TermSpec;
const NTermSpec = citron.NTermSpec;
const ParseOptions = citron.ParseOptions;
const Parser = citron.Parser;
const symT = citron.symT;
const symN = citron.symN;
const matchNumber = citron.matchNumber;

const arithmetic_terms = [_]TermSpec{
    .{ .name = "number", .kind = .{ .matcher = matchNumber } },
    .{ .name = "+", .kind = .{ .literal = "+" }, .precedence = 10, .assoc = .left },
    .{ .name = "*", .kind = .{ .literal = "*" }, .precedence = 20, .assoc = .left },
    .{ .name = "(", .kind = .{ .literal = "(" } },
    .{ .name = ")", .kind = .{ .literal = ")" } },
};

const arithmetic_regex_terms = [_]TermSpec{
    .{ .name = "number", .kind = .{ .regex = "[0-9]+" } },
    .{ .name = "+", .kind = .{ .regex = "\\+" }, .precedence = 10, .assoc = .left },
    .{ .name = "*", .kind = .{ .regex = "\\*" }, .precedence = 20, .assoc = .left },
    .{ .name = "(", .kind = .{ .regex = "\\(" } },
    .{ .name = ")", .kind = .{ .regex = "\\)" } },
    .{ .name = "ws", .kind = .{ .regex = "[ \\t\\r\\n]+" }, .skip = true },
};

const arithmetic_nterms = [_]NTermSpec{
    .{ .name = "expr" },
    .{ .name = "term" },
    .{ .name = "factor" },
};

const arithmetic_rhs_e_e_plus_t = [_]Symbol{ symN(0), symT(1), symN(1) };
const arithmetic_rhs_e_t = [_]Symbol{symN(1)};
const arithmetic_rhs_t_t_mul_f = [_]Symbol{ symN(1), symT(2), symN(2) };
const arithmetic_rhs_t_f = [_]Symbol{symN(2)};
const arithmetic_rhs_f_num = [_]Symbol{symT(0)};
const arithmetic_rhs_f_paren = [_]Symbol{ symT(3), symN(0), symT(4) };

const arithmetic_rules = [_]RuleSpec{
    .{ .lhs = 0, .rhs = &arithmetic_rhs_e_e_plus_t },
    .{ .lhs = 0, .rhs = &arithmetic_rhs_e_t },
    .{ .lhs = 1, .rhs = &arithmetic_rhs_t_t_mul_f },
    .{ .lhs = 1, .rhs = &arithmetic_rhs_t_f },
    .{ .lhs = 2, .rhs = &arithmetic_rhs_f_num },
    .{ .lhs = 2, .rhs = &arithmetic_rhs_f_paren },
};

const arithmetic_grammar = Grammar{
    .root = 0,
    .terms = &arithmetic_terms,
    .nterms = &arithmetic_nterms,
    .rules = &arithmetic_rules,
    .limits = .{
        .state_cap = 256,
        .max_items_per_state = 4096,
    },
};

const arithmetic_regex_grammar = Grammar{
    .root = 0,
    .terms = &arithmetic_regex_terms,
    .nterms = &arithmetic_nterms,
    .rules = &arithmetic_rules,
    .limits = .{
        .state_cap = 256,
        .max_items_per_state = 4096,
    },
};

fn evalAst(ast: *const Ast, node_idx: usize) ParseFailure!i64 {
    const node = ast.nodes[node_idx];
    if (node.symbol.is_term) {
        if (node.symbol.idx != 0 or node.token == null) return ParseFailure.InvalidSemanticValue;
        return std.fmt.parseInt(i64, node.token.?.lexeme, 10) catch ParseFailure.InvalidSemanticValue;
    }

    if (node.rule_idx == null) return ParseFailure.InvalidSemanticValue;
    const children = ast.children(node_idx);
    return switch (node.rule_idx.?) {
        0 => (try evalAst(ast, children[0])) + (try evalAst(ast, children[2])),
        1 => evalAst(ast, children[0]),
        2 => (try evalAst(ast, children[0])) * (try evalAst(ast, children[2])),
        3 => evalAst(ast, children[0]),
        4 => evalAst(ast, children[0]),
        5 => evalAst(ast, children[1]),
        else => ParseFailure.InvalidSemanticValue,
    };
}

test "arithmetic LALR(1) parser with generated lexer" {
    const P = Parser(arithmetic_grammar);
    const allocator = std.testing.allocator;

    var ast1 = try P.parse(allocator, "1 + 2 * 3", .{}, null);
    defer ast1.deinit();
    try std.testing.expectEqual(@as(i64, 7), try evalAst(&ast1, ast1.root));

    var ast2 = try P.parse(allocator, "(1 + 2) * 3", .{}, null);
    defer ast2.deinit();
    try std.testing.expectEqual(@as(i64, 9), try evalAst(&ast2, ast2.root));
}

test "arithmetic parser with regex term specs" {
    const P = Parser(arithmetic_regex_grammar);
    const allocator = std.testing.allocator;
    const opts = ParseOptions{
        .skip_whitespace = false,
        .skip_newline = false,
    };

    var ast1 = try P.parse(allocator, "1 + 2 * 3", opts, null);
    defer ast1.deinit();
    try std.testing.expectEqual(@as(i64, 7), try evalAst(&ast1, ast1.root));

    var ast2 = try P.parse(allocator, "(1 + 2)\n* 3", opts, null);
    defer ast2.deinit();
    try std.testing.expectEqual(@as(i64, 9), try evalAst(&ast2, ast2.root));
}
