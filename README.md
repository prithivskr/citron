# Citron

Compile-time LALR(1) parser generator library for Zig.

## Add As A Dependency

From your Zig project:

```sh
zig fetch --save git+https://github.com/prithivskr/citron
```

Then in your `build.zig`:

```zig
const dep = b.dependency("citron", .{
    .target = target,
    .optimize = optimize,
});

exe.root_module.addImport("citron", dep.module("citron"));
```

In code:

```zig
const citron = @import("citron");
```

## Quick Start

Define a grammar with terminals, nonterminals, and rules, then instantiate `Parser(grammar)`:

```zig
const std = @import("std");
const citron = @import("citron");

const Grammar = citron.Grammar;
const TermSpec = citron.TermSpec;
const NTermSpec = citron.NTermSpec;
const RuleSpec = citron.RuleSpec;
const Parser = citron.Parser;
const symT = citron.symT;
const symN = citron.symN;

const terms = [_]TermSpec{
    .{ .name = "number", .kind = .{ .regex = "[0-9]+" } },
    .{ .name = "+", .kind = .{ .literal = "+" }, .precedence = 10, .assoc = .left },
    .{ .name = "*", .kind = .{ .literal = "*" }, .precedence = 20, .assoc = .left },
    .{ .name = "ws", .kind = .{ .regex = "[ \\t\\r\\n]+" }, .skip = true },
};

const nterms = [_]NTermSpec{
    .{ .name = "expr" },
};

// expr -> expr + expr | expr * expr | number
const rules = [_]RuleSpec{
    .{ .lhs = 0, .rhs = &.{ symN(0), symT(1), symN(0) } },
    .{ .lhs = 0, .rhs = &.{ symN(0), symT(2), symN(0) } },
    .{ .lhs = 0, .rhs = &.{symT(0)} },
};

const g = Grammar{
    .root = 0,
    .terms = &terms,
    .nterms = &nterms,
    .rules = &rules,
    .limits = .{ .state_cap = 256, .max_items_per_state = 4096 },
};

pub fn main() !void {
    const P = Parser(g);
    var ast = try P.parse(std.heap.page_allocator, "1 + 2 * 3", .{}, null);
    defer ast.deinit();
    std.debug.print("nodes={d} root={d}\n", .{ ast.nodes.len, ast.root });
}
```

## Parsing API

- `const P = Parser(grammar);`
- `try P.parse(allocator, input, opts, ctx)` returns `citron.Ast`
- `P.writeDiagnostics(writer)` prints a state/item listing for the generated automaton

`opts` is `citron.ParseOptions`:
- `verbose`: prints shift/reduce trace to stderr
- `skip_whitespace` / `skip_newline`: controls built-in whitespace skipping

## AST

- `citron.Ast` stores `nodes` and a packed `edges` array
- Term nodes include `token.lexeme` and a `SourcePoint`
- Nonterm nodes include `rule_idx` and child pointers; use `ast.children(node_idx)` to traverse

## Notes

- Conflicts (unresolved shift/reduce or reduce/reduce) are compile-time errors; resolve with precedence/associativity on terminals
- If your grammar is large, bump `grammar.limits.state_cap` and `grammar.limits.max_items_per_state`

## Run Tests

```sh
zig build test
```
