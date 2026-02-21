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

## Run Tests

```sh
zig build test
```
