const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const nice = b.addModule("nice", .{
        .root_source_file = .{ .path = "src/nice.zig" },
    });

    const tests = b.addTest(.{
        .name = "nice-unit-tests",
        .root_source_file = .{ .path = "tests/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    tests.root_module.addImport("nice", nice);

    const run_main_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&b.addInstallArtifact(tests, .{}).step);
    test_step.dependOn(&run_main_tests.step);

    add_examples(b, .{
        .target = target,
        .nice_mod = nice,
    });
}

const ExampleOptions = struct {
    target: std.Build.ResolvedTarget,
    nice_mod: *std.Build.Module,
};

const Example = struct {
    name: []const u8,
    file: []const u8,
};

const examples = [_]Example{
    .{ .name = "parse", .file = "examples/parse.zig" },
    .{ .name = "stream", .file = "examples/stream.zig" },
    .{ .name = "reify", .file = "examples/reify.zig" },
};

pub fn add_examples(b: *std.Build, options: ExampleOptions) void {
    const example_step = b.step("examples", "build examples");

    inline for (examples) |example| {
        const ex_exe = b.addExecutable(.{
            .name = example.name,
            .root_source_file = .{ .path = example.file },
            .target = options.target,
            .optimize = .Debug,
        });

        ex_exe.root_module.addImport("nice", options.nice_mod);
        const install = b.addInstallArtifact(ex_exe, .{});
        example_step.dependOn(&install.step);
    }
}
