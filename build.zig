const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const nice = b.addModule("nice", .{
        .source_file = .{ .path = "src/nice.zig" },
    });

    add_examples(b, .{
        .target = target,
        .nice_mod = nice,
    });
}

const ExampleOptions = struct {
    target: std.zig.CrossTarget,
    nice_mod: *std.Build.Module,
};

const Example = struct {
    name: []const u8,
    file: []const u8,
};

const examples = [_]Example{
    .{ .name = "parse", .file = "examples/parse.zig" },
    .{ .name = "stream", .file = "examples/stream.zig" },
};

pub fn add_examples(b: *std.build, options: ExampleOptions) void {
    const example_step = b.step("examples", "build examples");

    inline for (examples) |example| {
        const ex_exe = b.addExecutable(.{
            .name = example.name,
            .root_source_file = .{ .path = example.file },
            .target = options.target,
            .optimize = .Debug,
        });

        ex_exe.addModule("nice", options.nice_mod);
        const install = b.addInstallArtifact(ex_exe, .{});
        example_step.dependOn(&install.step);
    }
}
