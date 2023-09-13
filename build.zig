const std = @import("std");

pub fn build(b: *std.Build) void {
    const nice = b.addModule("nice", .{
        .source_file = .{ .path = "src/config.zig" },
    });
}
