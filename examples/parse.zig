// This example is dedicated to the public domain or, where that is not possible,
// licensed under CC0-1.0, available at https://spdx.org/licenses/CC0-1.0.html

const std = @import("std");

const nice = @import("nice");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) return;

    const data = try std.fs.cwd().readFileAlloc(allocator, args[1], 4_294_967_295);
    var needfree = true;
    defer if (needfree) allocator.free(data);

    var diagnostics = nice.Diagnostics{};
    const document = nice.parseBuffer(allocator, data, &diagnostics, .{}) catch |err| {
        std.debug.print("{s}:{d} col:{d}: {s}\n", .{
            args[1],
            diagnostics.row,
            diagnostics.line_offset,
            diagnostics.message,
        });
        return err;
    };
    defer document.deinit();

    // free data memory to ensure that the parsed document is not holding
    // references to it.
    allocator.free(data);
    needfree = false;

    document.printDebug();
}
