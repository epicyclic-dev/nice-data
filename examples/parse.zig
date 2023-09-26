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

    const document = try nice.parseBuffer(allocator, data, .{});
    defer document.deinit();

    // free data memory to ensure that the parsed document is not holding
    // references to it.
    allocator.free(data);
    needfree = false;

    document.printDebug();
}
