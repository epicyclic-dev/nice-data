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

    const document: nice.Document = doc: {
        const file = try std.fs.cwd().openFile(args[1], .{});
        defer file.close();
        var parser = try nice.StreamParser.init(allocator, .{});
        defer parser.deinit();
        errdefer parser.parse_state.document.deinit();
        while (true) {
            var buf = [_]u8{0} ** 1024;
            const len = try file.read(&buf);
            if (len == 0) break;
            try parser.feed(buf[0..len]);
        }
        break :doc try parser.finish();
    };
    defer document.deinit();

    document.printDebug();
}
