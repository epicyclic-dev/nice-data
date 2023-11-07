// This example is dedicated to the public domain or, where that is not possible,
// licensed under CC0-1.0, available at https://spdx.org/licenses/CC0-1.0.html

const std = @import("std");

const nice = @import("nice");

const Enum = enum { first, second, third };
const TagUnion = union(Enum) { first: []const u8, second: i32, third: void };

const Example = struct {
    useful: bool,
    number: i32,
    string: []const u8,
    longstring: [:0]const u8,
    tuple: struct { bool, i8 },
    enume: Enum,
    taggart: TagUnion,
    voidtag: TagUnion,
    exist: ?bool,
    again: ?bool,
    array: [5]i16,
    nested: [3]struct { index: usize, title: []const u8 },
};

const source =
    \\useful: true
    \\number: 0x9001
    \\string: > salutations, earthen oblate spheroid
    \\
    \\longstring:
    \\	| If, at first, you don't think this string has
    \\	+ multiple lines, then perhaps you are the one who is
    \\	# yeah, let's add a newline here
    \\	> wrong.
    \\	# and a trailing newline for good measure
    \\	>
    \\tuple: [ no, 127 ]
    \\enume: .second
    \\taggart: {.first: string a thing}
    \\voidtag: .third
    \\list:
    \\	- I am a list item
    \\exist: null
    \\again: true
    \\array: [ 1, 2, 3, 4, 5 ]
    \\nested:
    \\	- { index: 1, title: none }
    \\	- { index: 2, title: such }
    \\	- { index: 3, title: luck }
    \\
;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var diagnostics = nice.Diagnostics{};
    var loaded = nice.parseBufferTo(Example, allocator, source, &diagnostics, .{}) catch |err| {
        std.debug.print("row:{d} col:{d}: {s}\n", .{
            diagnostics.row,
            diagnostics.line_offset,
            diagnostics.message,
        });
        return err;
    };
    defer loaded.deinit();

    std.debug.print("{s} {{\n", .{@typeName(Example)});
    std.debug.print("    useful: {}\n", .{loaded.value.useful});
    std.debug.print("    number: {d}\n", .{loaded.value.number});
    std.debug.print("    string: {s}\n", .{loaded.value.string});
    std.debug.print("    longstring: {s}\n", .{loaded.value.longstring});
    std.debug.print("    tuple: {{ {}, {d} }}\n", .{ loaded.value.tuple[0], loaded.value.tuple[1] });
    std.debug.print("    enume: .{s}\n", .{@tagName(loaded.value.enume)});
    std.debug.print("    taggart: ", .{});
    switch (loaded.value.taggart) {
        .first => |val| std.debug.print(".first = {s}\n", .{val}),
        .second => |val| std.debug.print(".second = {d}\n", .{val}),
        .third => std.debug.print(".third\n", .{}),
    }
    std.debug.print("    voidtag: ", .{});
    switch (loaded.value.voidtag) {
        .first => |val| std.debug.print(".first = {s}\n", .{val}),
        .second => |val| std.debug.print(".second = {d}\n", .{val}),
        .third => std.debug.print(".third\n", .{}),
    }
    std.debug.print("    exist: {?}\n", .{loaded.value.exist});
    std.debug.print("    again: {?}\n", .{loaded.value.again});
    std.debug.print("    array: [ ", .{});
    for (loaded.value.array) |item| {
        std.debug.print("{d}, ", .{item});
    }
    std.debug.print("]\n", .{});
    std.debug.print("    nested: [\n", .{});
    for (loaded.value.nested) |item| {
        std.debug.print("        {{ index: {d}, title: {s} }}\n", .{ item.index, item.title });
    }
    std.debug.print("    ]\n", .{});
    std.debug.print("}}\n", .{});
}
