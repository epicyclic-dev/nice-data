const std = @import("std");

pub const Value = union(enum) {
    pub const String = std.ArrayList(u8);
    pub const Map = std.StringArrayHashMap(Value);
    pub const List = std.ArrayList(Value);
    pub const TagType = @typeInfo(Value).Union.tag_type.?;

    scalar: String,
    string: String,
    list: List,
    flow_list: List,
    map: Map,
    flow_map: Map,

    pub inline fn fromScalar(alloc: std.mem.Allocator, input: []const u8) !Value {
        return try _fromScalarOrString(alloc, .scalar, input);
    }

    pub inline fn fromString(alloc: std.mem.Allocator, input: []const u8) !Value {
        return try _fromScalarOrString(alloc, .string, input);
    }

    inline fn _fromScalarOrString(alloc: std.mem.Allocator, comptime classification: TagType, input: []const u8) !Value {
        var res = @unionInit(Value, @tagName(classification), try String.initCapacity(alloc, input.len));
        @field(res, @tagName(classification)).appendSliceAssumeCapacity(input);
        return res;
    }

    pub inline fn newScalar(alloc: std.mem.Allocator) Value {
        return .{ .scalar = String.init(alloc) };
    }

    pub inline fn newString(alloc: std.mem.Allocator) Value {
        return .{ .string = String.init(alloc) };
    }

    pub inline fn newList(alloc: std.mem.Allocator) Value {
        return .{ .list = List.init(alloc) };
    }

    pub inline fn newFlowList(alloc: std.mem.Allocator) Value {
        return .{ .flow_list = List.init(alloc) };
    }

    pub inline fn newMap(alloc: std.mem.Allocator) Value {
        return .{ .map = Map.init(alloc) };
    }

    pub inline fn newFlowMap(alloc: std.mem.Allocator) Value {
        return .{ .flow_map = Map.init(alloc) };
    }

    pub fn recursiveEqualsExact(self: Value, other: Value) bool {
        if (@as(TagType, self) != other) return false;
        switch (self) {
            inline .scalar, .string => |str, tag| return std.mem.eql(u8, str.items, @field(other, @tagName(tag)).items),
            inline .list, .flow_list => |lst, tag| {
                const olst = @field(other, @tagName(tag));

                if (lst.items.len != olst.items.len) return false;
                for (lst.items, olst.items) |this, that| if (!this.recursiveEqualsExact(that)) return false;
                return true;
            },
            inline .map, .flow_map => |map, tag| {
                const omap = @field(other, @tagName(tag));

                if (map.count() != omap.count()) return false;
                var iter = map.iterator();
                var oiter = omap.iterator();
                // this loop structure enforces that the maps are in the same order
                while (iter.next()) |this| {
                    const that = oiter.next() orelse return false;
                    if (!std.mem.eql(u8, this.key_ptr.*, that.key_ptr.*) or !this.value_ptr.recursiveEqualsExact(that.value_ptr.*)) return false;
                }
                // the maps are equal if we have also consumed all of the values from
                // other.
                return oiter.next() == null;
            },
        }
    }

    pub fn printDebug(self: Value) void {
        self.printRecursive(0);
        std.debug.print("\n", .{});
    }

    fn printRecursive(self: Value, indent: usize) void {
        switch (self) {
            .scalar, .string => |str| {
                if (std.mem.indexOfScalar(u8, str.items, '\n')) |_| {
                    var lines = std.mem.splitScalar(u8, str.items, '\n');
                    std.debug.print("\n", .{});
                    while (lines.next()) |line| {
                        std.debug.print(
                            "{[empty]s: >[indent]}{[line]s}{[nl]s}",
                            .{
                                .empty = "",
                                .indent = indent,
                                .line = line,
                                .nl = if (lines.peek() == null) "" else "\n",
                            },
                        );
                    }
                } else {
                    std.debug.print("{s}", .{str.items});
                }
            },
            .list, .flow_list => |list| {
                if (list.items.len == 0) {
                    std.debug.print("[]", .{});
                    return;
                }

                std.debug.print("[\n", .{});
                for (list.items, 0..) |value, idx| {
                    std.debug.print("{[empty]s: >[indent]}[{[idx]d}] = ", .{ .empty = "", .indent = indent, .idx = idx });
                    value.printRecursive(indent + 2);
                    std.debug.print(",\n", .{});
                }
                std.debug.print(
                    "{[empty]s: >[indent]}]",
                    .{ .empty = "", .indent = indent },
                );
            },
            .map, .flow_map => |map| {
                if (map.count() == 0) {
                    std.debug.print("{{}}", .{});
                    return;
                }

                std.debug.print("{{\n", .{});

                var iter = map.iterator();

                while (iter.next()) |entry| {
                    std.debug.print(
                        "{[empty]s: >[indent]}{[key]s}: ",
                        .{ .empty = "", .indent = indent + 2, .key = entry.key_ptr.* },
                    );
                    entry.value_ptr.printRecursive(indent + 4);
                    std.debug.print(",\n", .{});
                }
                std.debug.print(
                    "{[empty]s: >[indent]}}}",
                    .{ .empty = "", .indent = indent },
                );
            },
        }
    }
};
