// Copyright 2023 torque@epicyclic.dev
//
// Licensed under the MIT/Expat license. You may not use this file except in
// compliance with the license. You may obtain a copy of the license at
//
//    https://spdx.org/licenses/MIT.html
//
// This software is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied.

const std = @import("std");

const Options = @import("../parser.zig").Options;

pub const Document = struct {
    arena: std.heap.ArenaAllocator,
    root: Value,

    pub fn init(alloc: std.mem.Allocator) Document {
        return .{
            .arena = std.heap.ArenaAllocator.init(alloc),
            .root = undefined,
        };
    }

    pub fn convertTo(self: *Document, comptime T: type, options: Options) !Parsed(T) {
        return .{
            .value = try self.root.convertTo(T, self.arena.allocator(), options),
            .arena = self.arena,
        };
    }

    pub fn printDebug(self: Document) void {
        return self.root.printDebug();
    }

    pub fn deinit(self: Document) void {
        self.arena.deinit();
    }
};

pub fn Parsed(comptime T: type) type {
    return struct {
        value: T,
        arena: std.heap.ArenaAllocator,

        pub fn deinit(self: @This()) void {
            self.arena.deinit();
        }
    };
}

pub const Value = union(enum) {
    pub const String = []const u8;
    pub const Map = std.StringArrayHashMap(Value);
    pub const List = std.ArrayList(Value);
    pub const TagType = @typeInfo(Value).Union.tag_type.?;

    scalar: String,
    string: String,
    list: List,
    inline_list: List,
    map: Map,
    inline_map: Map,

    pub fn convertTo(self: Value, comptime T: type, allocator: std.mem.Allocator, options: Options) !T {
        switch (@typeInfo(T)) {
            .Void => {
                switch (self) {
                    .scalar => |str| return if (str.len == 0) void{} else error.BadValue,
                    .string => |str| return if (options.coerce_strings and str.len == 0) void{} else error.BadValue,
                    else => return error.BadValue,
                }
            },
            .Bool => {
                switch (self) {
                    inline .scalar, .string => |str, tag| {
                        if (tag == .string and !options.coerce_strings) return error.BadValue;
                        if (options.case_insensitive_scalar_coersion) {
                            for (options.boolean_scalars.truthy) |check|
                                if (std.ascii.eqlIgnoreCase(str, check)) return true;
                            for (options.boolean_scalars.falsy) |check|
                                if (std.ascii.eqlIgnoreCase(str, check)) return false;
                        } else {
                            for (options.boolean_scalars.truthy) |check|
                                if (std.mem.eql(u8, str, check)) return true;
                            for (options.boolean_scalars.falsy) |check|
                                if (std.mem.eql(u8, str, check)) return false;
                        }

                        return error.BadValue;
                    },
                    else => return error.BadValue,
                }
            },
            .Int, .ComptimeInt => {
                switch (self) {
                    inline .scalar, .string => |str, tag| {
                        if (tag == .string and !options.coerce_strings) return error.BadValue;
                        return try std.fmt.parseInt(T, str, 0);
                    },
                    else => return error.BadValue,
                }
            },
            .Float, .ComptimeFloat => {
                switch (self) {
                    inline .scalar, .string => |str, tag| {
                        if (tag == .string and !options.coerce_strings) return error.BadValue;
                        return try std.fmt.parseFloat(T, str, 0);
                    },
                    else => return error.BadValue,
                }
            },
            .Pointer => |ptr| switch (ptr.size) {
                .Slice => {
                    // TODO: There is ambiguity here because a document expecting a list
                    //       of u8 could parse a string instead. Introduce a special
                    //       type to use for this? the problem is that it becomes
                    //       invasive into downstream code. Ultimately this should
                    //       probably be solved in the zig stdlib or similar.
                    switch (self) {
                        .scalar, .string => |str| {
                            if (ptr.child == u8) {
                                if (ptr.sentinel) |sent| {
                                    var copy = try allocator.allocSentinel(u8, str.len, @as(*const u8, @ptrCast(sent)).*);
                                    @memcpy(copy, str);
                                    return copy;
                                }
                                return str;
                            } else {
                                return error.BadValue;
                            }
                        },
                        .list, .inline_list => |lst| {
                            var result = try std.ArrayList(ptr.child).initCapacity(allocator, lst.items.len);
                            errdefer result.deinit();
                            for (lst.items) |item| {
                                result.appendAssumeCapacity(try item.convertTo(ptr.child, allocator, options));
                            }
                            if (ptr.sentinel) |sent| {
                                return try result.toOwnedSliceSentinel(@as(*align(1) const ptr.child, @ptrCast(sent)).*);
                            } else {
                                return try result.toOwnedSlice();
                            }
                        },
                        else => return error.BadValue,
                    }
                },
                .One => {
                    const result = try allocator.create(ptr.child);
                    errdefer allocator.destroy(result);
                    result.* = try self.convertTo(ptr.child, allocator, options);
                    return result;
                },
                else => @compileError("Cannot deserialize into many-pointer or c-pointer " ++ @typeName(T)), // do not support many or C item pointers.
            },
            .Array => |arr| {
                // TODO: There is ambiguity here because a document expecting a list
                //       of u8 could parse a string instead. Introduce a special
                //       type to use for this? the problem is that it becomes
                //       invasive into downstream code. Ultimately this should
                //       probably be solved in the zig stdlib or similar.
                switch (self) {
                    .scalar, .string => |str| {
                        if (arr.child == u8 and str.len == arr.len) {
                            var result: T = undefined;
                            @memcpy(&result, str);
                            return result;
                        } else return error.BadValue;
                    },
                    .list, .inline_list => |lst| {
                        var storage = try std.ArrayList(arr.child).initCapacity(allocator, arr.len);
                        defer storage.deinit();
                        for (lst.items) |item| {
                            storage.appendAssumeCapacity(try item.convertTo(arr.child, allocator, options));
                        }
                        // this may result in a big stack allocation, which is not ideal
                        var result: T = undefined;
                        @memcpy(&result, storage.items);
                        return result;
                    },
                    else => return error.BadValue,
                }
            },
            .Struct => |stt| {
                if (comptime std.meta.trait.hasFn("deserializeNice")(T))
                    return T.deserializeNice(self, allocator, options);

                if (stt.is_tuple) {
                    switch (self) {
                        .list, .inline_list => |list| {
                            if (list.items.len != stt.fields.len) return error.BadValue;
                            var result: T = undefined;
                            inline for (stt.fields, 0..) |field, idx| {
                                result[idx] = try list.items[idx].convertTo(field.type, allocator, options);
                            }
                            return result;
                        },
                        else => return error.BadValue,
                    }
                }

                switch (self) {
                    .map, .inline_map => |map| {
                        var result: T = undefined;

                        if (options.ignore_extra_fields) {
                            inline for (stt.fields) |field| {
                                if (map.get(field.name)) |value| {
                                    @field(result, field.name) = try value.convertTo(field.type, allocator, options);
                                } else if (options.treat_omitted_as_null and @typeInfo(field.type) == .Optional) {
                                    @field(result, field.name) = null;
                                } else {
                                    return error.BadValue;
                                }
                            }
                        } else {
                            // we could iterate over each map key and do an exhaustive
                            // comparison with each struct field name. This would save
                            // memory and it would probably be a fair amount faster for
                            // small structs.
                            var clone = try map.clone();
                            defer clone.deinit();
                            inline for (stt.fields) |field| {
                                if (clone.fetchSwapRemove(field.name)) |kv| {
                                    @field(result, field.name) = try kv.value.convertTo(field.type, allocator, options);
                                } else if (options.treat_omitted_as_null and @typeInfo(field.type) == .Optional) {
                                    @field(result, field.name) = null;
                                } else return error.BadValue;
                            }
                            // there were extra fields in the data
                            if (clone.count() > 0) return error.BadValue;
                        }

                        return result;
                    },
                    else => return error.BadValue,
                }
            },
            .Enum => {
                if (comptime std.meta.trait.hasFn("deserializeNice")(T))
                    return T.deserializeNice(self, allocator, options);

                switch (self) {
                    inline .scalar, .string => |str, tag| {
                        if (tag == .string and !options.coerce_strings) return error.BadValue;
                        const name = if (options.expect_enum_dot) blk: {
                            if (str.len > 0 and str[0] == '.')
                                break :blk str[1..]
                            else
                                return error.BadValue;
                        } else str;

                        if (std.meta.stringToEnum(T, name)) |value| return value;
                        if (options.allow_numeric_enums) {
                            const parsed = std.fmt.parseInt(@typeInfo(T).Enum.tag_type, str, 10) catch
                                return error.BadValue;
                            return std.meta.intToEnum(T, parsed) catch error.BadValue;
                        }
                        return error.BadValue;
                    },
                    else => return error.BadValue,
                }
            },
            .Union => |unn| {
                if (comptime std.meta.trait.hasFn("deserializeNice")(T))
                    return T.deserializeNice(self, allocator, options);

                if (unn.tag_type == null) @compileError("Cannot deserialize into untagged union " ++ @typeName(T));

                switch (self) {
                    .map, .inline_map => |map| {
                        // a union may not ever be deserialized from a map with more
                        // (or less) than one value
                        if (map.count() != 1) return error.BadValue;
                        const key = map.keys()[0];
                        const name = if (options.expect_enum_dot) blk: {
                            if (key.len > 0 and key[0] == '.')
                                break :blk key[1..]
                            else
                                return error.BadValue;
                        } else key;

                        inline for (unn.fields) |field| {
                            if (std.mem.eql(u8, name, field.name))
                                return @unionInit(T, field.name, try map.get(key).?.convertTo(field.type, allocator, options));
                        }
                        return error.BadValue;
                    },
                    inline .scalar, .string => |str, tag| {
                        if (tag == .string and !options.coerce_strings) return error.BadValue;
                        const name = if (options.expect_enum_dot) blk: {
                            if (str.len > 0 and str[0] == '.')
                                break :blk str[1..]
                            else
                                return error.BadValue;
                        } else str;

                        inline for (unn.fields) |field| {
                            if (@sizeOf(field.type) != 0) continue;
                            // this logic may be a little off: comtime_int,
                            // comptime_float, and type will all have size 0 because
                            // they can't be used at runtime. On the other hand, trying
                            // to use them here should result in a compile error? Also,
                            // it's a 0 sized type so initializing it as undefined
                            // shouldn't be a problem. As far as I know.
                            if (std.mem.eql(u8, name, field.name))
                                return @unionInit(T, field.name, undefined);
                        }
                        return error.BadValue;
                    },
                    else => return error.BadValue,
                }
            },
            .Optional => |opt| {
                switch (self) {
                    inline .scalar, .string => |str, tag| {
                        if (tag == .string and !options.coerce_strings) return error.BadValue;
                        if (options.case_insensitive_scalar_coersion) {
                            for (options.null_scalars) |check|
                                if (std.ascii.eqlIgnoreCase(str, check)) return null;
                        } else {
                            for (options.null_scalars) |check|
                                if (std.mem.eql(u8, str, check)) return null;
                        }

                        return try self.convertTo(opt.child, allocator, options);
                    },
                    else => return error.BadValue,
                }
            },
            else => @compileError("Cannot deserialize into unsupported type " ++ @typeName(T)),
        }
    }

    pub inline fn fromScalar(alloc: std.mem.Allocator, input: []const u8) !Value {
        return try _fromScalarOrString(alloc, .scalar, input);
    }

    pub inline fn fromString(alloc: std.mem.Allocator, input: []const u8) !Value {
        return try _fromScalarOrString(alloc, .string, input);
    }

    inline fn _fromScalarOrString(alloc: std.mem.Allocator, comptime classification: TagType, input: []const u8) !Value {
        return @unionInit(Value, @tagName(classification), try alloc.dupe(u8, input));
    }

    pub inline fn emptyScalar() Value {
        return .{ .scalar = "" };
    }

    pub inline fn emptyString() Value {
        return .{ .string = "" };
    }

    pub inline fn newList(alloc: std.mem.Allocator) Value {
        return .{ .list = List.init(alloc) };
    }

    pub inline fn newFlowList(alloc: std.mem.Allocator) Value {
        return .{ .inline_list = List.init(alloc) };
    }

    pub inline fn newMap(alloc: std.mem.Allocator) Value {
        return .{ .map = Map.init(alloc) };
    }

    pub inline fn newFlowMap(alloc: std.mem.Allocator) Value {
        return .{ .inline_map = Map.init(alloc) };
    }

    pub fn recursiveEqualsExact(self: Value, other: Value) bool {
        if (@as(TagType, self) != other) return false;
        switch (self) {
            inline .scalar, .string => |str, tag| return std.mem.eql(u8, str, @field(other, @tagName(tag))),
            inline .list, .inline_list => |lst, tag| {
                const olst = @field(other, @tagName(tag));

                if (lst.items.len != olst.items.len) return false;
                for (lst.items, olst.items) |this, that| if (!this.recursiveEqualsExact(that)) return false;
                return true;
            },
            inline .map, .inline_map => |map, tag| {
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
                if (std.mem.indexOfScalar(u8, str, '\n')) |_| {
                    var lines = std.mem.splitScalar(u8, str, '\n');
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
                    std.debug.print("{s}", .{str});
                }
            },
            .list, .inline_list => |list| {
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
            .map, .inline_map => |map| {
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
