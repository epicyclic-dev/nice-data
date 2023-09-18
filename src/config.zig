// Heavily inspired by, but not quite compatible with, NestedText. Key differences:
//
// - Doesn't support multiline keys (this means map keys cannot start with
//   ' ', \t, #, {, [, |, or >, and they cannot contain :)
// - Allows using tabs for indentation (but not mixed tabs/spaces)
// - Indentation must be quantized consistently throughout the document. e.g.
//   every nested layer being exactly 2 spaces past its parent. Tabs may
//   only use one tab per indentation level.
// - Allows flow-style lists, maps, and strings on the same line as map keys or
//   list items (i.e. the following are legal):
//
//      key: {inline: map}
//      key: [inline, list]
//      key: > inline string
//      - {map: item}
//      - [list, item]
//      - > inline string
//
//   The string case retains the possibility of having an inline map value starting
//   with {, [, or >
// - inline lists and maps cannot contain other inline structures. This may
//   change, as writing {:[{:[{:[{:[{:[{:[]}]}]}]}]}]} seems tremendously useful
// - a map keys and list item dashes must be followed by a value or an indented
//   section to reduce parser quantum state. This means that
//
//      foo:
//      bar: baz
//
//   or
//
//      -
//      - qux
//
//   are not valid. This can be represented with an inline empty string after foo:
//
//      foo: >
//      bar: baz
//
//   or
//
//      - >
//      - qux
//
// - newlines are strictly LF, if the parser finds CR, it is an error
// - blank lines may not contain any whitespace characters except the single LF
// - Additional string indicator `|` for soft-wrapped strings, i.e.
//
//      key: | this is not special
//      key:
//        | these lines are
//        | soft-wrapped
//
//   soft-wrapped lines are joined with a ' ' instead of a newline character.
//   Like multiline strings, the final space is stripped (I guess this is a very
//   janky way to add trailing whitespace to a string).
//
// - The parser is both strict and probably sloppy and may have weird edge
//   cases since I'm slinging code, not writing a spec. For example, tabs are
//   not trimmed from the values of inline lists/maps

const std = @import("std");

pub const Diagnostics = struct {
    row: usize,
    span: struct { absolute: usize, line_offset: usize, length: usize },
    message: []const u8,
};

pub const LineTokenizer = struct {
    buffer: []const u8,
    index: usize = 0,
    indentation: IndentationType = .immaterial,
    last_indent: usize = 0,
    diagnostics: *Diagnostics,

    row: usize = 0,

    const Error = error{
        BadToken,
        MixedIndentation,
        UnquantizedIndentation,
        TooMuchIndentation,
        MissingNewline,
        TrailingWhitespace,
        Impossible,
    };

    const IndentationType = union(enum) {
        immaterial: void,
        spaces: usize,
        tabs: void,
    };

    const InlineItem = union(enum) {
        empty: void,
        scalar: []const u8,
        string: []const u8,

        flow_list: []const u8,
        flow_map: []const u8,
    };

    const LineContents = union(enum) {
        comment: []const u8,

        in_line: InlineItem,
        list_item: InlineItem,
        map_item: struct { key: []const u8, val: InlineItem },
    };

    // we can dedent multiple levels at once. Example:
    //
    // foo:
    //   bar:
    //     > a
    //     > string
    // baz: [qux]
    //
    // capturing this is conceptually simple, but implementing it without complex
    // indentation tracking requires quantizing the indentation. This means our
    // IndentationType will also need to track the number of spaces used for
    // indentation, as detected. Then every line we have to check indent rem the
    // quantization level == 0 (otherwise we broke quantization) and compute indent
    // div the quantization level to give us our effective indentation level.

    const ShiftDirection = enum { indent, dedent, none };
    const RelativeIndent = union(ShiftDirection) {
        indent: void,
        dedent: usize,
        none: void,
    };

    const Line = struct {
        indent: RelativeIndent,
        contents: LineContents,
        raw: []const u8,
    };

    pub fn next(self: *LineTokenizer) Error!?Line {
        if (self.index == self.buffer.len) return null;

        var indent: usize = 0;
        var offset: usize = 0;

        for (self.buffer[self.index..], 0..) |char, idx| {
            switch (char) {
                ' ' => {
                    switch (self.indentation) {
                        // There's a weird coupling here because we can't set this until
                        // all spaces have been consumed. I also thought about ignoring
                        // spaces on comment lines since those don't affect the
                        // relative indent/dedent, but then we would allow comments
                        // to ignore our indent quantum, which I dislike due to it making
                        // ugly documents.
                        .immaterial => self.indentation = .{ .spaces = 0 },
                        .spaces => {},
                        .tabs => return error.MixedIndentation,
                    }
                    indent += 1;
                },
                '\t' => {
                    switch (self.indentation) {
                        .immaterial => self.indentation = .tabs,
                        .spaces => return error.MixedIndentation,
                        .tabs => {},
                    }
                    indent += 1;
                },
                '\r' => {
                    return error.BadToken;
                },
                '\n' => {
                    // don't even emit anything for empty rows.
                    self.row += 1;
                    offset = idx + 1;
                    // if it's too hard to deal with, Just Make It An Error!!!
                    // an empty line with whitespace on it is garbage. It can mess with
                    // the indentation detection grossly in a way that is annoying to
                    // deal with. Besides, having whitespace-only lines in a document
                    // is essentially terrorism, with which negotiations are famously
                    // not permitted.
                    if (indent > 0) return error.TrailingWhitespace;
                },
                else => break,
            }
        } else {
            std.debug.assert(self.buffer.len == self.index + indent + offset + 1);
            self.index = self.buffer.len;
            // this prong will get hit when the document only consists of whitespace
            return null;
        }

        var quantized: usize = if (self.indentation == .spaces) blk: {
            if (self.indentation.spaces == 0) {
                self.indentation.spaces = indent;
            }
            if (@rem(indent, self.indentation.spaces) != 0)
                return error.UnquantizedIndentation;

            break :blk @divExact(indent, self.indentation.spaces);
        } else indent;

        const relative: RelativeIndent = if (quantized > self.last_indent) rel: {
            if ((quantized - self.last_indent) > 1)
                return error.TooMuchIndentation;
            break :rel .indent;
        } else if (quantized < self.last_indent)
            .{ .dedent = self.last_indent - quantized }
        else
            .none;

        offset += indent;

        defer {
            self.row += 1;
            self.last_indent = quantized;
            self.index += offset;
        }

        const line = try consumeLine(self.buffer[self.index + offset ..]);
        offset += line.len + 1;

        // this should not be possible, as empty lines are caught earlier.
        if (line.len == 0) return error.Impossible;

        switch (line[0]) {
            '#' => {
                // simply lie about indentation when the line is a comment.
                quantized = self.last_indent;
                return .{
                    .indent = .none,
                    .contents = .{ .comment = line[1..] },
                    .raw = line,
                };
            },
            '|', '>', '[', '{' => {
                return .{
                    .indent = relative,
                    .contents = .{ .in_line = try detectInlineItem(line) },
                    .raw = line,
                };
            },
            '-' => {
                if (line.len > 1 and line[1] != ' ') return error.BadToken;

                return if (line.len == 1) .{
                    .indent = relative,
                    .contents = .{ .list_item = .empty },
                    .raw = line,
                } else .{
                    .indent = relative,
                    .contents = .{ .list_item = try detectInlineItem(line[2..]) },
                    .raw = line,
                };
            },
            else => {
                for (line, 0..) |char, idx| {
                    if (char == ':') {
                        if (idx + 1 == line.len) return .{
                            .indent = relative,
                            .contents = .{ .map_item = .{ .key = line[0..idx], .val = .empty } },
                            .raw = line,
                        };

                        if (line[idx + 1] != ' ') return error.BadToken;

                        return .{
                            .indent = relative,
                            .contents = .{ .map_item = .{
                                .key = line[0..idx],
                                .val = try detectInlineItem(line[idx + 2 ..]),
                            } },
                            .raw = line,
                        };
                    }
                }

                return .{
                    .indent = relative,
                    .contents = .{ .in_line = .{ .scalar = line } },
                    .raw = line,
                };
            },
        }
    }

    fn detectInlineItem(buf: []const u8) Error!InlineItem {
        if (buf.len == 0) return .empty;

        switch (buf[0]) {
            '|', '>' => |char| {
                if (buf.len > 1 and buf[1] != ' ') return error.BadToken;

                return .{
                    .string = buf.ptr[@min(2, buf.len) .. buf.len + @intFromBool(char == '|')],
                };
            },
            '[' => {
                if (buf.len < 2 or buf[buf.len - 1] != ']') return error.BadToken;

                // keep the closing ] for the flow parser
                return .{ .flow_list = buf[1..] };
            },
            '{' => {
                if (buf.len < 2 or buf[buf.len - 1] != '}') return error.BadToken;

                // keep the closing } fpr the flow parser
                return .{ .flow_map = buf[1..] };
            },
            else => {
                return .{ .scalar = buf };
            },
        }
    }

    fn consumeLine(buf: []const u8) ![]const u8 {
        for (buf, 0..) |char, idx| {
            switch (char) {
                '\n' => return buf[0..idx],
                '\r' => return error.BadToken,
                else => {},
            }
        }

        return error.MissingNewline;
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    dupe_behavior: DuplicateKeyBehavior = .fail,
    default_object: DefaultObject = .fail,
    diagnostics: Diagnostics = .{
        .row = 0,
        .span = .{ .absolute = 0, .line_offset = 0, .length = 0 },
        .message = "all is well",
    },

    pub const Error = error{
        UnexpectedIndent,
        UnexpectedValue,
        ExtraContent,
        EmptyDocument,
        DuplicateKey,
        BadMapEntry,
        Fail,
    } || LineTokenizer.Error || FlowParser.Error || std.mem.Allocator.Error;

    pub const DuplicateKeyBehavior = enum {
        use_first,
        use_last,
        fail,
    };

    pub const DefaultObject = enum {
        string,
        list,
        map,
        fail,
    };

    pub const Map = std.StringHashMap;
    pub const List = std.ArrayList;

    pub const Value = union(enum) {
        string: std.ArrayList(u8),
        list: List(Value),
        map: Map(Value),

        pub inline fn fromString(alloc: std.mem.Allocator, input: []const u8) !Value {
            var res: Value = .{ .string = try std.ArrayList(u8).initCapacity(alloc, input.len) };
            res.string.appendSliceAssumeCapacity(input);
            return res;
        }

        pub inline fn newString(alloc: std.mem.Allocator) Value {
            return .{ .string = std.ArrayList(u8).init(alloc) };
        }

        pub inline fn newList(alloc: std.mem.Allocator) Value {
            return .{ .list = List(Value).init(alloc) };
        }

        pub inline fn newMap(alloc: std.mem.Allocator) Value {
            return .{ .map = Map(Value).init(alloc) };
        }

        pub fn printDebug(self: Value) void {
            self.printRecursive(0);
            std.debug.print("\n", .{});
        }

        fn printRecursive(self: Value, indent: usize) void {
            switch (self) {
                .string => |str| {
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
                .list => |list| {
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
                .map => |map| {
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

    pub const ParseState = enum {
        initial,
        value,
        done,
    };

    pub const Document = struct {
        arena: std.heap.ArenaAllocator,
        root: Value,

        pub fn printDebug(self: Document) void {
            return self.root.printDebug();
        }

        pub fn deinit(self: Document) void {
            self.arena.deinit();
        }
    };

    pub fn parseBuffer(self: *Parser, buffer: []const u8) Error!Document {
        var document: Document = .{
            .arena = std.heap.ArenaAllocator.init(self.allocator),
            .root = undefined,
        };
        errdefer document.deinit();
        const arena_alloc = document.arena.allocator();

        var state: ParseState = .initial;
        var expect_shift: LineTokenizer.ShiftDirection = .none;
        var dangling_key: ?[]const u8 = null;
        var stack = std.ArrayList(*Value).init(arena_alloc);
        defer stack.deinit();

        var tok: LineTokenizer = .{ .buffer = buffer, .diagnostics = &self.diagnostics };
        while (try tok.next()) |line| {
            if (line.contents == .comment) continue;

            var flip = true;
            var flop = false;
            // this is needed to give us a second go round when the line is dedented
            flipflop: while (flip) : (flop = true) {
                switch (state) {
                    .initial => {
                        if (line.indent == .indent) return error.UnexpectedIndent;

                        switch (line.contents) {
                            // we filter out comments above
                            .comment => unreachable,
                            .in_line => |in_line| switch (in_line) {
                                // empty scalars are only emitted for a list_item or a map_item
                                .empty => unreachable,
                                .scalar => |str| {
                                    document.root = try valueFromString(arena_alloc, str);
                                    state = .done;
                                },
                                .string => |str| {
                                    document.root = try valueFromString(arena_alloc, str);
                                    // cheesy technique for differentiating the different string types
                                    if (str[str.len - 1] != '\n') try document.root.string.append(' ');
                                    try stack.append(&document.root);
                                    state = .value;
                                },
                                .flow_list => |str| {
                                    document.root = try parseFlowList(arena_alloc, str, self.dupe_behavior);
                                    state = .done;
                                },
                                .flow_map => |str| {
                                    document.root = try parseFlowMap(arena_alloc, str, self.dupe_behavior);
                                    state = .done;
                                },
                            },
                            .list_item => |value| {
                                document.root = .{ .list = List(Value).init(arena_alloc) };
                                try stack.append(&document.root);

                                switch (value) {
                                    .empty => {
                                        expect_shift = .indent;
                                        state = .value;
                                    },
                                    .string, .scalar => |str| {
                                        try document.root.list.append(try valueFromString(arena_alloc, chopNewline(str)));
                                        state = .value;
                                    },
                                    .flow_list => |str| {
                                        try document.root.list.append(try parseFlowList(arena_alloc, str, self.dupe_behavior));
                                        state = .value;
                                    },
                                    .flow_map => |str| {
                                        try document.root.list.append(try parseFlowMap(arena_alloc, str, self.dupe_behavior));
                                        state = .value;
                                    },
                                }
                            },
                            .map_item => |pair| {
                                document.root = .{ .map = Map(Value).init(arena_alloc) };
                                try stack.append(&document.root);

                                switch (pair.val) {
                                    .empty => {
                                        expect_shift = .indent;
                                        // If the key is on its own line, we don't have
                                        // an associated value until we parse the next
                                        // line. We need to store a reference to this
                                        // key somewhere until we can consume the
                                        // value. More parser state to lug along.

                                        dangling_key = pair.key;
                                        state = .value;
                                    },
                                    .string, .scalar => |str| {
                                        // we can do direct puts here because this is
                                        // the very first line of the document
                                        try document.root.map.put(pair.key, try valueFromString(arena_alloc, chopNewline(str)));
                                        state = .value;
                                    },
                                    .flow_list => |str| {
                                        try document.root.map.put(pair.key, try parseFlowList(arena_alloc, str, self.dupe_behavior));
                                        state = .value;
                                    },
                                    .flow_map => |str| {
                                        try document.root.map.put(pair.key, try parseFlowMap(arena_alloc, str, self.dupe_behavior));
                                        state = .value;
                                    },
                                }
                            },
                        }
                    },
                    .value => switch (stack.getLast().*) {
                        .string => |*string| {
                            if (line.indent == .indent) return error.UnexpectedIndent;
                            if (!flop and line.indent == .dedent) {
                                _ = string.pop();

                                var dedent_depth = line.indent.dedent;
                                while (dedent_depth > 0) : (dedent_depth -= 1)
                                    _ = stack.pop();

                                continue :flipflop;
                            }

                            switch (line.contents) {
                                .comment => unreachable,
                                .in_line => |in_line| switch (in_line) {
                                    .empty => unreachable,
                                    .string => |str| {
                                        try string.appendSlice(str);
                                        if (str[str.len - 1] != '\n') try string.append(' ');
                                    },
                                    else => return error.UnexpectedValue,
                                },
                                else => return error.UnexpectedValue,
                            }
                        },
                        .list => |*list| {
                            if (expect_shift == .indent and line.indent != .indent)
                                try list.append(try valueFromString(arena_alloc, ""));

                            // Consider:
                            //
                            // -
                            //   own-line scalar
                            // - inline scalar
                            //
                            // the own-line scalar will not push the stack but the next list item will be a dedent
                            if (!flop and line.indent == .dedent) {
                                // if line.indent.dedent is 1 and we're expecting it, the stack will not be popped,
                                // but we will continue loop flipflop. However, flop will be set to false on the next
                                // trip, so this if prong will not be run again.
                                var dedent_depth = line.indent.dedent - @intFromBool(expect_shift == .dedent);

                                while (dedent_depth > 0) : (dedent_depth -= 1)
                                    _ = stack.pop();

                                continue :flipflop;
                            }

                            switch (line.contents) {
                                .comment => unreachable,
                                .in_line => |in_line| {
                                    // assert that this line has been indented. this is required for an inline value when
                                    // the stack is in list mode.
                                    if (expect_shift != .indent or line.indent != .indent)
                                        return error.UnexpectedValue;

                                    expect_shift = .dedent;
                                    switch (in_line) {
                                        .empty => unreachable,
                                        .scalar => |str| try list.append(try valueFromString(arena_alloc, str)),
                                        .flow_list => |str| try list.append(try parseFlowList(arena_alloc, str, self.dupe_behavior)),
                                        .flow_map => |str| try list.append(try parseFlowMap(arena_alloc, str, self.dupe_behavior)),
                                        .string => |str| {
                                            // string pushes the stack
                                            const new_string = try appendListGetValue(list, try valueFromString(arena_alloc, str));

                                            if (str[str.len - 1] != '\n')
                                                try new_string.string.append(' ');

                                            try stack.append(new_string);
                                            expect_shift = .none;
                                        },
                                    }
                                },
                                .list_item => |value| {
                                    switch (line.indent) {
                                        // for dedent, the stack has already been popped, so this should be fine
                                        .none, .dedent => {
                                            expect_shift = .none;
                                            switch (value) {
                                                .empty => expect_shift = .indent,
                                                .scalar, .string => |str| try list.append(try valueFromString(arena_alloc, chopNewline(str))),
                                                .flow_list => |str| try list.append(try parseFlowList(arena_alloc, str, self.dupe_behavior)),
                                                .flow_map => |str| try list.append(try parseFlowMap(arena_alloc, str, self.dupe_behavior)),
                                            }
                                        },
                                        // a new list is being created
                                        .indent => {
                                            if (expect_shift != .indent)
                                                return error.UnexpectedIndent;

                                            const new_list = try appendListGetValue(list, .{ .list = List(Value).init(arena_alloc) });
                                            try stack.append(new_list);

                                            expect_shift = .none;
                                            switch (value) {
                                                .empty => expect_shift = .indent,
                                                .scalar, .string => |str| try new_list.list.append(try valueFromString(arena_alloc, chopNewline(str))),
                                                .flow_list => |str| try new_list.list.append(try parseFlowList(arena_alloc, str, self.dupe_behavior)),
                                                .flow_map => |str| try new_list.list.append(try parseFlowMap(arena_alloc, str, self.dupe_behavior)),
                                            }
                                        },
                                    }
                                },
                                .map_item => |pair| {
                                    // this prong cannot be hit on dedent in a valid way.
                                    //
                                    //    -
                                    //      map: value
                                    //      second: value
                                    //    third: value
                                    //
                                    // dedenting back to the list stack level requires list_item

                                    if (line.indent != .indent)
                                        return error.UnexpectedValue;

                                    const new_map = try appendListGetValue(list, .{ .map = Map(Value).init(arena_alloc) });
                                    try stack.append(new_map);
                                    expect_shift = .none;

                                    switch (pair.val) {
                                        .empty => {
                                            dangling_key = pair.key;
                                            expect_shift = .indent;
                                        },
                                        .scalar, .string => |str| try new_map.map.put(pair.key, try valueFromString(arena_alloc, chopNewline(str))),
                                        .flow_list => |str| try new_map.map.put(pair.key, try parseFlowList(arena_alloc, str, self.dupe_behavior)),
                                        .flow_map => |str| try new_map.map.put(pair.key, try parseFlowMap(arena_alloc, str, self.dupe_behavior)),
                                    }
                                },
                            }
                        },
                        .map => |*map| {
                            if (expect_shift == .indent and line.indent != .indent) {
                                try putMap(
                                    map,
                                    dangling_key orelse return error.Fail,
                                    try valueFromString(arena_alloc, ""),
                                    self.dupe_behavior,
                                );
                                dangling_key = null;
                            }

                            if (!flop and line.indent == .dedent) {
                                var dedent_depth = line.indent.dedent - @intFromBool(expect_shift == .dedent);

                                while (dedent_depth > 0) : (dedent_depth -= 1)
                                    _ = stack.pop();

                                continue :flipflop;
                            }

                            switch (line.contents) {
                                .comment => unreachable,
                                .in_line => |in_line| {
                                    // assert that this line has been indented. this is required for an inline value when
                                    // the stack is in map mode.
                                    if (expect_shift != .indent or line.indent != .indent or dangling_key == null)
                                        return error.UnexpectedValue;

                                    expect_shift = .dedent;

                                    switch (in_line) {
                                        .empty => unreachable,
                                        .scalar => |str| try putMap(map, dangling_key.?, try valueFromString(arena_alloc, str), self.dupe_behavior),
                                        .flow_list => |str| try putMap(map, dangling_key.?, try parseFlowList(arena_alloc, str, self.dupe_behavior), self.dupe_behavior),
                                        .flow_map => |str| {
                                            try putMap(map, dangling_key.?, try parseFlowMap(arena_alloc, str, self.dupe_behavior), self.dupe_behavior);
                                        },
                                        .string => |str| {
                                            // string pushes the stack
                                            const new_string = try putMapGetValue(map, dangling_key.?, try valueFromString(arena_alloc, str), self.dupe_behavior);
                                            if (str[str.len - 1] != '\n') try new_string.string.append(' ');
                                            try stack.append(new_string);
                                            expect_shift = .none;
                                        },
                                    }

                                    dangling_key = null;
                                },
                                .list_item => |value| {
                                    // this prong cannot be hit on dedent in a valid way.
                                    //
                                    //    map:
                                    //      - value
                                    //    - invalid
                                    //
                                    // dedenting back to the map stack level requires map_item

                                    if (expect_shift != .indent or line.indent != .indent or dangling_key == null)
                                        return error.UnexpectedValue;

                                    const new_list = try putMapGetValue(map, dangling_key.?, .{ .list = List(Value).init(arena_alloc) }, self.dupe_behavior);
                                    try stack.append(new_list);
                                    dangling_key = null;

                                    expect_shift = .none;
                                    switch (value) {
                                        .empty => expect_shift = .indent,
                                        .scalar, .string => |str| try new_list.list.append(try valueFromString(arena_alloc, chopNewline(str))),
                                        .flow_list => |str| try new_list.list.append(try parseFlowList(arena_alloc, str, self.dupe_behavior)),
                                        .flow_map => |str| try new_list.list.append(try parseFlowMap(arena_alloc, str, self.dupe_behavior)),
                                    }
                                },
                                .map_item => |pair| {
                                    expect_shift = .none;
                                    switch (line.indent) {
                                        // for dedent, the stack has already been popped, so this should be fine
                                        .none, .dedent => switch (pair.val) {
                                            .empty => {
                                                expect_shift = .indent;
                                                dangling_key = pair.key;
                                            },
                                            .scalar, .string => |str| try putMap(map, pair.key, try valueFromString(arena_alloc, chopNewline(str)), self.dupe_behavior),
                                            .flow_list => |str| try putMap(map, pair.key, try parseFlowList(arena_alloc, str, self.dupe_behavior), self.dupe_behavior),
                                            .flow_map => |str| try putMap(map, pair.key, try parseFlowMap(arena_alloc, str, self.dupe_behavior), self.dupe_behavior),
                                        },
                                        // a new map is being created
                                        .indent => {
                                            if (expect_shift != .indent or dangling_key == null) return error.UnexpectedValue;

                                            const new_map = try putMapGetValue(map, dangling_key.?, .{ .map = Map(Value).init(arena_alloc) }, self.dupe_behavior);
                                            try stack.append(new_map);
                                            dangling_key = null;

                                            switch (pair.val) {
                                                .empty => {
                                                    expect_shift = .indent;
                                                    dangling_key = pair.key;
                                                },
                                                .scalar, .string => |str| try new_map.map.put(pair.key, try valueFromString(arena_alloc, chopNewline(str))),
                                                .flow_list => |str| try new_map.map.put(pair.key, try parseFlowList(arena_alloc, str, self.dupe_behavior)),
                                                .flow_map => |str| try new_map.map.put(pair.key, try parseFlowMap(arena_alloc, str, self.dupe_behavior)),
                                            }
                                        },
                                    }
                                },
                            }
                        },
                    },
                    .done => return error.ExtraContent,
                }

                // this is specifically performed at the end of the loop body so that
                // `continue :flipflop` skips setting it.
                flip = false;
            }
        }

        switch (state) {
            .initial => switch (self.default_object) {
                .string => document.root = .{ .string = std.ArrayList(u8).init(arena_alloc) },
                .list => document.root = .{ .list = List(Value).init(arena_alloc) },
                .map => document.root = .{ .map = Map(Value).init(arena_alloc) },
                .fail => return error.EmptyDocument,
            },
            .value => switch (stack.getLast().*) {
                // remove the final trailing newline or space
                .string => |*string| _ = string.popOrNull(),
                // if we have a dangling -, attach an empty string to it
                .list => |*list| if (expect_shift == .indent) try list.append(try valueFromString(arena_alloc, "")),
                // if we have a dangling "key:", attach an empty string to it
                .map => |*map| if (dangling_key) |dk| try putMap(map, dk, try valueFromString(arena_alloc, ""), self.dupe_behavior),
            },
            .done => {},
        }

        return document;
    }

    inline fn chopNewline(buf: []const u8) []const u8 {
        return if (buf.len > 0 and buf[buf.len - 1] == '\n') buf[0 .. buf.len - 1] else buf;
    }

    fn valueFromString(alloc: std.mem.Allocator, buffer: []const u8) Error!Value {
        var result: Value = .{ .string = try std.ArrayList(u8).initCapacity(alloc, buffer.len) };
        result.string.appendSliceAssumeCapacity(buffer);
        return result;
    }

    fn parseFlowList(alloc: std.mem.Allocator, contents: []const u8) Error!Value {
        // TODO: if we pass in the parse stack, is it straightforward to support nested
        // lists/maps? Can seek the split iterator by manually setting index.
        var result: Value = .{ .list = List(Value).init(alloc) };

        // TODO: consume exactly one space after the comma
        var splitter = std.mem.splitScalar(u8, contents, ',');
        while (splitter.next()) |entry| {
            try result.list.append(
                try valueFromString(alloc, std.mem.trim(u8, entry, " ")),
            );
        }

        return result;
    }

    fn parseFlowMap(self: *Parser, alloc: std.mem.Allocator, contents: []const u8) Error!Value {
        var result: Value = .{ .map = Map(Value).init(alloc) };

        var splitter = std.mem.splitScalar(u8, contents, ',');

        while (splitter.next()) |entry| {
            const trimmed = std.mem.trim(u8, entry, " ");

            // TODO: consume exactly one space after the colon?
            const colon = std.mem.indexOfScalar(u8, trimmed, ':') orelse
                return error.BadMapEntry;

            try putMap(
                &result.map,
                trimmed[0..colon],
                try valueFromString(alloc, std.mem.trimLeft(u8, trimmed[colon + 1 .. trimmed.len], " ")),
                self.dupe_behavior,
            );
        }

        return result;
    }

    inline fn appendListGetValue(list: *List(Value), value: Value) Error!*Value {
        try list.append(value);
        return &list.items[list.items.len - 1];
    }

    inline fn putMap(map: *Map(Value), key: []const u8, value: Value, dupe_behavior: DuplicateKeyBehavior) Error!void {
        _ = try putMapGetValue(map, key, value, dupe_behavior);
    }

    inline fn putMapGetValue(map: *Map(Value), key: []const u8, value: Value, dupe_behavior: DuplicateKeyBehavior) Error!*Value {
        const gop = try map.getOrPut(key);

        if (gop.found_existing)
            switch (dupe_behavior) {
                .fail => return error.DuplicateKey,
                .use_first => {},
                .use_last => gop.value_ptr.* = value,
            }
        else
            gop.value_ptr.* = value;

        return gop.value_ptr;
    }

    pub fn dumpBufLines(self: *Parser, buf: []const u8) Error!void {
        var tok: LineTokenizer = .{ .buffer = buf, .diagnostics = &self.diagnostics };
        while (try tok.next()) |line| {
            dumpLine(line);
        }
    }

    fn dumpLine(line: LineTokenizer.Line) void {
        var dedbuf: [64]u8 = .{0} ** 64;
        var keybuf: [2048]u8 = .{0} ** 2048;
        var valbuf: [2048]u8 = .{0} ** 2048;

        const shiftstr = if (line.indent == .dedent)
            std.fmt.bufPrint(&dedbuf, " ({d})", .{line.indent.dedent}) catch unreachable
        else
            "";

        std.debug.print("{s}{s}: {s} => {s}\n", .{
            @tagName(line.indent), shiftstr, @tagName(line.contents), switch (line.contents) {
                .comment => |str| str,
                .in_line, .list_item => |scalar| switch (scalar) {
                    .empty => "[empty]",
                    .scalar,
                    .string,
                    .flow_list,
                    .flow_map,
                    => |str| std.fmt.bufPrint(&keybuf, "{s} => {s}", .{ @tagName(scalar), str }) catch unreachable,
                },
                .map_item => |map| std.fmt.bufPrint(&keybuf, "{s} : {s}", .{
                    map.key,
                    switch (map.val) {
                        .empty => "[empty]",
                        .scalar,
                        .string,
                        .flow_list,
                        .flow_map,
                        => |str| std.fmt.bufPrint(&valbuf, "{s} => {s}", .{ @tagName(map.val), str }) catch unreachable,
                    },
                }) catch unreachable,
            },
        });
    }
};
