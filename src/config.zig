// Heavily inspired by, but not quite compatible with, NestedText. Key differences:
//
// - Doesn't support multiline keys (this means map keys cannot start with
//   ' ', \t, #, {, [, or >, and they cannot contain :)
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
// - trailing spaces are not trimmed (and may cause errors in the future)
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
//   cases since I'm slinging code, not writing a spec.

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

        const relative: RelativeIndent = if (indent > self.last_indent)
            .indent
        else if (indent < self.last_indent)
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
                };
            },
            '|', '>', '[', '{' => {
                return .{
                    .indent = relative,
                    .contents = .{ .in_line = try detectInlineItem(line) },
                };
            },
            '-' => {
                if (line.len > 1 and line[1] != ' ') return error.BadToken;

                return if (line.len == 1) .{
                    .indent = relative,
                    .contents = .{ .list_item = .empty },
                } else .{
                    .indent = relative,
                    .contents = .{ .list_item = try detectInlineItem(line[2..]) },
                };
            },
            else => {
                for (line, 0..) |char, idx| {
                    if (char == ':') {
                        if (idx + 1 == line.len) return .{
                            .indent = relative,
                            .contents = .{ .map_item = .{ .key = line[0..idx], .val = .empty } },
                        };

                        if (line[idx + 1] != ' ') return error.BadToken;

                        return .{
                            .indent = relative,
                            .contents = .{ .map_item = .{
                                .key = line[0..idx],
                                .val = try detectInlineItem(line[idx + 2 ..]),
                            } },
                        };
                    }
                }

                return .{
                    .indent = relative,
                    .contents = .{ .in_line = .{ .scalar = line } },
                };
            },
        }
    }

    fn detectInlineItem(buf: []const u8) Error!InlineItem {
        if (buf.len == 0) return .empty;

        switch (buf[0]) {
            '|', '>' => |char| {
                if (buf.len > 1 and buf[1] != ' ') return error.BadToken;

                return if (buf.len == 1) .{
                    .string = if (char == '|') buf[1..] else buf.ptr[1 .. buf.len + 1],
                } else .{
                    .string = if (char == '|') buf[2..] else buf.ptr[2 .. buf.len + 1],
                };
            },
            '[' => {
                if (buf.len < 2 or buf[buf.len - 1] != ']') return error.BadToken;

                return .{ .flow_list = buf[1 .. buf.len - 1] };
            },
            '{' => {
                if (buf.len < 2 or buf[buf.len - 1] != '}') return error.BadToken;

                return .{ .flow_map = buf[1 .. buf.len - 1] };
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
        Fail,
    } || LineTokenizer.Error || std.mem.Allocator.Error;

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
    };

    pub const ParseState = enum {
        initial,
        value,
        done,
    };

    pub const Document = struct {
        arena: std.heap.ArenaAllocator,
        root: Value,

        fn deinit(self: *Document) void {
            self.arena.deinit();
        }
    };

    pub fn parseBuffer(self: *Parser, buffer: []const u8, allocator: std.mem.Allocator) Error!Document {
        var document: Document = .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .root = undefined,
        };
        errdefer document.deinit();
        const arena_alloc = document.arena.allocator();

        var state: ParseState = .initial;
        var expect_shift: LineTokenizer.ShiftDirection = .none;
        var empty_key: ?[]const u8 = null;
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
                                .flow_list => |str| {
                                    document.root = try parseFlowList(arena_alloc, str);
                                    state = .done;
                                },
                                .flow_map => |str| {
                                    document.root = try parseFlowMap(arena_alloc, str);
                                    state = .done;
                                },
                                .string => |str| {
                                    document.root = valueFromString(arena_alloc, str);
                                    // cheesy technique for differentiating the different string types
                                    // if (str[str.len-1] != '\n') try document.root.string.append(' ');
                                    try stack.append(&document.root);
                                    state = .value;
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
                                        document.root.list.append(try valueFromString(arena_alloc, str));
                                        state = .value;
                                    },
                                    .flow_list => |str| {
                                        document.root.list.append(try parseFlowList(arena_alloc, str));
                                        state = .value;
                                    },
                                    .flow_map => |str| {
                                        document.root.list.append(try parseFlowMap(arena_alloc, str));
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

                                        empty_key = pair.key;
                                        state = .value;
                                    },
                                    .string, .scalar => |str| {
                                        // we can do direct puts here because this is
                                        // the very first line of the document
                                        document.root.map.put(pair.key, try valueFromString(arena_alloc, str));
                                        state = .value;
                                    },
                                    .flow_list => |str| {
                                        document.root.map.put(pair.key, try parseFlowList(arena_alloc, str));
                                        state = .value;
                                    },
                                    .flow_map => |str| {
                                        document.root.map.put(pair.key, try parseFlowMap(arena_alloc, str));
                                        state = .value;
                                    },
                                }
                            },
                        }
                    },
                    .value => switch (stack.getLast()) {
                        .string => |*string| {
                            if (line.indent == .indent) return error.UnexpectedIndent;
                            if (!flop and line.indent == .dedent) {
                                // TODO: remove final newline or trailing space here

                                var dedent_depth = line.indent.dedent;
                                while (dedent_depth > 0) : (dedent_depth -= 1)
                                    _ = stack.pop();

                                continue :flipflop;
                            }

                            switch (line.contents) {
                                .comment => unreachable,
                                .in_line => |in_line| switch (in_line) {
                                    .empty => unreachable,
                                    .string => |str| try string.appendSlice(str),
                                    else => return error.UnexpectedValue,
                                },
                                else => return error.UnexpectedValue,
                            }
                        },
                        .list => |*list| {
                            if (expect_shift == .indent and line.indent != .indent) return error.ExpectedIndent;
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
                                    if (line.indent != .indent) return error.UnexpectedValue;

                                    switch (in_line) {
                                        .empty => unreachable,
                                        .scalar => |str| {
                                            expect_shift = .dedent;
                                            try list.append(try valueFromString(arena_alloc, str));
                                        },
                                        .string => |str| {
                                            // string pushes the stack
                                            expect_shift = .none;
                                            list.append(try valueFromString(arena_alloc, str));
                                            try stack.append(&list.items[list.items.len - 1]);
                                        },
                                        .flow_list => |str| {
                                            expect_shift = .dedent;
                                            try list.append(try parseFlowList(arena_alloc, str));
                                        },
                                        .flow_map => |str| {
                                            expect_shift = .dedent;
                                            try list.append(try parseFlowList(arena_alloc, str));
                                        },
                                    }
                                },
                                .list_item => |value| switch (value) {
                                    .empty => expect_shift = .indent,
                                    .scalar, .string => |str| list.append(try valueFromString(arena_alloc, str)),
                                    .flow_list => |str| try list.append(try parseFlowList(arena_alloc, str)),
                                    .flow_map => |str| try list.append(try parseFlowList(arena_alloc, str)),
                                },
                                .map_item => return error.UnexpectedValue,
                            }
                        },
                        .map => {
                            if (expect_shift and line.indent != .indent) return error.ExpectedIndent;
                            if (!flop and line.indent == .dedent) {
                                _ = stack.pop();
                                continue :flipflop;
                            }
                        },
                    },
                    .done => return error.ExtraContent,
                }
            }

            // this is specifically performed at the end of the loop body so that
            // `continue :flipflop` skips setting it.
            flip = false;
        }

        switch (state) {
            .initial => switch (self.default_object) {
                .string => document.root = .{ .string = std.ArrayList(u8).init(arena_alloc) },
                .list => document.root = .{ .list = List(Value).init(arena_alloc) },
                .map => document.root = .{ .map = Map(Value).init(arena_alloc) },
                .fail => return error.EmptyDocument,
            },
            .value => switch (stack.getLast()) {
                .string => {
                    // remove final character (newline or trailing space)
                    },
                .list => {},
                .map => {},
            },
            .done => {},
        }

        return document;
    }

    fn valueFromString(alloc: std.mem.Allocator, buffer: []const u8) Error!Value {
        var result: Value = .{ .string = try std.ArrayList(u8).initCapacity(alloc, buffer.len) };
        result.string.appendSliceAssumeCapacity(buffer);
        return result;
    }

    fn parseFlowList(alloc: std.mem.Allocator, contents: []const u8) Error!Value {
        var result: Value = .{ .list = List(Value).init(alloc) };

        var splitter = std.mem.splitScalar(u8, contents, ',');
        while (splitter.next()) |chunk| {
            try result.list.append(
                try valueFromString(alloc, std.mem.trim(u8, chunk, " ")),
            );
        }

        return result;
    }

    fn parseFlowMap(alloc: std.mem.Allocator, contents: []const u8) Error!Value {
        _ = contents;
        var result: Value = .{ .map = Map(Value).init(alloc) };
        var splitter = std.mem.splitScalar()
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
