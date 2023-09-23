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
// - terminated strings to allow trailing whitespace:
//      | this string has trailing whitespace    |
//      > and so does this one                   |
// - The parser is both strict and probably sloppy and may have weird edge
//   cases since I'm slinging code, not writing a spec. For example, tabs are
//   not trimmed from the values of inline lists/maps

const std = @import("std");

pub const IndexSlice = struct { start: usize, len: usize };

pub const Diagnostics = struct {
    row: usize,
    span: struct { absolute: usize, line_offset: usize, length: usize },
    message: []const u8,
};

pub const LineBuffer = struct {
    allocator: std.mem.Allocator,
    buffer: []u8,
    used: usize,
    window: IndexSlice,

    pub const default_capacity: usize = 4096;
    pub const Error = std.mem.Allocator.Error;

    pub fn init(allocator: std.mem.Allocator) Error!LineBuffer {
        return initCapacity(allocator, default_capacity);
    }

    pub fn initCapacity(allocator: std.mem.Allocator, capacity: usize) Error!LineBuffer {
        return .{
            .allocator = allocator,
            .buffer = try allocator.alloc(u8, capacity),
            .used = 0,
            .window = .{ .start = 0, .len = 0 },
        };
    }

    pub fn feed(self: *LineBuffer, data: []const u8) Error!void {
        if (data.len == 0) return;
        // TODO: check for usize overflow here if we want Maximum Robustness
        const new_window_len = self.window.len + data.len;

        // data cannot fit in the buffer with our scan window, so we have to realloc
        if (new_window_len > self.buffer.len) {
            // TODO: adopt an overallocation strategy? Will potentially avoid allocating
            //       on every invocation but will cause the buffer to oversize
            try self.allocator.realloc(self.buffer, new_window_len);
            self.rehome();
            @memcpy(self.buffer[self.used..].ptr, data);
            self.used = new_window_len;
            self.window.len = new_window_len;
        }
        // data will fit, but needs to be moved in the buffer
        else if (self.window.start + new_window_len > self.buffer.len) {
            self.rehome();
            @memcpy(self.buffer[self.used..].ptr, data);
            self.used = new_window_len;
            self.window.len = new_window_len;
        }
        // data can simply be appended
        else {
            @memcpy(self.buffer[self.used..].ptr, data);
        }
    }

    /// The memory returned by this function is valid until the next call to `feed`.
    /// The resulting slice does not include the newline character.
    pub fn nextLine(self: *LineBuffer) ?[]const u8 {
        if (self.window.start >= self.buffer.len or self.window.len == 0)
            return null;

        const window = self.buffer[self.window.start..][0..self.window.len];
        const split = std.mem.indexOfScalar(u8, window, '\n') orelse return null;

        self.window.start += split + 1;
        self.window.len -= split + 1;

        return window[0..split];
    }

    fn rehome(self: *LineBuffer) void {
        if (self.window.start == 0) return;

        const window = self.buffer[self.window.start..][0..self.window.len];

        if (self.window.len > self.window.start)
            std.mem.copyForwards(u8, self.buffer, window)
        else
            @memcpy(self.buffer.ptr, window);

        self.window.start = 0;
        self.used = window.len;
    }
};

pub const FixedLineBuffer = struct {
    buffer: []const u8,
    window: IndexSlice,

    pub fn init(data: []const u8) FixedLineBuffer {
        return .{ .buffer = data, .window = .{ .start = 0, .len = data.len } };
    }

    pub fn nextLine(self: *FixedLineBuffer) ?[]const u8 {
        if (self.window.start >= self.buffer.len or self.window.len == 0)
            return null;

        const window = self.buffer[self.window.start..][0..self.window.len];
        const split = std.mem.indexOfScalar(u8, window, '\n') orelse return null;

        self.window.start += split + 1;
        self.window.len -= split + 1;

        return window[0..split];
    }
};

const IndentationType = union(enum) {
    immaterial: void,
    spaces: usize,
    tabs: void,
};

const InlineItem = union(enum) {
    empty: void,
    scalar: []const u8,
    line_string: []const u8,
    space_string: []const u8,

    flow_list: []const u8,
    flow_map: []const u8,

    fn lineEnding(self: InlineItem) u8 {
        return switch (self) {
            .line_string => '\n',
            .space_string => ' ',
            else => unreachable,
        };
    }
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

pub fn LineTokenizer(comptime Buffer: type) type {
    return struct {
        buffer: Buffer,
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

        pub fn next(self: *@This()) Error!?Line {
            lineloop: while (self.buffer.nextLine()) |raw_line| {
                var indent: usize = 0;
                for (raw_line, 0..) |char, idx| {
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
                        },
                        '\t' => {
                            switch (self.indentation) {
                                .immaterial => self.indentation = .tabs,
                                .spaces => return error.MixedIndentation,
                                .tabs => {},
                            }
                        },
                        '\r' => {
                            return error.BadToken;
                        },
                        else => {
                            indent = idx;
                            break;
                        },
                    }
                } else {
                    if (raw_line.len > 0) return error.TrailingWhitespace;
                    continue :lineloop;
                }

                var quantized: usize = if (self.indentation == .spaces) quant: {
                    if (self.indentation.spaces == 0) {
                        self.indentation.spaces = indent;
                    }
                    if (@rem(indent, self.indentation.spaces) != 0)
                        return error.UnquantizedIndentation;

                    break :quant @divExact(indent, self.indentation.spaces);
                } else indent;

                const relative: RelativeIndent = if (quantized > self.last_indent) rel: {
                    if ((quantized - self.last_indent) > 1)
                        return error.TooMuchIndentation;
                    break :rel .indent;
                } else if (quantized < self.last_indent)
                    .{ .dedent = self.last_indent - quantized }
                else
                    .none;

                defer {
                    self.row += 1;
                    self.last_indent = quantized;
                }

                const line = raw_line[indent..];

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

                // somehow everything else has failed
                return error.Impossible;
            }
            return null;
        }

        fn detectInlineItem(buf: []const u8) Error!InlineItem {
            if (buf.len == 0) return .empty;

            switch (buf[0]) {
                '>', '|' => |char| {
                    if (buf.len > 1 and buf[1] != ' ') return error.BadToken;

                    const slice: []const u8 = switch (buf[buf.len - 1]) {
                        ' ', '\t' => return error.TrailingWhitespace,
                        '|' => buf[@min(2, buf.len) .. buf.len - @intFromBool(buf.len > 1)],
                        else => buf[@min(2, buf.len)..buf.len],
                    };

                    return if (char == '>')
                        .{ .line_string = slice }
                    else
                        .{ .space_string = slice };
                },
                '[' => {
                    if (buf.len < 2 or buf[buf.len - 1] != ']')
                        return error.BadToken;

                    // keep the closing ] for the flow parser
                    return .{ .flow_list = buf[1..] };
                },
                '{' => {
                    if (buf.len < 2 or buf[buf.len - 1] != '}')
                        return error.BadToken;

                    // keep the closing } fpr the flow parser
                    return .{ .flow_map = buf[1..] };
                },
                else => {
                    if (buf[buf.len - 1] == ' ' or buf[buf.len - 1] == '\t')
                        return error.TrailingWhitespace;

                    return .{ .scalar = buf };
                },
            }
        }
    };
}

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
        BadState,
        BadToken,
        Fail,
    } || LineTokenizer(FixedLineBuffer).Error || std.mem.Allocator.Error;

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

    pub const ParseState = enum {
        initial,
        value,
        done,
    };

    pub const Document = struct {
        arena: std.heap.ArenaAllocator,
        root: Value,

        pub fn init(alloc: std.mem.Allocator) Document {
            return .{
                .arena = std.heap.ArenaAllocator.init(alloc),
                .root = undefined,
            };
        }

        pub fn printDebug(self: Document) void {
            return self.root.printDebug();
        }

        pub fn deinit(self: Document) void {
            self.arena.deinit();
        }
    };

    pub const State = struct {
        pub const Stack = std.ArrayList(*Value);

        document: Document,
        value_stack: Stack,
        state: ParseState = .initial,
        expect_shift: ShiftDirection = .none,
        dangling_key: ?[]const u8 = null,

        pub fn init(alloc: std.mem.Allocator) State {
            return .{
                .document = Document.init(alloc),
                .value_stack = Stack.init(alloc),
            };
        }

        pub fn deinit(self: State) void {
            self.value_stack.deinit();
        }
    };

    pub fn parseBuffer(self: *Parser, buffer: []const u8) Error!Document {
        var document = Document.init(self.allocator);
        errdefer document.deinit();
        const arena_alloc = document.arena.allocator();

        var state: ParseState = .initial;
        var expect_shift: ShiftDirection = .none;
        var dangling_key: ?[]const u8 = null;
        var stack = std.ArrayList(*Value).init(arena_alloc);
        defer stack.deinit();

        var tok: LineTokenizer(FixedLineBuffer) = .{
            .buffer = FixedLineBuffer.init(buffer),
            .diagnostics = &self.diagnostics,
        };

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
                                    document.root = try Value.fromScalar(arena_alloc, str);
                                    // this is a cheesy hack. If the document consists
                                    // solely of a scalar, the finalizer will try to
                                    // chop a line ending off of it, so we need to add
                                    // a sacrificial padding character to avoid
                                    // chopping off something that matters.
                                    try document.root.string.append(' ');
                                    state = .done;
                                },
                                .line_string, .space_string => |str| {
                                    document.root = try Value.fromString(arena_alloc, str);
                                    try document.root.string.append(in_line.lineEnding());
                                    try stack.append(&document.root);
                                    state = .value;
                                },
                                .flow_list => |str| {
                                    document.root = try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior);
                                    state = .done;
                                },
                                .flow_map => |str| {
                                    document.root = try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior);
                                    state = .done;
                                },
                            },
                            .list_item => |value| {
                                document.root = Value.newList(arena_alloc);
                                try stack.append(&document.root);

                                switch (value) {
                                    .empty => {
                                        expect_shift = .indent;
                                        state = .value;
                                    },
                                    .scalar => |str| {
                                        try document.root.list.append(try Value.fromScalar(arena_alloc, str));
                                        state = .value;
                                    },
                                    .line_string, .space_string => |str| {
                                        try document.root.list.append(try Value.fromString(arena_alloc, str));
                                        state = .value;
                                    },
                                    .flow_list => |str| {
                                        try document.root.list.append(try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior));
                                        state = .value;
                                    },
                                    .flow_map => |str| {
                                        try document.root.list.append(try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior));
                                        state = .value;
                                    },
                                }
                            },
                            .map_item => |pair| {
                                document.root = Value.newMap(arena_alloc);
                                try stack.append(&document.root);

                                switch (pair.val) {
                                    .empty => {
                                        expect_shift = .indent;
                                        // If the key is on its own line, we don't have
                                        // an associated value until we parse the next
                                        // line. We need to store a reference to this
                                        // key somewhere until we can consume the
                                        // value. More parser state to lug along.

                                        dangling_key = try arena_alloc.dupe(u8, pair.key);
                                        state = .value;
                                    },
                                    .scalar => |str| {
                                        // we can do direct puts here because this is
                                        // the very first line of the document
                                        try document.root.map.put(pair.key, try Value.fromScalar(arena_alloc, str));
                                        state = .value;
                                    },
                                    .line_string, .space_string => |str| {
                                        // we can do direct puts here because this is
                                        // the very first line of the document
                                        try document.root.map.put(pair.key, try Value.fromString(arena_alloc, str));
                                        state = .value;
                                    },
                                    .flow_list => |str| {
                                        try document.root.map.put(pair.key, try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior));
                                        state = .value;
                                    },
                                    .flow_map => |str| {
                                        try document.root.map.put(pair.key, try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior));
                                        state = .value;
                                    },
                                }
                            },
                        }
                    },
                    .value => switch (stack.getLast().*) {
                        // these three states are never reachable here. flow_list and
                        // flow_map are parsed with a separate state machine. These
                        // value types can only be present by themselves as the first
                        // line of the document, in which case the document consists
                        // only of that single line: this parser jumps immediately into
                        // the .done state, bypassing the .value state in which this
                        // switch is embedded.
                        .scalar, .flow_list, .flow_map => unreachable,
                        .string => |*string| {
                            if (line.indent == .indent)
                                return error.UnexpectedIndent;

                            if (!flop and line.indent == .dedent) {
                                // kick off the last trailing space or newline
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
                                    .line_string, .space_string => |str| {
                                        try string.appendSlice(str);
                                        try string.append(in_line.lineEnding());
                                    },
                                    else => return error.UnexpectedValue,
                                },
                                else => return error.UnexpectedValue,
                            }
                        },
                        .list => |*list| {
                            // detect that the previous item was actually empty
                            //
                            //    -
                            //    - something
                            //
                            // the first line here creates the expect_shift, but the second line
                            // is a valid continuation of the list despite not being indented
                            if (!flop and (expect_shift == .indent and line.indent != .indent))
                                try list.append(Value.newScalar(arena_alloc));

                            // Consider:
                            //
                            //    -
                            //      own-line scalar
                            //    - inline scalar
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
                                        .scalar => |str| try list.append(try Value.fromScalar(arena_alloc, str)),
                                        .flow_list => |str| try list.append(try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior)),
                                        .flow_map => |str| try list.append(try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior)),
                                        .line_string, .space_string => |str| {
                                            // string pushes the stack
                                            const new_string = try appendListGetValue(list, try Value.fromString(arena_alloc, str));
                                            try stack.append(new_string);

                                            try new_string.string.append(in_line.lineEnding());
                                            expect_shift = .none;
                                        },
                                    }
                                },
                                .list_item => |value| {
                                    if (flop or (line.indent == .none or line.indent == .dedent)) {
                                        expect_shift = .none;
                                        switch (value) {
                                            .empty => expect_shift = .indent,
                                            .scalar => |str| try list.append(try Value.fromScalar(arena_alloc, str)),
                                            .line_string, .space_string => |str| try list.append(try Value.fromString(arena_alloc, str)),
                                            .flow_list => |str| try list.append(try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior)),
                                            .flow_map => |str| try list.append(try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior)),
                                        }
                                    } else if (line.indent == .indent) {
                                        if (expect_shift != .indent) return error.UnexpectedIndent;

                                        const new_list = try appendListGetValue(list, Value.newList(arena_alloc));
                                        try stack.append(new_list);
                                        expect_shift = .none;
                                        continue :flipflop;
                                    } else unreachable;
                                },
                                .map_item => {
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

                                    const new_map = try appendListGetValue(list, Value.newMap(arena_alloc));
                                    try stack.append(new_map);
                                    expect_shift = .none;
                                    continue :flipflop;
                                },
                            }
                        },
                        .map => |*map| {
                            // detect that the previous item was actually empty
                            //
                            //    foo:
                            //    bar: baz
                            //
                            // the first line here creates the expect_shift, but the second line
                            // is a valid continuation of the map despite not being indented
                            if (!flop and (expect_shift == .indent and line.indent != .indent)) {
                                try putMap(
                                    map,
                                    dangling_key orelse return error.Fail,
                                    Value.newScalar(arena_alloc),
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
                                        .scalar => |str| try putMap(map, dangling_key.?, try Value.fromScalar(arena_alloc, str), self.dupe_behavior),
                                        .flow_list => |str| try putMap(map, dangling_key.?, try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior), self.dupe_behavior),
                                        .flow_map => |str| {
                                            try putMap(map, dangling_key.?, try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior), self.dupe_behavior);
                                        },
                                        .line_string, .space_string => |str| {
                                            // string pushes the stack
                                            const new_string = try putMapGetValue(map, dangling_key.?, try Value.fromString(arena_alloc, str), self.dupe_behavior);
                                            try new_string.string.append(in_line.lineEnding());
                                            try stack.append(new_string);
                                            expect_shift = .none;
                                        },
                                    }

                                    dangling_key = null;
                                },
                                .list_item => {
                                    // this prong cannot be hit on dedent in a valid way.
                                    //
                                    //    map:
                                    //      - value
                                    //    - invalid
                                    //
                                    // dedenting back to the map stack level requires map_item

                                    if (expect_shift != .indent or line.indent != .indent or dangling_key == null)
                                        return error.UnexpectedValue;

                                    const new_list = try putMapGetValue(map, dangling_key.?, Value.newList(arena_alloc), self.dupe_behavior);
                                    try stack.append(new_list);
                                    dangling_key = null;
                                    expect_shift = .none;
                                    continue :flipflop;
                                },
                                .map_item => |pair| {
                                    if (flop or (line.indent == .none or line.indent == .dedent)) {
                                        expect_shift = .none;
                                        switch (pair.val) {
                                            .empty => {
                                                expect_shift = .indent;
                                                dangling_key = try arena_alloc.dupe(u8, pair.key);
                                            },
                                            .scalar => |str| try putMap(map, pair.key, try Value.fromScalar(arena_alloc, str), self.dupe_behavior),
                                            .line_string, .space_string => |str| try putMap(map, pair.key, try Value.fromString(arena_alloc, str), self.dupe_behavior),
                                            .flow_list => |str| try putMap(map, pair.key, try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior), self.dupe_behavior),
                                            .flow_map => |str| try putMap(map, pair.key, try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior), self.dupe_behavior),
                                        }
                                    } else if (line.indent == .indent) {
                                        if (expect_shift != .indent or dangling_key == null) return error.UnexpectedValue;

                                        const new_map = try putMapGetValue(map, dangling_key.?, Value.newMap(arena_alloc), self.dupe_behavior);
                                        try stack.append(new_map);
                                        dangling_key = null;
                                        continue :flipflop;
                                    } else unreachable;
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
                .list => document.root = Value.newList(arena_alloc),
                .map => document.root = Value.newMap(arena_alloc),
                .fail => return error.EmptyDocument,
            },
            .value => switch (stack.getLast().*) {
                // remove the final trailing newline or space
                .scalar, .string => |*string| _ = string.popOrNull(),
                // if we have a dangling -, attach an empty string to it
                .list => |*list| if (expect_shift == .indent) try list.append(Value.newScalar(arena_alloc)),
                // if we have a dangling "key:", attach an empty string to it
                .map => |*map| if (dangling_key) |dk| try putMap(map, dk, Value.newScalar(arena_alloc), self.dupe_behavior),
                .flow_list, .flow_map => {},
            },
            .done => {},
        }

        return document;
    }

    const FlowStackItem = struct {
        value: *Value,
        // lists need this. maps do also for keys and values.
        item_start: usize = 0,
    };

    const FlowStack: type = std.ArrayList(FlowStackItem);

    inline fn getStackTip(stack: FlowStack) Error!*FlowStackItem {
        if (stack.items.len == 0) return error.BadState;
        return &stack.items[stack.items.len - 1];
    }

    inline fn setStackItemStart(stack: FlowStack, start: usize) Error!void {
        if (stack.items.len == 0) return error.BadState;
        stack.items[stack.items.len - 1].item_start = start;
    }

    inline fn popStack(stack: *FlowStack) Error!FlowParseState {
        if (stack.popOrNull() == null)
            return error.BadState;

        const parent = stack.getLastOrNull() orelse return .done;

        return switch (parent.value.*) {
            .flow_list => .want_list_separator,
            .flow_map => .want_map_separator,
            else => return error.BadState,
        };
    }

    const FlowParseState = enum {
        want_list_item,
        consuming_list_item,
        want_list_separator,
        want_map_key,
        consuming_map_key,
        want_map_value,
        consuming_map_value,
        want_map_separator,
        done,
    };

    pub fn parseFlow(
        alloc: std.mem.Allocator,
        contents: []const u8,
        root_type: Value.TagType,
        dupe_behavior: DuplicateKeyBehavior,
    ) Error!Value {
        // prime the stack:

        var root: Value = switch (root_type) {
            .flow_list => Value.newFlowList(alloc),
            .flow_map => Value.newFlowMap(alloc),
            else => return error.BadState,
        };
        var state: FlowParseState = switch (root_type) {
            .flow_list => .want_list_item,
            .flow_map => .want_map_key,
            else => unreachable,
        };
        var stack = try FlowStack.initCapacity(alloc, 1);
        stack.appendAssumeCapacity(.{ .value = &root });
        var dangling_key: ?[]const u8 = null;

        charloop: for (contents, 0..) |char, idx| {
            switch (state) {
                .want_list_item => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => {
                        // empty value
                        const tip = try getStackTip(stack);
                        try tip.value.flow_list.append(Value.newScalar(alloc));
                        tip.item_start = idx + 1;
                    },
                    '{' => {
                        const tip = try getStackTip(stack);

                        const new_map = try Parser.appendListGetValue(
                            &tip.value.flow_list,
                            Value.newFlowMap(alloc),
                        );

                        tip.item_start = idx;
                        try stack.append(.{ .value = new_map });
                        state = .want_map_key;
                    },
                    '[' => {
                        const tip = try getStackTip(stack);

                        const new_list = try Parser.appendListGetValue(
                            &tip.value.flow_list,
                            Value.newFlowList(alloc),
                        );

                        tip.item_start = idx;
                        try stack.append(.{ .value = new_list, .item_start = idx + 1 });
                        state = .want_list_item;
                    },
                    ']' => {
                        const finished = stack.getLastOrNull() orelse return error.BadState;
                        if (finished.value.flow_list.items.len > 0 or idx > finished.item_start)
                            try finished.value.flow_list.append(Value.newScalar(alloc));
                        state = try popStack(&stack);
                    },
                    else => {
                        try setStackItemStart(stack, idx);
                        state = .consuming_list_item;
                    },
                },
                .consuming_list_item => switch (char) {
                    ',' => {
                        const tip = try getStackTip(stack);

                        try tip.value.flow_list.append(
                            try Value.fromScalar(alloc, contents[tip.item_start..idx]),
                        );
                        tip.item_start = idx + 1;

                        state = .want_list_item;
                    },
                    ']' => {
                        const finished = stack.getLastOrNull() orelse return error.BadState;
                        try finished.value.flow_list.append(
                            try Value.fromScalar(alloc, contents[finished.item_start..idx]),
                        );
                        state = try popStack(&stack);
                    },
                    else => continue :charloop,
                },
                .want_list_separator => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => {
                        try setStackItemStart(stack, idx);
                        state = .want_list_item;
                    },
                    ']' => state = try popStack(&stack),
                    else => return error.BadToken,
                },
                .want_map_key => switch (char) {
                    ' ', '\t' => continue :charloop,
                    // forbid these characters so that flow dictionary keys cannot start
                    // with characters that regular dictionary keys cannot start with
                    // (even though they're unambiguous in this specific context).
                    '{', '[', '#', '-', '>', '|', ',' => return error.BadToken,
                    ':' => {
                        // we have an empty map key
                        dangling_key = "";
                        state = .want_map_value;
                    },
                    '}' => state = try popStack(&stack),
                    else => {
                        try setStackItemStart(stack, idx);
                        state = .consuming_map_key;
                    },
                },
                .consuming_map_key => switch (char) {
                    ':' => {
                        const tip = try getStackTip(stack);
                        dangling_key = try alloc.dupe(u8, contents[tip.item_start..idx]);

                        state = .want_map_value;
                    },
                    else => continue :charloop,
                },
                .want_map_value => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => {
                        const tip = try getStackTip(stack);
                        try Parser.putMap(
                            &tip.value.flow_map,
                            dangling_key.?,
                            Value.newScalar(alloc),
                            dupe_behavior,
                        );

                        dangling_key = null;
                        state = .want_map_key;
                    },
                    '[' => {
                        const tip = try getStackTip(stack);

                        const new_list = try Parser.putMapGetValue(
                            &tip.value.flow_map,
                            dangling_key.?,
                            Value.newFlowList(alloc),
                            dupe_behavior,
                        );

                        try stack.append(.{ .value = new_list, .item_start = idx + 1 });
                        dangling_key = null;
                        state = .want_list_item;
                    },
                    '{' => {
                        const tip = try getStackTip(stack);

                        const new_map = try Parser.putMapGetValue(
                            &tip.value.flow_map,
                            dangling_key.?,
                            Value.newFlowMap(alloc),
                            dupe_behavior,
                        );

                        try stack.append(.{ .value = new_map });
                        dangling_key = null;
                        state = .want_map_key;
                    },
                    '}' => {
                        // the value is an empty string and this map is closed
                        const tip = try getStackTip(stack);
                        try Parser.putMap(
                            &tip.value.flow_map,
                            dangling_key.?,
                            Value.newScalar(alloc),
                            dupe_behavior,
                        );

                        dangling_key = null;
                        state = try popStack(&stack);
                    },
                    else => {
                        try setStackItemStart(stack, idx);
                        state = .consuming_map_value;
                    },
                },
                .consuming_map_value => switch (char) {
                    ',', '}' => |term| {
                        const tip = try getStackTip(stack);
                        try Parser.putMap(
                            &tip.value.flow_map,
                            dangling_key.?,
                            try Value.fromScalar(alloc, contents[tip.item_start..idx]),
                            dupe_behavior,
                        );
                        dangling_key = null;
                        state = .want_map_key;
                        if (term == '}') state = try popStack(&stack);
                    },
                    else => continue :charloop,
                },
                .want_map_separator => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => state = .want_map_key,
                    '}' => state = try popStack(&stack),
                    else => return error.BadToken,
                },
                // the root value was closed but there are characters remaining
                // in the buffer
                .done => return error.BadState,
            }
        }
        // we ran out of characters while still in the middle of an object
        if (state != .done) return error.BadState;

        return root;
    }

    inline fn appendListGetValue(list: *Value.List, value: Value) Error!*Value {
        try list.append(value);
        return &list.items[list.items.len - 1];
    }

    inline fn putMap(map: *Value.Map, key: []const u8, value: Value, dupe_behavior: DuplicateKeyBehavior) Error!void {
        _ = try putMapGetValue(map, key, value, dupe_behavior);
    }

    inline fn putMapGetValue(map: *Value.Map, key: []const u8, value: Value, dupe_behavior: DuplicateKeyBehavior) Error!*Value {
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
