const std = @import("std");

const buffers = @import("./linebuffer.zig");
const tokenizer = @import("./tokenizer.zig");
const Value = @import("./parser/value.zig").Value;

pub const Diagnostics = struct {
    row: usize,
    span: struct { absolute: usize, line_offset: usize, length: usize },
    message: []const u8,
};

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
} || tokenizer.Error || std.mem.Allocator.Error;

pub const DuplicateKeyBehavior = enum {
    use_first,
    use_last,
    fail,
};

pub const DefaultObject = enum {
    scalar,
    string,
    list,
    map,
    fail,
};

const ParseState = enum { initial, value, done };

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

pub const Parser = struct {
    allocator: std.mem.Allocator,
    dupe_behavior: DuplicateKeyBehavior = .fail,
    default_object: DefaultObject = .fail,
    diagnostics: Diagnostics = .{
        .row = 0,
        .span = .{ .absolute = 0, .line_offset = 0, .length = 0 },
        .message = "all is well",
    },

    pub const State = struct {
        pub const Stack = std.ArrayList(*Value);

        document: Document,
        value_stack: Stack,
        state: enum { initial, value, done } = .initial,
        expect_shift: tokenizer.ShiftDirection = .none,
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
        var expect_shift: tokenizer.ShiftDirection = .none;
        var dangling_key: ?[]const u8 = null;
        var stack = std.ArrayList(*Value).init(arena_alloc);
        defer stack.deinit();

        var tok: tokenizer.LineTokenizer(buffers.FixedLineBuffer) = .{
            .buffer = buffers.FixedLineBuffer.init(buffer),
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
                        if (line.shift == .indent) return error.UnexpectedIndent;

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
                                state = .value;

                                switch (value) {
                                    .empty => expect_shift = .indent,
                                    .scalar => |str| try document.root.list.append(try Value.fromScalar(arena_alloc, str)),
                                    .line_string, .space_string => |str| try document.root.list.append(try Value.fromString(arena_alloc, str)),
                                    .flow_list => |str| try document.root.list.append(try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior)),
                                    .flow_map => |str| try document.root.list.append(try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior)),
                                }
                            },
                            .map_item => |pair| {
                                document.root = Value.newMap(arena_alloc);
                                try stack.append(&document.root);
                                state = .value;

                                const dupekey = try arena_alloc.dupe(u8, pair.key);
                                switch (pair.val) {
                                    .empty => {
                                        expect_shift = .indent;
                                        // If the key is on its own line, we don't have
                                        // an associated value until we parse the next
                                        // line. We need to store a reference to this
                                        // key somewhere until we can consume the
                                        // value. More parser state to lug along.

                                        dangling_key = dupekey;
                                    },
                                    .scalar => |str| try document.root.map.put(dupekey, try Value.fromScalar(arena_alloc, str)),
                                    .line_string, .space_string => |str| try document.root.map.put(dupekey, try Value.fromString(arena_alloc, str)),
                                    .flow_list => |str| try document.root.map.put(dupekey, try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior)),
                                    .flow_map => |str| try document.root.map.put(dupekey, try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior)),
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
                            if (line.shift == .indent)
                                return error.UnexpectedIndent;

                            if (!flop and line.shift == .dedent) {
                                // kick off the last trailing space or newline
                                _ = string.pop();

                                var dedent_depth = line.shift.dedent;
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
                            if (!flop and (expect_shift == .indent and line.shift != .indent))
                                try list.append(Value.newScalar(arena_alloc));

                            // Consider:
                            //
                            //    -
                            //      own-line scalar
                            //    - inline scalar
                            //
                            // the own-line scalar will not push the stack but the next list item will be a dedent
                            if (!flop and line.shift == .dedent) {
                                // if line.shift.dedent is 1 and we're expecting it, the stack will not be popped,
                                // but we will continue loop flipflop. However, flop will be set to false on the next
                                // trip, so this if prong will not be run again.
                                var dedent_depth = line.shift.dedent - @intFromBool(expect_shift == .dedent);

                                while (dedent_depth > 0) : (dedent_depth -= 1)
                                    _ = stack.pop();

                                continue :flipflop;
                            }

                            switch (line.contents) {
                                .comment => unreachable,
                                .in_line => |in_line| {
                                    // assert that this line has been indented. this is required for an inline value when
                                    // the stack is in list mode.
                                    if (expect_shift != .indent or line.shift != .indent)
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
                                    if (flop or (line.shift == .none or line.shift == .dedent)) {
                                        expect_shift = .none;
                                        switch (value) {
                                            .empty => expect_shift = .indent,
                                            .scalar => |str| try list.append(try Value.fromScalar(arena_alloc, str)),
                                            .line_string, .space_string => |str| try list.append(try Value.fromString(arena_alloc, str)),
                                            .flow_list => |str| try list.append(try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior)),
                                            .flow_map => |str| try list.append(try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior)),
                                        }
                                    } else if (line.shift == .indent) {
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

                                    if (line.shift != .indent)
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
                            if (!flop and (expect_shift == .indent and line.shift != .indent)) {
                                try putMap(
                                    map,
                                    dangling_key orelse return error.Fail,
                                    Value.newScalar(arena_alloc),
                                    self.dupe_behavior,
                                );
                                dangling_key = null;
                            }

                            if (!flop and line.shift == .dedent) {
                                var dedent_depth = line.shift.dedent - @intFromBool(expect_shift == .dedent);

                                while (dedent_depth > 0) : (dedent_depth -= 1)
                                    _ = stack.pop();

                                continue :flipflop;
                            }

                            switch (line.contents) {
                                .comment => unreachable,
                                .in_line => |in_line| {
                                    // assert that this line has been indented. this is required for an inline value when
                                    // the stack is in map mode.
                                    if (expect_shift != .indent or line.shift != .indent or dangling_key == null)
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

                                    if (expect_shift != .indent or line.shift != .indent or dangling_key == null)
                                        return error.UnexpectedValue;

                                    const new_list = try putMapGetValue(map, dangling_key.?, Value.newList(arena_alloc), self.dupe_behavior);
                                    try stack.append(new_list);
                                    dangling_key = null;
                                    expect_shift = .none;
                                    continue :flipflop;
                                },
                                .map_item => |pair| {
                                    if (flop or (line.shift == .none or line.shift == .dedent)) {
                                        expect_shift = .none;
                                        const dupekey = try arena_alloc.dupe(u8, pair.key);
                                        switch (pair.val) {
                                            .empty => {
                                                expect_shift = .indent;
                                                dangling_key = dupekey;
                                            },
                                            .scalar => |str| try putMap(map, dupekey, try Value.fromScalar(arena_alloc, str), self.dupe_behavior),
                                            .line_string, .space_string => |str| try putMap(map, dupekey, try Value.fromString(arena_alloc, str), self.dupe_behavior),
                                            .flow_list => |str| try putMap(map, dupekey, try parseFlow(arena_alloc, str, .flow_list, self.dupe_behavior), self.dupe_behavior),
                                            .flow_map => |str| try putMap(map, dupekey, try parseFlow(arena_alloc, str, .flow_map, self.dupe_behavior), self.dupe_behavior),
                                        }
                                    } else if (line.shift == .indent) {
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
                .scalar => document.root = .{ .scalar = std.ArrayList(u8).init(arena_alloc) },
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

    const FlowStack: type = std.ArrayList(*Value);

    inline fn getStackTip(stack: FlowStack) Error!*Value {
        if (stack.items.len == 0) return error.BadState;
        return stack.items[stack.items.len - 1];
    }

    inline fn popStack(stack: *FlowStack) Error!FlowParseState {
        if (stack.popOrNull() == null)
            return error.BadState;

        const parent = stack.getLastOrNull() orelse return .done;

        return switch (parent.*) {
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
        stack.appendAssumeCapacity(&root);
        // used to distinguish betwen [] and [ ], and it also tracks
        // a continuous value between different states
        var item_start: usize = 0;
        var dangling_key: ?[]const u8 = null;

        charloop: for (contents, 0..) |char, idx| {
            switch (state) {
                .want_list_item => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => {
                        // empty value
                        const tip = try getStackTip(stack);
                        try tip.flow_list.append(Value.newScalar(alloc));
                        item_start = idx + 1;
                    },
                    '{' => {
                        const tip = try getStackTip(stack);

                        const new_map = try Parser.appendListGetValue(
                            &tip.flow_list,
                            Value.newFlowMap(alloc),
                        );

                        item_start = idx;
                        try stack.append(new_map);
                        state = .want_map_key;
                    },
                    '[' => {
                        const tip = try getStackTip(stack);

                        const new_list = try Parser.appendListGetValue(
                            &tip.flow_list,
                            Value.newFlowList(alloc),
                        );

                        item_start = idx + 1;
                        try stack.append(new_list);
                        state = .want_list_item;
                    },
                    ']' => {
                        const finished = stack.getLastOrNull() orelse return error.BadState;
                        if (finished.flow_list.items.len > 0 or idx > item_start)
                            try finished.flow_list.append(Value.newScalar(alloc));
                        state = try popStack(&stack);
                    },
                    else => {
                        item_start = idx;
                        state = .consuming_list_item;
                    },
                },
                .consuming_list_item => switch (char) {
                    ',' => {
                        const tip = try getStackTip(stack);

                        try tip.flow_list.append(
                            try Value.fromScalar(alloc, contents[item_start..idx]),
                        );
                        item_start = idx + 1;

                        state = .want_list_item;
                    },
                    ']' => {
                        const finished = stack.getLastOrNull() orelse return error.BadState;
                        try finished.flow_list.append(
                            try Value.fromScalar(alloc, contents[item_start..idx]),
                        );
                        state = try popStack(&stack);
                    },
                    else => continue :charloop,
                },
                .want_list_separator => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => {
                        item_start = idx;
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
                        item_start = idx;
                        state = .consuming_map_key;
                    },
                },
                .consuming_map_key => switch (char) {
                    ':' => {
                        dangling_key = try alloc.dupe(u8, contents[item_start..idx]);
                        state = .want_map_value;
                    },
                    else => continue :charloop,
                },
                .want_map_value => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => {
                        const tip = try getStackTip(stack);
                        try Parser.putMap(
                            &tip.flow_map,
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
                            &tip.flow_map,
                            dangling_key.?,
                            Value.newFlowList(alloc),
                            dupe_behavior,
                        );

                        try stack.append(new_list);
                        dangling_key = null;
                        item_start = idx + 1;
                        state = .want_list_item;
                    },
                    '{' => {
                        const tip = try getStackTip(stack);

                        const new_map = try Parser.putMapGetValue(
                            &tip.flow_map,
                            dangling_key.?,
                            Value.newFlowMap(alloc),
                            dupe_behavior,
                        );

                        try stack.append(new_map);
                        dangling_key = null;
                        state = .want_map_key;
                    },
                    '}' => {
                        // the value is an empty string and this map is closed
                        const tip = try getStackTip(stack);
                        try Parser.putMap(
                            &tip.flow_map,
                            dangling_key.?,
                            Value.newScalar(alloc),
                            dupe_behavior,
                        );

                        dangling_key = null;
                        state = try popStack(&stack);
                    },
                    else => {
                        item_start = idx;
                        state = .consuming_map_value;
                    },
                },
                .consuming_map_value => switch (char) {
                    ',', '}' => |term| {
                        const tip = try getStackTip(stack);
                        try Parser.putMap(
                            &tip.flow_map,
                            dangling_key.?,
                            try Value.fromScalar(alloc, contents[item_start..idx]),
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
};
