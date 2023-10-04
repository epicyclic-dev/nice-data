const std = @import("std");

const tokenizer = @import("../tokenizer.zig");
const Error = @import("../parser.zig").Error;
const DuplicateKeyBehavior = @import("../parser.zig").DuplicateKeyBehavior;
const Options = @import("../parser.zig").Options;
const Diagnostics = @import("../parser.zig").Diagnostics;
const Document = @import("./value.zig").Document;
const Value = @import("./value.zig").Value;

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

pub const State = struct {
    pub const Stack = std.ArrayList(*Value);

    document: Document,
    diagnostics: *Diagnostics,
    value_stack: Stack,
    mode: enum { initial, value, done } = .initial,
    expect_shift: tokenizer.ShiftDirection = .none,
    dangling_key: ?[]const u8 = null,

    pub fn init(allocator: std.mem.Allocator, diagnostics: *Diagnostics) State {
        return .{
            .document = Document.init(allocator),
            .diagnostics = diagnostics,
            .value_stack = Stack.init(allocator),
        };
    }

    pub fn deinit(self: State) void {
        self.value_stack.deinit();
    }

    pub fn finish(state: *State, options: Options) Error!Document {
        const arena_alloc = state.document.arena.allocator();

        switch (state.mode) {
            .initial => switch (options.default_object) {
                .string => state.document.root = Value.newString(arena_alloc),
                .list => state.document.root = Value.newList(arena_alloc),
                .map => state.document.root = Value.newMap(arena_alloc),
                .fail => {
                    state.diagnostics.length = 0;
                    state.diagnostics.message = "the document is empty";
                    return error.EmptyDocument;
                },
            },
            .value => switch (state.value_stack.getLast().*) {
                // remove the final trailing newline or space
                .string => |*string| _ = string.popOrNull(),
                // if we have a dangling -, attach an empty string to it
                .list => |*list| if (state.expect_shift == .indent) try list.append(Value.newScalar(arena_alloc)),
                // if we have a dangling "key:", attach an empty string to it
                .map => |*map| if (state.dangling_key) |dk| try state.putMap(
                    map,
                    dk,
                    Value.newScalar(arena_alloc),
                    options.duplicate_key_behavior,
                ),
                .scalar, .flow_list, .flow_map => {},
            },
            .done => {},
        }

        return state.document;
    }

    pub fn parseLine(state: *State, line: tokenizer.Line, dkb: DuplicateKeyBehavior) Error!void {
        if (line.contents == .comment) return;

        // this gives us a second loop when the stack tip changes (i.e. during dedent or
        // some indents (not all indents push the stack))
        const arena_alloc = state.document.arena.allocator();
        var firstpass = true;
        restack: while (true) : (firstpass = false) {
            switch (state.mode) {
                .initial => {
                    if (line.shift == .indent) {
                        state.diagnostics.length = 1;
                        state.diagnostics.message = "the first object in the document cannot be indented";
                        return error.UnexpectedIndent;
                    }

                    switch (line.contents) {
                        // we filter out comments above
                        .comment => unreachable,
                        .in_line => |in_line| switch (in_line) {
                            // empty scalars are only emitted for a list_item or a map_item
                            .empty => unreachable,
                            .scalar => |str| {
                                state.document.root = try Value.fromScalar(arena_alloc, str);
                                state.mode = .done;
                            },
                            .line_string, .space_string => |str| {
                                state.document.root = try Value.fromString(arena_alloc, str);
                                try state.document.root.string.append(in_line.lineEnding());
                                try state.value_stack.append(&state.document.root);
                                state.mode = .value;
                            },
                            .flow_list => |str| {
                                state.document.root = try state.parseFlow(str, .flow_list, dkb);
                                state.mode = .done;
                            },
                            .flow_map => |str| {
                                state.document.root = try state.parseFlow(str, .flow_map, dkb);
                                state.mode = .done;
                            },
                        },
                        .list_item => |value| {
                            state.document.root = Value.newList(arena_alloc);
                            try state.value_stack.append(&state.document.root);
                            state.mode = .value;

                            const rootlist = &state.document.root.list;
                            switch (value) {
                                .empty => state.expect_shift = .indent,
                                .scalar => |str| try rootlist.append(try Value.fromScalar(arena_alloc, str)),
                                .line_string, .space_string => |str| try rootlist.append(try Value.fromString(arena_alloc, str)),
                                .flow_list => |str| try rootlist.append(try state.parseFlow(str, .flow_list, dkb)),
                                .flow_map => |str| try rootlist.append(try state.parseFlow(str, .flow_map, dkb)),
                            }
                        },
                        .map_item => |pair| {
                            state.document.root = Value.newMap(arena_alloc);
                            try state.value_stack.append(&state.document.root);
                            state.mode = .value;

                            const rootmap = &state.document.root.map;
                            const dupekey = try arena_alloc.dupe(u8, pair.key);
                            switch (pair.val) {
                                .empty => {
                                    state.expect_shift = .indent;
                                    state.dangling_key = dupekey;
                                },
                                .scalar => |str| try rootmap.put(dupekey, try Value.fromScalar(arena_alloc, str)),
                                .line_string, .space_string => |str| try rootmap.put(dupekey, try Value.fromString(arena_alloc, str)),
                                .flow_list => |str| try rootmap.put(dupekey, try state.parseFlow(str, .flow_list, dkb)),
                                .flow_map => |str| try rootmap.put(dupekey, try state.parseFlow(str, .flow_map, dkb)),
                            }
                        },
                    }
                },
                .value => switch (state.value_stack.getLast().*) {
                    // these three states are never reachable here. flow_list and
                    // flow_map are parsed with a separate state machine. These
                    // value types can only be present by themselves as the first
                    // line of the document, in which case the document consists
                    // only of that single line: this parser jumps immediately into
                    // the .done state, bypassing the .value state in which this
                    // switch is embedded.
                    .scalar, .flow_list, .flow_map => return error.Fail,
                    .string => |*string| {
                        if (line.shift == .indent) {
                            state.diagnostics.length = 1;
                            state.diagnostics.message = "the document contains an invalid indented line in a multiline string";
                            return error.UnexpectedIndent;
                        }

                        if (firstpass and line.shift == .dedent) {
                            // kick off the last trailing space or newline
                            _ = string.pop();

                            var dedent_depth = line.shift.dedent;
                            while (dedent_depth > 0) : (dedent_depth -= 1)
                                _ = state.value_stack.pop();

                            continue :restack;
                        }

                        switch (line.contents) {
                            .comment => unreachable,
                            .in_line => |in_line| switch (in_line) {
                                .empty => unreachable,
                                .line_string, .space_string => |str| {
                                    try string.appendSlice(str);
                                    try string.append(in_line.lineEnding());
                                },
                                else => {
                                    state.diagnostics.length = 1;
                                    state.diagnostics.message = "the document contains an invalid object in a multiline string";
                                    return error.UnexpectedValue;
                                },
                            },
                            else => {
                                state.diagnostics.length = 1;
                                state.diagnostics.message = "the document contains an invalid object in a multiline string";
                                return error.UnexpectedValue;
                            },
                        }
                    },
                    .list => |*list| {
                        // detect that the previous item was actually empty
                        //
                        //    -
                        //    - something
                        //
                        // the first line here creates the state.expect_shift, but the second line
                        // is a valid continuation of the list despite not being indented
                        if (firstpass and (state.expect_shift == .indent and line.shift != .indent))
                            try list.append(Value.newScalar(arena_alloc));

                        // Consider:
                        //
                        //    -
                        //      own-line scalar
                        //    - inline scalar
                        //
                        // the own-line scalar will not push the stack but the next list item will be a dedent
                        if (firstpass and line.shift == .dedent) {
                            // if line.shift.dedent is 1 and we're expecting it, the stack will not be popped,
                            // but we will continue restack. However, firstpass will be set to false on the next
                            // trip, so this if prong will not be run again.
                            var dedent_depth = line.shift.dedent - @intFromBool(state.expect_shift == .dedent);

                            while (dedent_depth > 0) : (dedent_depth -= 1)
                                _ = state.value_stack.pop();

                            continue :restack;
                        }

                        switch (line.contents) {
                            .comment => unreachable,
                            .in_line => |in_line| {
                                // assert that this line has been indented and that indentation is expected.
                                if (state.expect_shift != .indent or line.shift != .indent) {
                                    state.diagnostics.length = 1;
                                    state.diagnostics.message = "the document contains an invalid inline object in a list";
                                    return error.UnexpectedValue;
                                }

                                state.expect_shift = .dedent;
                                switch (in_line) {
                                    .empty => unreachable,
                                    .scalar => |str| try list.append(try Value.fromScalar(arena_alloc, str)),
                                    .flow_list => |str| try list.append(try state.parseFlow(str, .flow_list, dkb)),
                                    .flow_map => |str| try list.append(try state.parseFlow(str, .flow_map, dkb)),
                                    .line_string, .space_string => |str| {
                                        const new_string = try appendListGetValue(list, try Value.fromString(arena_alloc, str));
                                        try new_string.string.append(in_line.lineEnding());
                                        try state.value_stack.append(new_string);
                                        state.expect_shift = .none;
                                    },
                                }
                            },
                            .list_item => |value| {
                                if (!firstpass or (line.shift == .none or line.shift == .dedent)) {
                                    state.expect_shift = .none;
                                    switch (value) {
                                        .empty => state.expect_shift = .indent,
                                        .scalar => |str| try list.append(try Value.fromScalar(arena_alloc, str)),
                                        .line_string, .space_string => |str| try list.append(try Value.fromString(arena_alloc, str)),
                                        .flow_list => |str| try list.append(try state.parseFlow(str, .flow_list, dkb)),
                                        .flow_map => |str| try list.append(try state.parseFlow(str, .flow_map, dkb)),
                                    }
                                } else if (line.shift == .indent) {
                                    if (state.expect_shift != .indent) return error.UnexpectedIndent;

                                    const new_list = try appendListGetValue(list, Value.newList(arena_alloc));
                                    try state.value_stack.append(new_list);
                                    state.expect_shift = .none;
                                    continue :restack;
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

                                if (state.expect_shift != .indent or line.shift != .indent) {
                                    state.diagnostics.length = 1;
                                    state.diagnostics.message = "the document contains an invalid map key in a list";
                                    return error.UnexpectedValue;
                                }

                                const new_map = try appendListGetValue(list, Value.newMap(arena_alloc));
                                try state.value_stack.append(new_map);
                                state.expect_shift = .none;
                                continue :restack;
                            },
                        }
                    },
                    .map => |*map| {
                        // detect that the previous item was actually empty
                        //
                        //    foo:
                        //    bar: baz
                        //
                        // the first line here creates the state.expect_shift, but the second line
                        // is a valid continuation of the map despite not being indented
                        if (firstpass and (state.expect_shift == .indent and line.shift != .indent)) {
                            try state.putMap(
                                map,
                                state.dangling_key orelse {
                                    state.diagnostics.length = 1;
                                    state.diagnostics.message = "the document is somehow missing a key (this shouldn't be possible)";
                                    return error.Fail;
                                },
                                Value.newScalar(arena_alloc),
                                dkb,
                            );
                            state.dangling_key = null;
                        }

                        if (firstpass and line.shift == .dedent) {
                            var dedent_depth = line.shift.dedent - @intFromBool(state.expect_shift == .dedent);

                            while (dedent_depth > 0) : (dedent_depth -= 1)
                                _ = state.value_stack.pop();

                            continue :restack;
                        }

                        switch (line.contents) {
                            .comment => unreachable,
                            .in_line => |in_line| {
                                // assert that this line has been indented. this is required for an inline value when
                                // the stack is in map mode.
                                if (state.expect_shift != .indent or line.shift != .indent or state.dangling_key == null) {
                                    state.diagnostics.length = 1;
                                    state.diagnostics.message = "the document contains an invalid inline object in a map";
                                    return error.UnexpectedValue;
                                }

                                state.expect_shift = .dedent;

                                switch (in_line) {
                                    .empty => unreachable,
                                    .scalar => |str| try state.putMap(map, state.dangling_key.?, try Value.fromScalar(arena_alloc, str), dkb),
                                    .flow_list => |str| try state.putMap(map, state.dangling_key.?, try state.parseFlow(str, .flow_list, dkb), dkb),
                                    .flow_map => |str| {
                                        try state.putMap(map, state.dangling_key.?, try state.parseFlow(str, .flow_map, dkb), dkb);
                                    },
                                    .line_string, .space_string => |str| {
                                        // string pushes the stack
                                        const new_string = try state.putMapGetValue(map, state.dangling_key.?, try Value.fromString(arena_alloc, str), dkb);
                                        try new_string.string.append(in_line.lineEnding());
                                        try state.value_stack.append(new_string);
                                        state.expect_shift = .none;
                                    },
                                }

                                state.dangling_key = null;
                            },
                            .list_item => {
                                // this prong cannot be hit on dedent in a valid way.
                                //
                                //    map:
                                //      - value
                                //    - invalid
                                //
                                // dedenting back to the map stack level requires map_item

                                if (state.expect_shift != .indent or line.shift != .indent or state.dangling_key == null) {
                                    state.diagnostics.length = 1;
                                    state.diagnostics.message = "the document contains an invalid list item in a map";
                                    return error.UnexpectedValue;
                                }

                                const new_list = try state.putMapGetValue(map, state.dangling_key.?, Value.newList(arena_alloc), dkb);
                                try state.value_stack.append(new_list);
                                state.dangling_key = null;
                                state.expect_shift = .none;
                                continue :restack;
                            },
                            .map_item => |pair| {
                                if (!firstpass or (line.shift == .none or line.shift == .dedent)) {
                                    state.expect_shift = .none;
                                    const dupekey = try arena_alloc.dupe(u8, pair.key);
                                    switch (pair.val) {
                                        .empty => {
                                            state.expect_shift = .indent;
                                            state.dangling_key = dupekey;
                                        },
                                        .scalar => |str| try state.putMap(map, dupekey, try Value.fromScalar(arena_alloc, str), dkb),
                                        .line_string, .space_string => |str| try state.putMap(map, dupekey, try Value.fromString(arena_alloc, str), dkb),
                                        .flow_list => |str| try state.putMap(map, dupekey, try state.parseFlow(str, .flow_list, dkb), dkb),
                                        .flow_map => |str| try state.putMap(map, dupekey, try state.parseFlow(str, .flow_map, dkb), dkb),
                                    }
                                } else if (line.shift == .indent) {
                                    if (state.expect_shift != .indent or state.dangling_key == null) {
                                        state.diagnostics.length = 1;
                                        state.diagnostics.message = "the document contains indented map item in a map";
                                        return error.UnexpectedValue;
                                    }

                                    const new_map = try state.putMapGetValue(map, state.dangling_key.?, Value.newMap(arena_alloc), dkb);
                                    try state.value_stack.append(new_map);
                                    state.dangling_key = null;
                                    continue :restack;
                                } else unreachable;
                            },
                        }
                    },
                },
                .done => {
                    state.diagnostics.length = 1;
                    state.diagnostics.message = "the document contains extra data after the top level structure";
                    return error.ExtraContent;
                },
            }

            // the stack has not changed, so break the loop
            break :restack;
        }
    }

    pub fn parseFlow(
        state: *State,
        contents: []const u8,
        root_type: Value.TagType,
        dkb: DuplicateKeyBehavior,
    ) Error!Value {
        const arena_alloc = state.document.arena.allocator();

        var root: Value = switch (root_type) {
            .flow_list => Value.newFlowList(arena_alloc),
            .flow_map => Value.newFlowMap(arena_alloc),
            else => {
                state.diagnostics.length = 1;
                state.diagnostics.message = "the flow item was closed too many times";
                return error.BadState;
            },
        };
        var pstate: FlowParseState = switch (root_type) {
            .flow_list => .want_list_item,
            .flow_map => .want_map_key,
            else => unreachable,
        };

        // used to distinguish betwen [] and [ ], and it also tracks
        // a continuous value between different states
        var item_start: usize = 0;
        var dangling_key: ?[]const u8 = null;
        try state.value_stack.append(&root);

        charloop: for (contents, 0..) |char, idx| {
            switch (pstate) {
                .want_list_item => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => {
                        // empty value
                        const tip = try state.getStackTip();
                        try tip.flow_list.append(Value.newScalar(arena_alloc));
                        item_start = idx + 1;
                    },
                    '{' => {
                        const tip = try state.getStackTip();

                        const new_map = try appendListGetValue(
                            &tip.flow_list,
                            Value.newFlowMap(arena_alloc),
                        );

                        item_start = idx;
                        try state.value_stack.append(new_map);
                        pstate = .want_map_key;
                    },
                    '[' => {
                        const tip = try state.getStackTip();

                        const new_list = try appendListGetValue(
                            &tip.flow_list,
                            Value.newFlowList(arena_alloc),
                        );

                        item_start = idx + 1;
                        try state.value_stack.append(new_list);
                        pstate = .want_list_item;
                    },
                    ']' => {
                        const finished = state.value_stack.getLastOrNull() orelse {
                            state.diagnostics.length = 1;
                            state.diagnostics.message = "the flow list was closed too many times";
                            return error.BadState;
                        };
                        if (finished.flow_list.items.len > 0 or idx > item_start)
                            try finished.flow_list.append(Value.newScalar(arena_alloc));
                        pstate = try state.popFlowStack();
                    },
                    else => {
                        item_start = idx;
                        pstate = .consuming_list_item;
                    },
                },
                .consuming_list_item => switch (char) {
                    ',' => {
                        const tip = try state.getStackTip();

                        try tip.flow_list.append(
                            try Value.fromScalar(arena_alloc, contents[item_start..idx]),
                        );
                        item_start = idx + 1;

                        pstate = .want_list_item;
                    },
                    ']' => {
                        const finished = state.value_stack.getLastOrNull() orelse {
                            state.diagnostics.length = 1;
                            state.diagnostics.message = "the flow list was closed too many times";
                            return error.BadState;
                        };
                        try finished.flow_list.append(
                            try Value.fromScalar(arena_alloc, contents[item_start..idx]),
                        );
                        pstate = try state.popFlowStack();
                    },
                    else => continue :charloop,
                },
                .want_list_separator => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => {
                        item_start = idx;
                        pstate = .want_list_item;
                    },
                    ']' => pstate = try state.popFlowStack(),
                    else => return {
                        state.diagnostics.length = 1;
                        state.diagnostics.message = "the document contains an invalid flow list separator";
                        return error.BadToken;
                    },
                },
                .want_map_key => switch (char) {
                    ' ', '\t' => continue :charloop,
                    // forbid these characters so that flow dictionary keys cannot start
                    // with characters that regular dictionary keys cannot start with
                    // (even though they're unambiguous in this specific context).
                    '{', '[', '#', '-', '>', '|', ',' => return {
                        state.diagnostics.length = 1;
                        state.diagnostics.message = "this document contains a flow map key that starts with an invalid character";
                        return error.BadToken;
                    },
                    ':' => {
                        // we have an empty map key
                        dangling_key = "";
                        pstate = .want_map_value;
                    },
                    '}' => pstate = try state.popFlowStack(),
                    else => {
                        item_start = idx;
                        pstate = .consuming_map_key;
                    },
                },
                .consuming_map_key => switch (char) {
                    ':' => {
                        dangling_key = try arena_alloc.dupe(u8, contents[item_start..idx]);
                        pstate = .want_map_value;
                    },
                    else => continue :charloop,
                },
                .want_map_value => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => {
                        const tip = try state.getStackTip();
                        try state.putMap(
                            &tip.flow_map,
                            dangling_key.?,
                            Value.newScalar(arena_alloc),
                            dkb,
                        );

                        dangling_key = null;
                        pstate = .want_map_key;
                    },
                    '[' => {
                        const tip = try state.getStackTip();

                        const new_list = try state.putMapGetValue(
                            &tip.flow_map,
                            dangling_key.?,
                            Value.newFlowList(arena_alloc),
                            dkb,
                        );

                        try state.value_stack.append(new_list);
                        dangling_key = null;
                        item_start = idx + 1;
                        pstate = .want_list_item;
                    },
                    '{' => {
                        const tip = try state.getStackTip();

                        const new_map = try state.putMapGetValue(
                            &tip.flow_map,
                            dangling_key.?,
                            Value.newFlowMap(arena_alloc),
                            dkb,
                        );

                        try state.value_stack.append(new_map);
                        dangling_key = null;
                        pstate = .want_map_key;
                    },
                    '}' => {
                        // the value is an empty string and this map is closed
                        const tip = try state.getStackTip();
                        try state.putMap(
                            &tip.flow_map,
                            dangling_key.?,
                            Value.newScalar(arena_alloc),
                            dkb,
                        );

                        dangling_key = null;
                        pstate = try state.popFlowStack();
                    },
                    else => {
                        item_start = idx;
                        pstate = .consuming_map_value;
                    },
                },
                .consuming_map_value => switch (char) {
                    ',', '}' => |term| {
                        const tip = try state.getStackTip();
                        try state.putMap(
                            &tip.flow_map,
                            dangling_key.?,
                            try Value.fromScalar(arena_alloc, contents[item_start..idx]),
                            dkb,
                        );
                        dangling_key = null;
                        pstate = .want_map_key;
                        if (term == '}') pstate = try state.popFlowStack();
                    },
                    else => continue :charloop,
                },
                .want_map_separator => switch (char) {
                    ' ', '\t' => continue :charloop,
                    ',' => pstate = .want_map_key,
                    '}' => pstate = try state.popFlowStack(),
                    else => return {
                        state.diagnostics.length = 1;
                        state.diagnostics.message = "this document contains an invalid character instead of a flow map separator";
                        return error.BadToken;
                    },
                },
                // the root value was closed but there are characters remaining
                // in the buffer
                .done => return {
                    state.diagnostics.length = 1;
                    state.diagnostics.message = "this document extra data after single item";
                    return error.BadState;
                },
            }
        }
        // we ran out of characters while still in the middle of an object
        if (pstate != .done) return {
            state.diagnostics.length = 1;
            state.diagnostics.message = "this document contains an unterminated flow item";
            return error.BadState;
        };

        return root;
    }

    inline fn getStackTip(state: State) Error!*Value {
        if (state.value_stack.items.len == 0) return {
            state.diagnostics.length = 1;
            state.diagnostics.message = "this document contains an unexpected bottom of the stack";
            return error.BadState;
        };
        return state.value_stack.items[state.value_stack.items.len - 1];
    }

    inline fn popFlowStack(state: *State) Error!FlowParseState {
        if (state.value_stack.popOrNull() == null) {
            state.diagnostics.length = 1;
            state.diagnostics.message = "this document contains an unexpected bottom of the stack";
            return error.BadState;
        }
        const parent = state.value_stack.getLastOrNull() orelse return .done;

        return switch (parent.*) {
            .flow_list => .want_list_separator,
            .flow_map => .want_map_separator,
            else => .done,
        };
    }

    inline fn appendListGetValue(list: *Value.List, value: Value) Error!*Value {
        try list.append(value);
        return &list.items[list.items.len - 1];
    }

    inline fn putMap(state: *State, map: *Value.Map, key: []const u8, value: Value, dkb: DuplicateKeyBehavior) Error!void {
        _ = try state.putMapGetValue(map, key, value, dkb);
    }

    inline fn putMapGetValue(state: *State, map: *Value.Map, key: []const u8, value: Value, dkb: DuplicateKeyBehavior) Error!*Value {
        const gop = try map.getOrPut(key);

        if (gop.found_existing)
            switch (dkb) {
                .fail => {
                    state.diagnostics.length = 1;
                    state.diagnostics.message = "this document contains a duplicate key";
                    return error.DuplicateKey;
                },
                .use_first => {},
                .use_last => gop.value_ptr.* = value,
            }
        else
            gop.value_ptr.* = value;

        return gop.value_ptr;
    }
};
