const std = @import("std");

const buffers = @import("./linebuffer.zig");
const tokenizer = @import("./tokenizer.zig");
const State = @import("./parser/state.zig").State;
pub const Document = @import("./parser/value.zig").Document;
pub const Parsed = @import("./parser/value.zig").Parsed;
pub const Value = @import("./parser/value.zig").Value;

pub const Diagnostics = struct {
    row: usize = 0,
    line_offset: usize = 0,
    length: usize = 0,
    message: []const u8 = "no problems",
};

pub const Error = error{
    UnexpectedIndent,
    UnexpectedValue,
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

pub const Options = struct {
    // If a mapping has multiple entries with the same key, this option defines how the
    // parser should behave. The default behavior is to emit an error if a repeated key
    // is encountered.
    duplicate_key_behavior: DuplicateKeyBehavior = .fail,

    // If an empty document is parsed, this defines what value type should be the
    // resulting document root object. The default behavior is to emit an error if the
    // document is empty.
    default_object: enum { string, list, map, fail } = .fail,

    // Only used by the parseTo family of functions.
    // If false, and a mapping contains additional keys that do not map to the fields of
    // the corresponding object, an error will be raised. By default, additional keys
    // will be skipped and no error will be raised. Note that tagged unions must be
    // represented by a map with a single key, and having more than one key will always
    // be an error, even if this option is set to true.
    ignore_extra_fields: bool = true,

    // Only used by the parseTo family of functions.
    // If true, if a struct field is an optional type and the corresponding mapping key
    // does not exist, the object field will be set to `null`. By default, if the
    // parsed document is missing a mapping key for a given field, an error will be
    // raised instead.
    treat_omitted_as_null: bool = false,

    // Only used by the parseTo family of functions.
    // If true, strings may be coerced into other scalar types, like booleans or
    // numbers. By default, only document scalar fields will attempt to coerce to
    // non-string values.
    coerce_strings: bool = false,

    // Only used by the parseTo family of functions.
    // Two lists of strings. Truthy strings will be parsed to boolean true. Falsy
    // strings will be parsed to boolean  false. All other strings will raise an
    // error.
    boolean_strings: struct { truthy: []const []const u8, falsy: []const []const u8 } = .{
        .truthy = &.{ "true", "True", "yes", "on" },
        .falsy = &.{ "false", "False", "no", "off" },
    },

    null_strings: []const []const u8 = &.{ "null", "nil", "None" },

    // Only used by the parseTo family of functions.
    // If true, document scalars that appear to be numbers will attempt to convert into
    // enum values as an integer. By default, all enums in the document must be
    // specified by name, not by numeric value. Note that conversion by name will always
    // be tried first, even if this option is enabled, so if you're stupid enough to do:
    //
    //     const Horrible = enum {
    //         @"1" = 0,
    //         @"0" = 1,
    //     };
    //
    // then you deserve what you get. And what you'll get is confusing results.
    // Also note that this option does not apply to tagged unions, despite those being
    // backed by possibly ordered enums.
    allow_numeric_enums: bool = false,
};

pub fn parseBuffer(
    allocator: std.mem.Allocator,
    buffer: []const u8,
    diagnostics: *Diagnostics,
    options: Options,
) !Document {
    var state = State.init(allocator, diagnostics);
    defer state.deinit();
    errdefer state.document.deinit();

    var tok: tokenizer.LineTokenizer(buffers.ValidatingFixedLineBuffer) = .{
        .buffer = buffers.ValidatingFixedLineBuffer.init(buffer, diagnostics),
    };

    while (try tok.next()) |line| try state.parseLine(line, options.duplicate_key_behavior);
    // state doesn't have access to the tokenizer, which is the only thing that can
    // error if unparsed lines remain in the buffer by the time that "finish" is
    // called.
    try tok.finish();
    return try state.finish(options);
}

pub fn parseBufferTo(
    comptime T: type,
    allocator: std.mem.Allocator,
    buffer: []const u8,
    diagnostics: *Diagnostics,
    options: Options,
) !Parsed(T) {
    var doc = try parseBuffer(allocator, buffer, diagnostics, options);
    return try doc.convertTo(T, options);
}

pub const StreamParser = struct {
    linetok: tokenizer.LineTokenizer(buffers.ValidatingLineBuffer),
    parse_state: State,
    parse_options: Options = .{},

    pub fn init(allocator: std.mem.Allocator, options: Options) !StreamParser {
        const diagnostics = try allocator.create(Diagnostics);
        errdefer allocator.destroy(diagnostics);
        diagnostics.* = Diagnostics{};

        return .{
            .linetok = .{
                .buffer = try buffers.ValidatingLineBuffer.init(allocator, diagnostics),
            },
            .parse_state = State.init(allocator, diagnostics),
            .parse_options = options,
        };
    }

    pub fn deinit(self: StreamParser) void {
        self.linetok.buffer.allocator.destroy(self.parse_state.diagnostics);
        self.linetok.buffer.deinit();
        self.parse_state.deinit();
    }

    pub fn feed(self: *StreamParser, data: []const u8) !void {
        try self.linetok.buffer.feed(data);
        while (try self.linetok.next()) |line| try self.parse_state.parseLine(line, self.parse_options.duplicate_key_behavior);
    }

    pub fn finish(self: *StreamParser) !Document {
        try self.linetok.finish();
        return try self.parse_state.finish(self.parse_options);
    }
};