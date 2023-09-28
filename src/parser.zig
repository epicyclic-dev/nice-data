const std = @import("std");

const buffers = @import("./linebuffer.zig");
const tokenizer = @import("./tokenizer.zig");
const State = @import("./parser/state.zig").State;
pub const Document = @import("./parser/state.zig").Document;
pub const Value = @import("./parser/value.zig").Value;

pub const Diagnostics = struct {
    row: usize = 0,
    span: struct { absolute: usize = 0, line_offset: usize = 0, length: usize = 0 } = .{},
    message: []const u8 = "no problems",
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

pub const Options = struct {
    // If a mapping has multiple entries with the same key, this option defines how the
    // parser should behave. The default behavior is to emit an error if a repeated key
    // is encountered.
    duplicate_key_behavior: DuplicateKeyBehavior = .fail,

    // If an empty document is parsed, this defines what value type should be the
    // resulting document root object. The default behavior is to emit an error if the
    // document is empty.
    default_object: enum { string, list, map, fail } = .fail,
};

pub fn parseBuffer(allocator: std.mem.Allocator, buffer: []const u8, options: Options) !Document {
    var state = State.init(allocator);
    defer state.deinit();
    errdefer state.document.deinit();

    var diagnostics = Diagnostics{};
    var tok: tokenizer.LineTokenizer(buffers.ValidatingFixedLineBuffer) = .{
        .buffer = buffers.ValidatingFixedLineBuffer.init(buffer),
        .diagnostics = &diagnostics,
    };

    while (try tok.next()) |line| try state.parseLine(line, options.duplicate_key_behavior);
    // state doesn't have access to the tokenizer, which is the only thing that can
    // error if unparsed lines remain in the buffer by the time that "finish" is
    // called.
    try tok.finish();
    return try state.finish(options);
}

pub const StreamParser = struct {
    linetok: tokenizer.LineTokenizer(buffers.ValidatingLineBuffer),
    parse_state: State,
    parse_options: Options = .{},
    diagnostics: Diagnostics = .{},

    pub fn init(allocator: std.mem.Allocator, options: Options) !StreamParser {
        const diagnostics = try allocator.create(Diagnostics);
        errdefer allocator.destroy(diagnostics);
        diagnostics.* = Diagnostics{};

        return .{
            .linetok = .{
                .buffer = try buffers.ValidatingLineBuffer.init(allocator),
                .diagnostics = diagnostics,
            },
            .parse_state = State.init(allocator),
            .parse_options = options,
        };
    }

    pub fn deinit(self: StreamParser) void {
        self.linetok.buffer.allocator.destroy(self.linetok.diagnostics);
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
