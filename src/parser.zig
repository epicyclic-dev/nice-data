const std = @import("std");

const buffers = @import("./linebuffer.zig");
const tokenizer = @import("./tokenizer.zig");
const Value = @import("./parser/value.zig").Value;
const State = @import("./parser/state.zig").State;
const Document = @import("./parser/state.zig").Document;

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

pub const Parser = struct {
    allocator: std.mem.Allocator,
    options: Options = .{},
    diagnostics: Diagnostics = .{
        .row = 0,
        .span = .{ .absolute = 0, .line_offset = 0, .length = 0 },
        .message = "all is well",
    },

    pub fn parseBuffer(self: *Parser, buffer: []const u8) Error!Document {
        var tok: tokenizer.LineTokenizer(buffers.FixedLineBuffer) = .{
            .buffer = buffers.FixedLineBuffer.init(buffer),
            .diagnostics = &self.diagnostics,
        };

        var state = State.init(self.allocator);
        defer state.deinit();
        errdefer state.document.deinit();

        // TODO: pass the diagnostics pointer as well
        while (try tok.next()) |line| try state.parseLine(line, self.options.duplicate_key_behavior);

        return try state.finish(self.options);
    }
};
