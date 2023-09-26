const std = @import("std");

const Diagnostics = @import("./parser.zig").Diagnostics;

pub const Error = error{
    BadToken,
    MixedIndentation,
    UnquantizedIndentation,
    TooMuchIndentation,
    MissingNewline,
    TrailingWhitespace,
    Impossible,
};

pub const DetectedIndentation = union(enum) {
    unknown: void,
    spaces: usize,
    tabs: void,
};

pub const InlineItem = union(enum) {
    empty: void,
    scalar: []const u8,
    line_string: []const u8,
    space_string: []const u8,

    flow_list: []const u8,
    flow_map: []const u8,

    pub fn lineEnding(self: InlineItem) u8 {
        return switch (self) {
            .line_string => '\n',
            .space_string => ' ',
            else => unreachable,
        };
    }
};

pub const LineContents = union(enum) {
    comment: []const u8,

    in_line: InlineItem,
    list_item: InlineItem,
    map_item: struct { key: []const u8, val: InlineItem },
};

pub const ShiftDirection = enum { indent, dedent, none };

pub const LineShift = union(ShiftDirection) {
    indent: void,
    // we can dedent multiple levels at once.
    dedent: usize,
    none: void,
};

pub const Line = struct {
    shift: LineShift,
    contents: LineContents,
    raw: []const u8,
};

// buffer is expected to be either LineBuffer or FixedLineBuffer, but can
// technically be anything with a `nextLine` method
pub fn LineTokenizer(comptime Buffer: type) type {
    return struct {
        buffer: Buffer,
        index: usize = 0,
        indentation: DetectedIndentation = .unknown,
        last_indent: usize = 0,
        diagnostics: *Diagnostics,
        row: usize = 0,

        pub fn next(self: *@This()) !?Line {
            lineloop: while (try self.buffer.nextLine()) |raw_line| {
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
                                .unknown => self.indentation = .{ .spaces = 0 },
                                .spaces => {},
                                .tabs => return error.MixedIndentation,
                            }
                        },
                        '\t' => {
                            switch (self.indentation) {
                                .unknown => self.indentation = .tabs,
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

                const shift: LineShift = if (quantized > self.last_indent) rel: {
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
                        // force comments to be followed by a space. This makes them
                        // behave the same way as strings, actually.
                        if (line.len > 1 and line[1] != ' ') return error.BadToken;

                        // simply lie about indentation when the line is a comment.
                        quantized = self.last_indent;
                        return .{
                            .shift = .none,
                            .contents = .{ .comment = line[1..] },
                            .raw = line,
                        };
                    },
                    '|', '>', '[', '{' => {
                        return .{
                            .shift = shift,
                            .contents = .{ .in_line = try detectInlineItem(line) },
                            .raw = line,
                        };
                    },
                    '-' => {
                        if (line.len > 1 and line[1] != ' ') return error.BadToken;

                        return if (line.len == 1) .{
                            .shift = shift,
                            .contents = .{ .list_item = .empty },
                            .raw = line,
                        } else .{
                            .shift = shift,
                            .contents = .{ .list_item = try detectInlineItem(line[2..]) },
                            .raw = line,
                        };
                    },
                    else => {
                        for (line, 0..) |char, idx| {
                            if (char == ':') {
                                if (idx + 1 == line.len) return .{
                                    .shift = shift,
                                    .contents = .{ .map_item = .{ .key = line[0..idx], .val = .empty } },
                                    .raw = line,
                                };

                                if (line[idx + 1] != ' ') return error.BadToken;

                                return .{
                                    .shift = shift,
                                    .contents = .{ .map_item = .{
                                        .key = line[0..idx],
                                        .val = try detectInlineItem(line[idx + 2 ..]),
                                    } },
                                    .raw = line,
                                };
                            }
                        }

                        return .{
                            .shift = shift,
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
