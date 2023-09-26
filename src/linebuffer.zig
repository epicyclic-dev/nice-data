const std = @import("std");

pub const IndexSlice = struct { start: usize, len: usize };

pub const Error = error{
    CarriageReturn,
    TrailingWhitespace,
    NonprintingAscii,
    InputIsNotValidUtf8,
};

pub const Strictness = struct {
    check_carriage_return: bool = true,
    check_trailing_whitespace: bool = true,
    check_nonprinting_ascii: bool = true,
    validate_utf8: bool = false,
};

pub const ValidatingLineBuffer = LineBuffer(.{
    .validate_utf8 = true,
});
pub const StrictLineBuffer = LineBuffer(.{});
pub const SloppyLineBuffer = LineBuffer(.{
    .check_carriage_return = false,
    .check_trailing_whitespace = false,
    .check_nonprinting_ascii = false,
    .validate_utf8 = false,
});
pub const ValidatingFixedLineBuffer = FixedLineBuffer(.{
    .validate_utf8 = true,
});
pub const StrictFixedLineBuffer = FixedLineBuffer(.{});
pub const SloppyFixedLineBuffer = FixedLineBuffer(.{
    .check_carriage_return = false,
    .check_trailing_whitespace = false,
    .check_nonprinting_ascii = false,
    .validate_utf8 = false,
});

pub fn LineBuffer(comptime options: Strictness) type {
    return struct {
        allocator: std.mem.Allocator,
        internal: FixedLineBuffer(options),
        used: usize,

        pub const default_capacity: usize = 4096;

        pub fn init(allocator: std.mem.Allocator) !@This() {
            return initCapacity(allocator, default_capacity);
        }

        pub fn initCapacity(allocator: std.mem.Allocator, capacity: usize) !@This() {
            return .{
                .allocator = allocator,
                .internal = .{
                    .buffer = try allocator.alloc(u8, capacity),
                    .window = .{ .start = 0, .len = 0 },
                },
                .used = 0,
            };
        }

        pub fn deinit(self: @This()) void {
            self.allocator.free(self.internal.buffer);
        }

        pub fn feed(self: *@This(), data: []const u8) !void {
            if (data.len == 0) return;
            // TODO: check for usize overflow here if we want Maximum Robustness
            const new_window_len = self.internal.window.len + data.len;

            // data cannot fit in the buffer with our scan window, so we have to realloc
            if (new_window_len > self.internal.buffer.len) {
                // TODO: adopt an overallocation strategy? Will potentially avoid allocating
                //       on every invocation but will cause the buffer to oversize
                self.internal.buffer = try self.allocator.realloc(@constCast(self.internal.buffer), new_window_len);
                self.rehome();
                @memcpy(@constCast(self.internal.buffer[self.used..].ptr), data);
            }
            // data will fit, but needs to be moved in the buffer
            else if (self.internal.window.start + new_window_len > self.internal.buffer.len) {
                self.rehome();
                @memcpy(@constCast(self.internal.buffer[self.used..].ptr), data);
            }
            // data can simply be appended
            else {
                @memcpy(@constCast(self.internal.buffer[self.used..].ptr), data);
            }
            self.used += data.len;
            self.internal.window.len = new_window_len;
        }

        /// The memory returned by this function is valid until the next call to `feed`.
        /// The resulting slice does not include the newline character.
        pub fn nextLine(self: *@This()) !?[]const u8 {
            return self.internal.nextLine();
        }

        fn rehome(self: *@This()) void {
            self.internal.rehome();
            self.used = self.internal.window.len;
        }
    };
}

pub fn FixedLineBuffer(comptime options: Strictness) type {
    return struct {
        buffer: []const u8,
        window: IndexSlice,

        pub fn init(data: []const u8) @This() {
            return .{ .buffer = data, .window = .{ .start = 0, .len = data.len } };
        }

        pub fn nextLine(self: *@This()) !?[]const u8 {
            if (self.window.start >= self.buffer.len or self.window.len == 0)
                return null;

            const window = self.buffer[self.window.start..][0..self.window.len];

            const split: usize = split: {
                for (window, 0..) |char, idx| {
                    if (comptime options.check_carriage_return)
                        if (char == '\r') return error.IllegalCarriageReturn;

                    if (comptime options.check_nonprinting_ascii)
                        if ((char != '\n' and char != '\t') and (char < ' ' or char == 0x7F))
                            return error.IllegalNonprintingAscii;

                    if (comptime options.check_trailing_whitespace) {
                        if (char == '\n') {
                            if (idx > 0 and (window[idx - 1] == ' ' or window[idx - 1] == '\t'))
                                return error.IllegalTrailingSpace;
                            break :split idx;
                        }
                    } else {
                        if (char == '\n') break :split idx;
                    }
                }
                return null;
            };

            self.window.start += split + 1;
            self.window.len -= split + 1;

            if (comptime options.validate_utf8) {
                const line = window[0..split];
                return if (std.unicode.utf8ValidateSlice(line)) line else error.InputIsNotValidUtf8;
            } else {
                return window[0..split];
            }
        }

        // move the current scan window to the beginning of the buffer. This internal
        // method is used by LineBuffer.
        fn rehome(self: *@This()) void {
            if (self.window.start == 0) return;

            const window = self.buffer[self.window.start..][0..self.window.len];

            // if the window is longer than its starting index, the memory move will be
            // overlapping, so we can't use memcpy
            if (self.window.len > self.window.start)
                std.mem.copyForwards(u8, @constCast(self.buffer), window)
            else
                @memcpy(@constCast(self.buffer.ptr), window);

            self.window.start = 0;
        }
    };
}
