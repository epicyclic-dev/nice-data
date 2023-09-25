const std = @import("std");

pub const IndexSlice = struct { start: usize, len: usize };

pub const LineBuffer = struct {
    allocator: std.mem.Allocator,
    internal: FixedLineBuffer,
    used: usize,

    pub const default_capacity: usize = 4096;
    pub const Error = std.mem.Allocator.Error;

    pub fn init(allocator: std.mem.Allocator) Error!LineBuffer {
        return initCapacity(allocator, default_capacity);
    }

    pub fn initCapacity(allocator: std.mem.Allocator, capacity: usize) Error!LineBuffer {
        return .{
            .allocator = allocator,
            .internal = .{
                .buffer = try allocator.alloc(u8, capacity),
                .window = .{ .start = 0, .len = 0 },
            },
            .used = 0,
        };
    }

    pub fn feed(self: *LineBuffer, data: []const u8) Error!void {
        if (data.len == 0) return;
        // TODO: check for usize overflow here if we want Maximum Robustness
        const new_window_len = self.internal.window.len + data.len;

        // data cannot fit in the buffer with our scan window, so we have to realloc
        if (new_window_len > self.internal.buffer.len) {
            // TODO: adopt an overallocation strategy? Will potentially avoid allocating
            //       on every invocation but will cause the buffer to oversize
            try self.allocator.realloc(self.internal.buffer, new_window_len);
            self.rehome();
            @memcpy(self.internal.buffer[self.used..].ptr, data);
            self.used = new_window_len;
            self.internal.window.len = new_window_len;
        }
        // data will fit, but needs to be moved in the buffer
        else if (self.internal.window.start + new_window_len > self.internal.buffer.len) {
            self.rehome();
            @memcpy(self.internal.buffer[self.used..].ptr, data);
            self.used = new_window_len;
            self.internal.window.len = new_window_len;
        }
        // data can simply be appended
        else {
            @memcpy(self.internal.buffer[self.used..].ptr, data);
        }
    }

    /// The memory returned by this function is valid until the next call to `feed`.
    /// The resulting slice does not include the newline character.
    pub fn nextLine(self: *LineBuffer) ?[]const u8 {
        return self.internal.nextLine();
    }

    fn rehome(self: *LineBuffer) void {
        self.internal.rehome();
        self.used = self.internal.window.len;
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

    // move the current scan window to the beginning of the buffer. This internal
    // method is used by LineBuffer.
    fn rehome(self: *LineBuffer) usize {
        if (self.window.start == 0) return;

        const window = self.buffer[self.window.start..][0..self.window.len];

        // if the window is longer than its starting index, the memory move will be
        // overlapping, so we can't use memcpy
        if (self.window.len > self.window.start)
            std.mem.copyForwards(u8, self.buffer, window)
        else
            @memcpy(self.buffer.ptr, window);

        self.window.start = 0;
    }
};
