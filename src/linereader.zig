const std = @import("std");
const builtin = @import("builtin");
const mem = @import("gc.zig");
const target = @import("builtin").target;

const is_windows = target.os.tag == .windows;
const linenoise = @cImport({
    if (!is_windows) {
        @cInclude("stddef.h");
        @cInclude("linenoise.h");
    }
});

/// Linenoise wrapper with a Reader interface
const Linenoise = struct {
    line: ?[:0]u8 = null,
    index: usize = 0,
    remaining: usize = 0,
    eof_next: bool = false,
    prompt: []const u8 = "",

    fn init() Linenoise {
        return .{};
    }

    pub fn hidePrompt(self: *Linenoise) void {
        self.prompt = "";
    }

    /// Prints the REPL prompt. On nix* systems this is done by linenoise in readFn.
    pub fn printPrompt(self: *Linenoise, prompt: []const u8) !void {
        self.prompt = prompt;
        if (is_windows and self.prompt.len > 0) {
            try std.io.getStdOut().writer().print("{s}", .{self.prompt});
        }
    }

    /// Add the given entry to the REPL history
    pub fn addToHistory(_: *Linenoise, entry: []const u8) !void {
        if (!is_windows and entry.len > 0) {
            const duped = try mem.allocator.dupeZ(u8, entry);
            defer mem.allocator.free(duped);
            // Linenoise takes a copy
            _ = linenoise.linenoiseHistoryAdd(duped.ptr);
        }
    }

    /// This satisfies the Reader interface. The first call will cause
    /// linenoise to be invoked with an optional prompt. The returned line
    /// is then consumed, after which EndOfStream is returned. Rinse and
    /// repeat. For Windows, we simply delegate to stdin.
    fn readFn(self: *@This(), dest: []u8) anyerror!usize {
        var copy_count: usize = 0;
        if (!is_windows) {
            if (self.eof_next) {
                self.eof_next = false;
                return error.EndOfStream;
            }

            if (self.remaining == 0) {
                // This gives us a [*c]u8...
                const c_line = linenoise.linenoise(self.prompt.ptr);
                if (c_line == null) {
                    return error.EndOfStream;
                }

                // ...which we convert to a [:0]u8
                self.line = std.mem.span(c_line);
                self.remaining = self.line.?.len;
                self.index = 0;
            }

            copy_count = std.math.min(self.remaining, dest.len);
            if (copy_count > 0) std.mem.copy(u8, dest, self.line.?[self.index .. self.index + copy_count]);
            self.remaining -= copy_count;
            self.index += copy_count;

            if (self.remaining == 0) {
                self.eof_next = true;
                std.heap.c_allocator.free(self.line.?);
            }
            return copy_count;
        } else {
            return std.io.getStdIn().reader().read(dest);
        }
    }

    pub const Reader = std.io.Reader(*@This(), anyerror, readFn);
    pub fn reader(self: *@This()) Reader {
        return .{ .context = self };
    }
};

pub var linenoise_wrapper = Linenoise.init();
pub var linenoise_reader = linenoise_wrapper.reader();
