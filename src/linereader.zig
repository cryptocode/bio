const std = @import("std");
const target = @import("builtin").target;
const gc = @import("boehm.zig");

const is_windows = target.os.tag == .windows;
const linenoise = @cImport({
    if (!is_windows) {
        @cInclude("stddef.h");
        @cInclude("stdlib.h");
        @cInclude("linenoise.h");
    }
});

fn readLineFromFileAlloc(io: std.Io, allocator: std.mem.Allocator, file: std.Io.File) !?[]u8 {
    var read_buffer: [256]u8 = undefined;
    var reader = file.reader(io, &read_buffer);
    var line = std.array_list.Managed(u8).init(allocator);
    errdefer line.deinit();

    while (true) {
        const byte = reader.interface.takeByte() catch |err| switch (err) {
            error.EndOfStream => {
                if (line.items.len == 0) return null;
                return try line.toOwnedSlice();
            },
            else => return err,
        };

        if (byte == '\n') return try line.toOwnedSlice();
        try line.append(byte);
    }
}

pub fn readLine(allocator: std.mem.Allocator, io: std.Io, prompt: []const u8) !?[]u8 {
    if (!is_windows) {
        const c_line = linenoise.linenoise(prompt.ptr);
        if (c_line == null) return null;
        defer std.c.free(c_line);
        return try allocator.dupe(u8, std.mem.span(c_line));
    }

    if (prompt.len > 0) {
        var buffer: [4096]u8 = undefined;
        var writer = std.Io.File.stdout().writer(io, &buffer);
        try writer.interface.print("{s}", .{prompt});
        try writer.interface.flush();
    }
    return readLineFromFileAlloc(io, allocator, .stdin());
}

pub fn addToHistory(entry: []const u8) !void {
    if (!is_windows and entry.len > 0) {
        const duped = try gc.allocator().dupeZ(u8, entry);
        _ = linenoise.linenoiseHistoryAdd(duped.ptr);
    }
}
