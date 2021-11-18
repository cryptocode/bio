const std = @import("std");
const mem = @import("gc.zig");

/// Maintains current source locations, pushing and popping from a stack as
/// files are being imported.
pub const SourceLocation = struct {
    var stack: std.ArrayList(SourceLocation) = undefined;
    var files: std.StringHashMap(void) = undefined;

    pub fn current() *SourceLocation {
        return &stack.items[stack.items.len - 1];
    }

    /// Push a new sourec context. The file name is duplicated.
    pub fn push(file: []const u8) !void {
        const loc = try stack.addOne();
        loc.line = 0;
        loc.col = 0;
        if (!files.contains(file)) {
            loc.file = try mem.allocator.dupe(u8, file);
            try files.put(loc.file, {});
        } else {
            loc.file = files.getEntry(file).?.key_ptr.*;
        }
    }

    pub fn pop() void {
        _ = stack.pop();
    }

    pub fn initStack() void {
        stack = std.ArrayList(SourceLocation).init(mem.allocator);
        files = std.StringHashMap(void).init(mem.allocator);
        push("repl") catch unreachable;
    }

    pub fn deinitStack() void {
        while (stack.items.len > 0) {
            pop();
        }
        var it = files.iterator();
        while (it.next()) |file| {
            mem.allocator.free(file.key_ptr.*);
        }
        files.deinit();
        stack.deinit();
    }

    file: []const u8 = "invalid",
    line: u32 = 0,
    col: u32 = 0,
};
