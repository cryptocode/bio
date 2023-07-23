const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = std.mem.Allocator;

const gc = @cImport({
    @cInclude("gc.h");
});

pub fn allocator() Allocator {
    if (gc.GC_is_init_called() == 0) {
        std.debug.print("INITING\n", .{});
        gc.GC_init();
        //gc.GC_disable();
    }

    //if (true) return std.heap.page_allocator;

    return Allocator{
        .ptr = undefined,
        .vtable = &gc_allocator_vtable,
    };
}


/// When free'ing manually, call this function before any allocations
/// to enable leak detections.
pub fn enableLeakDetection() void {
    gc.GC_set_find_leak(1);
}

pub const GCKind = enum {
    nul,
    /// Short collection burst (typically a page worth of allocations)
    short,
    /// Regular collection
    normal,
    /// This is a full collection, only use when system is running low on memory
    aggressive,
};

pub fn setMaxHeap(max_size: usize) void {
    gc.GC_set_max_heap_size(max_size);    
}

/// Run garbage collector where `kind` is the type of collection to run; normally you want .short or .normal.
/// Returns true if collection is completed. It's possible to call this in a loop until it returns true (only useful for .short)
pub fn collect(kind: GCKind) bool {
    switch (kind) {
        .short => {if (gc.GC_collect_a_little() != 0) return false;},
        .normal => gc.GC_gcollect(),
        .aggressive => gc.GC_gcollect_and_unmap(),
        else => {},
    }

    return true;
}

pub fn memUsage() usize {
    return gc.GC_get_memory_use();
}

pub fn heapSize() u64 {
    return gc.GC_get_heap_size();
}

pub fn collectionCount() usize {
    return gc.GC_get_gc_no();
}

pub fn enable() void {
    gc.GC_enable();
}

pub fn disable() void {
    gc.GC_disable();
}


pub fn setLeakDetection(enabled: bool) void {
    return gc.GC_set_find_leak(@intFromBool(enabled));
}

/// Zig allocator using Boehm GC
pub const GcAllocator = struct {
    fn alloc(_: *anyopaque, len: usize, log2_align: u8, return_address: usize) ?[*]u8 {
        _ = return_address;
        assert(len > 0);
        return alignedAlloc(len, log2_align);
    }

    fn resize(_: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, return_address: usize) bool {
        _ = .{log2_buf_align, return_address};
        if (new_len <= buf.len) {
            return true;
        }

        const full_len = alignedAllocSize(buf.ptr);
        if (new_len <= full_len) {
            return true;
        }

        return false;
    }

    fn free(_: *anyopaque, buf: []u8, log2_buf_align: u8, return_address: usize) void {
        _ = .{log2_buf_align, return_address};
        alignedFree(buf.ptr);
    }

    fn getHeader(ptr: [*]u8) *[*]u8 {
        return @as(*[*]u8, @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize)));
    }

    fn alignedAlloc(len: usize, log2_align: u8) ?[*]u8 {
        const alignment = @as(usize, 1) << @as(Allocator.Log2Align, @intCast(log2_align));
        var unaligned_ptr = @as([*]u8, @ptrCast(gc.GC_malloc(len + alignment - 1 + @sizeOf(usize)) orelse return null));
        const unaligned_addr = @intFromPtr(unaligned_ptr);
        const aligned_addr = mem.alignForward(usize, unaligned_addr + @sizeOf(usize), alignment);
        var aligned_ptr = unaligned_ptr + (aligned_addr - unaligned_addr);
        getHeader(aligned_ptr).* = unaligned_ptr;
        return aligned_ptr;
    }

    fn alignedFree(ptr: [*]u8) void {
        const unaligned_ptr = getHeader(ptr).*;
        gc.GC_free(unaligned_ptr);
    }

    fn alignedAllocSize(ptr: [*]u8) usize {
        const unaligned_ptr = getHeader(ptr).*;
        const delta = @intFromPtr(ptr) - @intFromPtr(unaligned_ptr);
        return gc.GC_size(unaligned_ptr) - delta;
    }
};

const gc_allocator_vtable = Allocator.VTable{
    .alloc = GcAllocator.alloc,
    .resize = GcAllocator.resize,
    .free = GcAllocator.free,
};

test "GcAllocator" {
    const alloc = allocator();
    try std.heap.testAllocator(alloc);
    try std.heap.testAllocatorAligned(alloc);
    try std.heap.testAllocatorLargeAlignment(alloc);
    try std.heap.testAllocatorAlignedShrink(alloc);
}

test "heap size" {
    // No garbage so should be 0
    try testing.expect(collect(.short) == 0);

    // Force a collection should work
    collect();

    try testing.expect(heapSize() > 0);
}
