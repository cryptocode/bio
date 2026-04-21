const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = std.mem.Allocator;

const gc = @cImport({
    @cInclude("gc.h");
});

/// Returns the Zig allocator interface to BoehmGC
pub fn allocator() Allocator {
    if (gc.GC_is_init_called() == 0) {
        // Make sure interior pointers are handled. It's recommended to call this before GC_init.
        gc.GC_set_all_interior_pointers(1);
        gc.GC_init();
    }

    return Allocator{
        .ptr = undefined,
        .vtable = &gc_allocator_vtable,
    };
}

/// Kind of garbage collection to run
pub const GCKind = enum {
    nul,
    /// Short collection burst (typically a page worth of allocations)
    short,
    /// Regular collection
    normal,
    /// This is a full collection, only use when system is running low on memory
    aggressive,
};

/// Run garbage collector where `kind` is the type of collection to run; normally you want .short or .normal.
/// Returns true if collection is completed. It's possible to call this in a loop until it returns true (only useful for .short)
pub fn collect(kind: GCKind) bool {
    switch (kind) {
        .short => {
            if (gc.GC_collect_a_little() != 0) return false;
        },
        .normal => gc.GC_gcollect(),
        .aggressive => gc.GC_gcollect_and_unmap(),
        else => {},
    }

    return true;
}

pub fn setMaxHeap(max_size: usize) void {
    gc.GC_set_max_heap_size(max_size);
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
pub const BoehmGcAllocator = struct {
    fn alloc(_: *anyopaque, len: usize, alignment: std.mem.Alignment, return_address: usize) ?[*]u8 {
        _ = return_address;
        assert(len > 0);
        const raw = gc.GC_memalign(alignment.toByteUnits(), len);
        return @as([*]u8, @ptrCast(raw));
    }

    fn resize(_: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, return_address: usize) bool {
        _ = .{ buf, alignment, new_len, return_address };
        return false;
    }

    fn remap(_: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, return_address: usize) ?[*]u8 {
        _ = .{ buf, alignment, new_len, return_address };
        return null;
    }

    fn free(_: *anyopaque, buf: []u8, alignment: std.mem.Alignment, return_address: usize) void {
        _ = .{ buf, alignment, return_address };
    }
};

const gc_allocator_vtable = Allocator.VTable{
    .alloc = BoehmGcAllocator.alloc,
    .resize = BoehmGcAllocator.resize,
    .remap = BoehmGcAllocator.remap,
    .free = BoehmGcAllocator.free,
};
