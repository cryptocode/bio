const std = @import("std");
const ast = @import("ast.zig");

pub const Opcode = enum(u8) {
    call_function = 0x0,
    call_intrinsic,
    gc_collect,
    /// Compare the top two values on the stack and update the compare state
    order,
    /// Push an immediate value
    push,
    /// Pop a value and discard it
    pop,
    /// The stack must contain a list of values to be added, which is replaced by the sum
    add,
    /// Print the top value on the stack
    print,
};

pub const Op = union(Opcode) {
    // A function is initially identified by its name, and then resolved to an offset
    call_function: struct {
        name: []const u8,
        offset: ?usize = null,
    },
    // An intrinsic is initially identified by its name, and then resolved to a function pointer
    call_intrinsic: struct {
        name: []const u8,
        fun: ?ast.IntrinsicFn,
    },
    gc_collect: void,
    order: struct {
        op: std.math.Order,
    },
    push: ast.ExprValue,
    pop: void,
    add: void,
    print: void,
};

/// Execution state
/// When the state is serialized, the VM can be paused and resumed at a later time
pub const State = struct {
    /// Program counter
    pc: usize,

    /// Result of `order`: -1 if less than, 0 if equal, 1 if greater
    /// Used by je, jne, jg, jge, jl, jle
    compare_result: i1,
};

pub const VM = struct {
    state: State = undefined,
    constants: std.ArrayList(ast.ExprValue),
    stack: std.ArrayList(ast.ExprValue),
    ops: std.ArrayList(Op),

    pub fn init() VM {
        return .{
            .stack = std.ArrayList(ast.ExprValue).init(std.heap.page_allocator),
            .constants = std.ArrayList(ast.ExprValue).init(std.heap.page_allocator),
            .ops = std.ArrayList(Op).init(std.heap.page_allocator),
        };
    }

    pub fn run(self: *@This()) !void {
        for (self.ops.items) |op| {
            switch (op) {
                .call_function => {},
                .call_intrinsic => {},
                .gc_collect => {},
                .order => {},
                .push => |val| {
                    try self.stack.append(val);
                },
                .pop => {},
                .add => {
                    // TODO: stack should contain number of items to add - or... be a list of values
                    var sum: f64 = 0;
                    while (self.stack.items.len > 0) {
                        const value = self.stack.pop();
                        sum += value.num;
                    }
                    try self.stack.append(.{ .num = sum});
                },
                .print => {
                    const value = self.stack.pop();
                    std.debug.print("\nOn stack: {}\n", .{value});
                },
            }
        }
    }

    /// Serialize the ops to a writer
    pub fn serialize(self: *@This(), alloc: std.mem.Allocator, writer: anytype) !void {
        _ = alloc;
        for (self.ops.items) |op| {
            try writer.writeIntLittle(u8, @enumToInt(op));
            switch (op) {
                .call_function => {},
                .call_intrinsic => {},
                .gc_collect => {},
                .order => {},
                .push => |val| {
                    try serializeValue(val, writer);
                },
                .pop => {},
                .add => {},
                .print => {},
            }
        }
    }

    fn serializeValue(val: ast.ExprValue, writer: anytype) !void {
        try writer.writeIntLittle(u8, @enumToInt(val));
        switch (val) {
            .num => {
                try writer.writeIntLittle(u64, @bitCast(u64, val.num));
            },
            else => {},
        }
    }

    fn deserializeValue(reader: anytype) !ast.ExprValue {
        const kind = try reader.readEnum(ast.ExprType, std.builtin.Endian.Little);
        switch (kind) {
            .num => {
                const num = @bitCast(f64, try reader.readIntLittle(u64));
                return .{ .num = num };
            },
            else => {
                return error.InvalidValue;
            },
        }
    }

    /// Deserialize the ops from a reader
    pub fn deserialize(_: *@This(), reader: anytype) !void {
        while (true) {
            const op = reader.readEnum(Opcode, std.builtin.Endian.Little) catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };
            switch (op) {
                .call_function => {},
                .call_intrinsic => {},
                .gc_collect => {},
                .order => {},
                .push => {
                    const val = try deserializeValue(reader);
                    std.debug.print("::push num {}\n", .{val.num});
                },
                .pop => {},
                .add => {
                    std.debug.print("::add\n", .{});
                },
                .print => {
                    std.debug.print("::print\n", .{});
                },
            }
        }
    }
};

test "vm-1" {
    var vm = VM.init();
    try vm.ops.append(.{ .push = .{ .num = 1.0 } });
    try vm.ops.append(.{ .push = .{ .num = 2.0 } });
    try vm.ops.append(.{ .add = {} });
    try vm.ops.append(.{ .print = {} });
    try vm.run();
}

test "serialize-1" {
    var vm = VM.init();
    try vm.ops.append(.{ .push = .{ .num = 1.0 } });
    try vm.ops.append(.{ .push = .{ .num = 2.0 } });
    try vm.ops.append(.{ .add = {} });
    try vm.ops.append(.{ .print = {} });
    var bytes = std.ArrayList(u8).init(std.testing.allocator);    
    defer bytes.deinit();
    try vm.serialize(std.heap.page_allocator, bytes.writer());
    
    // Print all bytes as hex
    for (bytes.items) |b| {
        std.debug.print("{x} ", .{b});
    }
    std.debug.print("\n", .{});
    std.debug.print("Deserialize\n", .{});
    
    var fbs = std.io.fixedBufferStream(bytes.items);
    try vm.deserialize(fbs.reader());
}

// test "deserialize-1" {
//     var vm = VM.init();
//     try vm.ops.append(.{ .push = .{ .num = 1.0 } });
//     try vm.ops.append(.{ .push = .{ .num = 2.0 } });
//     try vm.ops.append(.{ .add = {} });
//     try vm.ops.append(.{ .print = {} });
//     // const bytes = vm.ops.toBytes();
//     // std.debug.print("bytes: {}\n", .{bytes});
//     // const ops = std.ArrayList(Op).fromBytes(bytes);
//     // std.debug.print("ops: {}\n", .{ops});    
// }