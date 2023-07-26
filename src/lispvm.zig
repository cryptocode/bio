const std = @import("std");
const ast = @import("ast.zig");

const BytecodeVersion = 1;

pub const Opcode = enum(u8) {
    call_function = 0x0,
    call_intrinsic,
    gc_collect,
    /// Compare the top two values on the stack and update the compare state
    order,
    /// Push an immediate (const) value
    push,
    /// Pop a value and discard it
    pop,
    /// The stack must contain a list of values to be added, which is replaced by the sum
    add,
    /// Pop and print the N values on the stack in argument order
    print,
    /// Defines a constant byte sequence by name
    /// Example: const_bytes_set foo [bar\0abc]
    //const_bytes_set,
    //const_bytes_get,
    /// Exit the process with an exit code
    exit,
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
    // Pop N values from the stack, push the accumulated sum
    add: u64,
    // Pop N values from the stack, print them in argument order
    print: u64,
    // Exit the process with an exit code
    exit: u8,
};

/// Execution state
/// When the state is serialized, the VM can be paused and resumed at a later time
pub const State = struct {
    /// Program counter
    pc: usize,

    /// Result of `order`: -1 if less than, 0 if equal, 1 if greater
    /// Used by je, jne, jg, jge, jl, jle
    /// This is a virtual register to reduce stack operations
    compare_result: i2,
};

pub const VM = struct {
    state: State = undefined,
    const_pool: std.ArrayList(ast.ExprValue),
    stack: std.ArrayList(ast.ExprValue),
    ops: std.ArrayList(Op),

    pub fn init() VM {
        return .{
            .stack = std.ArrayList(ast.ExprValue).init(std.heap.page_allocator),
            .const_pool = std.ArrayList(ast.ExprValue).init(std.heap.page_allocator),
            .ops = std.ArrayList(Op).init(std.heap.page_allocator),
            .state = .{ .pc = 0, .compare_result = 0 },
        };
    }

    pub fn clearOps(self: *@This()) void {
        self.ops.clearRetainingCapacity();
        self.state.pc = 0;
    }

    // fn render(expr: *ast.Expr) ![]u8 {
    //     const str = try expr.toStringAlloc();
    //     defer mem.allocator.free(str);
    //     return try std.mem.replaceOwned(u8, mem.allocator, str, "\\n", "\n");
    // }
    pub fn run(self: *@This()) !void {
        for (self.ops.items) |op| {
            defer self.state.pc += 1;
            switch (op) {
                .call_function => {},
                .call_intrinsic => {},
                .gc_collect => {},
                .order => {},
                .push => |val| {
                    try self.stack.append(val);
                },
                .pop => {},
                .add => |arg_count| {
                    var sum: f64 = 0;
                    for (0..arg_count) |_| {
                        const value = self.stack.pop();
                        if (value == .sym) {
                            std.debug.print("WTF got sym {s}\n", .{value.sym});
                        }
                        sum += value.num;
                    }
                    // while (self.stack.items.len > 0) {
                    //     const value = self.stack.pop();
                    //     sum += value.num;
                    // }
                    try self.stack.append(.{ .num = sum });
                },
                .print => |arg_count| {

                    std.debug.print("Arg count: {d}\n", .{arg_count});

                    // Print the top arg_count values on the stack, in argument order
                    self.stack.items = self.stack.items[self.stack.items.len - arg_count..];
                    for (self.stack.items) |value| {
                        // TODO: render(...) before printing (do what stdPrint does)
                        std.debug.print("{d}\n", .{value.num});
                    }

                    //const value = self.stack.resize(self.stack.items.len - arg_count);
                    // TODO: render(...) before printing (do what stdPrint does)
                    //std.debug.print("\nOn stack: {d}\n", .{value});
                },
                .exit => {
                    std.os.exit(op.exit);
                },
            }
        }
    }

    /// Serialize the ops to a writer
    pub fn serialize(self: *@This(), alloc: std.mem.Allocator, writer: anytype) !void {
        _ = alloc;
        try writer.writeIntLittle(u8, BytecodeVersion);
        for (self.ops.items) |op| {
            try writer.writeIntLittle(u8, @intFromEnum(op));
            switch (op) {
                .push => |val| {
                    try serializeValue(val, writer);
                },
                .print => |arg_count| {
                    try writer.writeIntLittle(u64, arg_count);
                },
                .exit => {
                    try writer.writeByte(op.exit);
                },
                else => {},
            }
        }
    }

    fn serializeValue(val: ast.ExprValue, writer: anytype) !void {
        try writer.writeIntLittle(u8, @intFromEnum(val));
        switch (val) {
            .num => {
                try writer.writeIntLittle(u64, @as(u64, @bitCast(val.num)));
            },
            else => {},
        }
    }

    /// Deserialize the ops from a reader
    pub fn deserialize(_: *@This(), reader: anytype) !void {
        const version = try reader.readIntLittle(u8);
        if (version != BytecodeVersion) {
            return error.InvalidBytecodeVersion;
        }

        while (true) {
            const op = reader.readEnum(Opcode, std.builtin.Endian.Little) catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };
            switch (op) {
                .push => {
                    const val = try deserializeValue(reader);
                    _ = val;
                },
                .print => {
                    const count = try reader.readIntLittle(u64);
                    _ = count;
                },
                .exit => {
                    const exit = try reader.readByte();
                    _ = exit;
                },
                else => {},
            }
        }
    }

    fn deserializeValue(reader: anytype) !ast.ExprValue {
        const kind = try reader.readEnum(ast.ExprType, std.builtin.Endian.Little);
        switch (kind) {
            .num => {
                const num = @as(f64, @bitCast(try reader.readIntLittle(u64)));
                return .{ .num = num };
            },
            else => {
                return error.InvalidValue;
            },
        }
    }

    /// Assemble text into byte code
    pub fn assemble(self: *@This(), reader: anytype) void {
        _ = .{ self, reader };
    }

    pub fn disassemble(self: *@This(), writer: anytype) !void {
        for (self.ops.items) |op| {
            try writer.print("{s} ", .{@tagName(op)});
            switch (op) {
                .push => |val| {
                    try writer.print("{s} ", .{@tagName(val)});
                    switch (val) {
                        .num => {
                            try writer.print("{}", .{val.num});
                        },
                        else => {},
                    }
                },
                .print => |arg_count| {
                    try writer.print("{}", .{arg_count});
                },
                .exit => {
                    try writer.print("{}", .{op.exit});
                },
                else => {},
            }
            try writer.print("\n", .{});
        }
    }
};

test "vm-1" {
    var vm = VM.init();
    try vm.ops.append(.{ .push = .{ .num = 1.0 } });
    try vm.ops.append(.{ .push = .{ .num = 2.0 } });
    try vm.ops.append(.{ .add = 2 });
    try vm.ops.append(.{ .print = 1 });
    //try vm.ops.append(.{ .exit = 0 });
    try vm.run();
}

test "serialize-1" {
    var vm = VM.init();
    try vm.ops.append(.{ .push = .{ .num = 1.0 } });
    try vm.ops.append(.{ .push = .{ .num = 2.0 } });
    try vm.ops.append(.{ .add = 2 });
    try vm.ops.append(.{ .print = 1 });
    try vm.ops.append(.{ .exit = 0 });

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

    std.debug.print("========================\n", .{});
    std.debug.print("Disassembly:\n", .{});
    std.debug.print("========================\n", .{});
    try vm.disassemble(std.io.getStdOut().writer());
    std.debug.print("========================\n", .{});
}
