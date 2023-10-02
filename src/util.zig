const std = @import("std");
const ast = @import("ast.zig");
const interpreter = @import("interpreter.zig");

const Expr = ast.Expr;
const ExprType = ast.ExprType;
const ExprErrors = ast.ExprErrors;

pub fn isEmptyList(expr: *Expr) bool {
    return (expr.val == ExprType.lst and expr.val.lst.items.len == 0);
}

pub fn isError(expr: *Expr) bool {
    return expr.val == ExprType.err;
}

pub fn isFalsy(expr: *Expr) bool {
    return expr.isIntrinsic(.@"#f") or expr.isIntrinsic(.nil) or isEmptyList(expr) or isError(expr);
}

pub fn isNil(expr: *Expr) bool {
    return expr.isIntrinsic(.nil);
}

pub fn isNotNil(expr: *Expr) bool {
    return !expr.isIntrinsic(.nil);
}

pub fn requireExactArgCount(args_required: usize, args: []const *Expr) !void {
    if (args.len != args_required) {
        return ExprErrors.InvalidArgumentCount;
    }
}

pub fn requireMinimumArgCount(args_required: usize, args: []const *Expr) !void {
    if (args.len < args_required) {
        return ExprErrors.InvalidArgumentCount;
    }
}

pub fn requireMaximumArgCount(max_arg_count: usize, args: []const *Expr) !void {
    if (args.len > max_arg_count) {
        return ExprErrors.InvalidArgumentCount;
    }
}

pub fn requireType(ev: *interpreter.Interpreter, expr: *Expr, etype: ExprType) !void {
    if (expr.val != etype) {
        const str = try expr.toStringAlloc();
        try ev.printErrorFmt(expr, "Expected {s}, got {s} with value: {s}", .{ ast.typeString(etype), ast.typeString(std.meta.activeTag(expr.val)), str });
        return ExprErrors.AlreadyReported;
    }
}

// Copy bytes from a reader to a writer, until EOF
pub fn copyBytes(reader: anytype, writer: anytype) !void {
    var buffered_reader = std.io.bufferedReader(reader);
    var instream = buffered_reader.reader();
    read_loop: while (true) {
        const byte = instream.readByte() catch |err| switch (err) {
            error.EndOfStream => break :read_loop,
            else => return err,
        };
        try writer.writeByte(byte);
    }
}
