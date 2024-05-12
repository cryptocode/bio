//! Implementation of all built-in Bio functions, symbols and math constants.
//! Adding a built-in to Bio is as simple as adding a public constant or function in this file.

const std = @import("std");
const ast = @import("ast.zig");
const interpreter = @import("interpreter.zig");
const gc = @import("boehm.zig");
const util = @import("util.zig");

const Env = ast.Env;
const Expr = ast.Expr;
const ExprValue = ast.ExprValue;
const ExprType = ast.ExprType;
const ExprErrors = ast.ExprErrors;
const Interpreter = interpreter.Interpreter;
const Intrinsic = ast.Intrinsic;

const isFalsy = util.isFalsy;
const isEmptyList = util.isEmptyList;
const isError = util.isError;
const requireExactArgCount = util.requireExactArgCount;
const requireMinimumArgCount = util.requireMinimumArgCount;
const requireMaximumArgCount = util.requireMaximumArgCount;
const requireType = util.requireType;

// Intrinsic numberic constants
pub const @"std-math-e": f64 = std.math.e;
pub const @"std-math-pi": f64 = std.math.pi;

// Intrinsic symbols
pub const @"#!": void = {};
pub const @"#?": void = {};
pub const @"#f": void = {};
pub const @"#t": void = {};
pub const @"#value": void = {};
pub const @"&break": void = {};
pub const @"&eval": void = {};
pub const @"&mut": void = {};
pub const @"&rest": void = {};
pub const @"if": void = {};
pub const begin: void = {};
pub const cond: void = {};
pub const macroexpand: void = {};
pub const nil: void = {};

/// Tells the interpreter to terminate with an exit code. We don't simply terminate
/// the process here, as we're running with leak detection, but rather sets an exit code
/// which causes the eval loop to exit. This helps stress testing the GC/cleanup logic.
pub fn exit(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var exit_code: u8 = 0;
    if (args.len > 0) {
        // and args[0].val == ExprType.num
        const code_expr = try ev.eval(env, args[0]);
        try requireType(ev, code_expr, ExprType.num);
        exit_code = @as(u8, @intFromFloat(code_expr.val.num));
    }

    ev.exit_code = exit_code;
    return ast.getIntrinsic(.nil);
}

/// Open a file for reading and writing, create it if necessary. This produces an "any"
/// expression, where the value is an opaque pointer that's casted back to its actual
/// type when needed, such as in stdFileReadline.
pub fn @"io.open-file"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const filename_expr = try ev.eval(env, args[0]);
    if (filename_expr.val == ExprType.sym) {
        const file = try gc.allocator().create(std.fs.File);
        file.* = std.fs.cwd().createFile(filename_expr.val.sym, .{ .truncate = false, .read = true }) catch |err| {
            try ev.printErrorFmt(filename_expr, "Could not open file: {s}", .{ast.errString(err)});
            return err;
        };
        var expr = try Expr.create(true);
        expr.val = ExprValue{ .any = @intFromPtr(file) };
        return expr;
    }
    return ast.getIntrinsic(.nil);
}

/// Close a file, and deallocate the associated File object
pub fn @"io.close-file"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const file_ptr = try ev.eval(env, args[0]);
    try requireType(ev, file_ptr, ExprType.any);
    const file = @as(*std.fs.File, @ptrFromInt(file_ptr.val.any));
    file.close();
    return ast.getIntrinsic(.nil);
}

/// Reads a byte from the given file
pub fn @"io.read-byte"(ev: *Interpreter, env: *Env, args: []const *Expr) !*Expr {
    try requireExactArgCount(1, args);
    const file_ptr = try ev.eval(env, args[0]);
    try requireType(ev, file_ptr, ExprType.any);
    const file = @as(*std.fs.File, @ptrFromInt(file_ptr.val.any));
    if (file.reader().readByte()) |byte| {
        return ast.makeAtomByDuplicating(&.{byte});
    } else |e| switch (e) {
        error.EndOfStream => return try ast.makeError(try ast.makeAtomByDuplicating("EOF")),
        else => return try ast.makeError(try ast.makeAtomByDuplicating("Could not read from file")),
    }
}

/// Reads a line from the given file, or from stdin if no argument is given
pub fn @"io.read-line"(ev: *Interpreter, env: *Env, args: []const *Expr) !*Expr {
    var reader: std.fs.File.Reader = std.io.getStdIn().reader();
    if (args.len > 0) {
        try requireExactArgCount(1, args);
        const file_ptr = try ev.eval(env, args[0]);
        try requireType(ev, file_ptr, ExprType.any);
        const file = @as(*std.fs.File, @ptrFromInt(file_ptr.val.any));

        reader = file.reader();
    }

    if (reader.readUntilDelimiterOrEofAlloc(gc.allocator(), '\n', std.math.maxInt(usize))) |maybe| {
        if (maybe) |line| {
            return ast.makeAtomAndTakeOwnership(line);
        } else {
            return try ast.makeError(try ast.makeAtomByDuplicating("EOF"));
        }
    } else |_| {
        return try ast.makeError(try ast.makeAtomByDuplicating("Could not read from file"));
    }
}

/// Appends a line to the file, or to stdout if no argument is given
pub fn @"io.write-line"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(1, args);
    const line_to_write = try ev.eval(env, args[args.len - 1]);
    try requireType(ev, line_to_write, ExprType.sym);

    var writer: std.fs.File.Writer = std.io.getStdOut().writer();
    if (args.len > 1) {
        try requireExactArgCount(2, args);
        const file_ptr = try ev.eval(env, args[0]);
        try requireType(ev, file_ptr, ExprType.any);
        const file = @as(*std.fs.File, @ptrFromInt(file_ptr.val.any));
        try file.seekFromEnd(0);

        writer = file.writer();
    }

    try writer.writeAll(line_to_write.val.sym);
    _ = try writer.write("\n");
    return ast.getIntrinsic(.nil);
}

/// Import and evaluate a Bio file
pub fn import(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const filename_expr = try ev.eval(env, args[0]);
    if (filename_expr.val == ExprType.sym) {
        var out: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        const path = std.fs.realpath(filename_expr.val.sym, &out) catch |err| switch (err) {
            error.FileNotFound => {
                try ev.printErrorFmt(filename_expr, "File not found: {s}", .{filename_expr.val.sym});
                return ExprErrors.AlreadyReported;
            },
            else => return err,
        };

        const file = std.fs.openFileAbsolute(path, .{}) catch |err| {
            try ev.printErrorFmt(filename_expr, "Could not open file: {s}", .{ast.errString(err)});
            return err;
        };
        defer file.close();

        const reader = file.reader();
        var res: *Expr = ast.getIntrinsic(.nil);
        const expr_list = try ast.Parser.parseMultipleExpressionsFromReader(reader, try gc.allocator().dupe(u8, path));
        for (expr_list.val.lst.items) |expr| {
            res = try ev.eval(ev.env, expr);
            if (ev.has_errors) break;
        }
        return res;
    } else {
        return ExprErrors.InvalidArgumentType;
    }
}

pub fn @"std-gc-collect"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, env, args };
    _ = gc.collect(.short);
    return ast.getIntrinsic(.nil);
}

pub fn @"debug-verbose"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ env, args };
    ev.verbose = !ev.verbose;
    const bool_str = if (ev.verbose) "on " else "off";
    try std.io.getStdOut().writer().print("Verbosity is now {s}\n", .{bool_str});
    return ast.getIntrinsic(.nil);
}

pub fn assert(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    if (!(try ev.eval(env, args[0])).isIntrinsic(.@"#t")) {
        return ExprErrors.AssertionFailed;
    }
    return ast.getIntrinsic(.@"#t");
}

/// Renders the expression as a string and returns an owned slice.
/// For now, only newline escapes are done (this should be extended to handle all of them)
fn render(ev: *Interpreter, env: *Env, expr: *Expr) ![]u8 {
    _ = &.{ ev, env };
    const str = try expr.toStringAlloc();
    return try std.mem.replaceOwned(u8, gc.allocator(), str, "\\n", "\n");
}

/// Implements (string expr...), i.e. rendering of expressions as strings
pub fn string(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var builder = std.ArrayList(u8).init(gc.allocator());
    defer builder.deinit();
    const writer = builder.writer();

    for (args) |expr| {
        const value = try ev.eval(env, expr);
        const rendered = try render(ev, env, value);
        try writer.writeAll(rendered);
    }
    return ast.makeAtomByDuplicating(builder.items);
}

/// Implements (print expr...)
pub fn print(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    for (args, 0..) |expr, index| {
        const value = try ev.eval(env, expr);
        const rendered = try render(ev, env, value);
        try std.io.getStdOut().writer().print("{s}", .{rendered});
        if (index + 1 < args.len) {
            try std.io.getStdOut().writer().print(" ", .{});
        }
    }
    return ast.getIntrinsic(.nil);
}

/// Returns the current environment as an expression, allowing the user to make constructs
/// such as modules and object instances.
pub fn self(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, args };
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .env = env };
    return expr;
}

/// Returns the parent environment, or nil if the environment is the root
pub fn @"parent-env"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, args };
    var expr = try Expr.create(true);
    if (env.parent) |p| {
        expr.val = ExprValue{ .env = p };
        return expr;
    }
    return ast.getIntrinsic(.nil);
}

/// Print environments. Runs the GC to minimize the environment listing, unless no-gc is passed.
pub fn @"print-env"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = args;

    // Print out all environments, including which environment is the parent.
    try std.io.getStdOut().writer().print("Environment for {s}: {*}\n", .{ env.name, env });

    var iter = env.map.iterator();
    while (iter.next()) |item| {
        try std.io.getStdOut().writer().writeByteNTimes(' ', 4);
        try std.io.getStdOut().writer().print("{s} = ", .{item.key_ptr.*.val.sym});
        try item.value_ptr.*.print();
        if (ev.verbose) {
            try std.io.getStdOut().writer().print(", env {*}", .{item.value_ptr.*.env});
        }
        try std.io.getStdOut().writer().print("\n", .{});
    }

    // TODO: recursively print parents if first argument is #t
    if (env.parent) |parent| {
        try std.io.getStdOut().writer().writeByteNTimes(' ', 4);
        try std.io.getStdOut().writer().print("Parent environment is {s}: {*}\n", .{ parent.name, parent });
    }

    try std.io.getStdOut().writer().print("\n", .{});
    return ast.getIntrinsic(.nil);
}

/// Like (if), but the branch is chosen based on error state
pub fn @"try"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);

    // Select branch based on the presence of an error
    var branch: usize = 1;
    const result = try ev.eval(env, args[0]);
    if (result.val == ExprType.err) {
        try ev.env.put(@tagName(Intrinsic.@"#!"), result);
        branch += 1;
    } else {
        try ev.env.put(@tagName(Intrinsic.@"#value"), result);
    }

    // The error branch is optional
    if (branch < args.len) {
        return try ev.eval(env, args[branch]);
    } else {
        return ast.getIntrinsic(.nil);
    }
}

/// Create an error expression
pub fn @"error"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    return try ast.makeError(try ev.eval(env, args[0]));
}

/// Ordering, where lists are compared recurisively
fn order_impl(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!std.math.Order {
    try requireExactArgCount(2, args);
    const op1 = args[0];
    const op2 = args[1];

    if (op1.val == ExprType.num and op2.val == ExprType.num) {
        return std.math.order(op1.val.num, op2.val.num);
    } else if (op1.isIntrinsic(.nil) and op2.isIntrinsic(.nil)) {
        return std.math.Order.eq;
    } else if (op1.isIntrinsic(.nil) and op2.isIntrinsic(.@"#f")) {
        return std.math.Order.eq;
    } else if (op1.isIntrinsic(.@"#t") and op2.isIntrinsic(.@"#t")) {
        return std.math.Order.eq;
    } else if (op1.isIntrinsic(.@"#t")) {
        return std.math.Order.lt;
    } else if (op1.isIntrinsic(.@"#f") and isFalsy(op2)) {
        return std.math.Order.eq;
    } else if (op1.isIntrinsic(.@"#f")) {
        return std.math.Order.lt;
    } else if (op1.val == ExprType.sym and op2.val == ExprType.sym) {
        return std.mem.order(u8, op1.val.sym, op2.val.sym);
    } else if (op1.isIntrinsic(.nil)) {
        return if (isEmptyList(op2)) std.math.Order.eq else std.math.Order.lt;
    } else if (op2.isIntrinsic(.nil)) {
        return if (isEmptyList(op1)) std.math.Order.eq else std.math.Order.gt;
    } else if (op1.val == ExprType.lst and op2.val == ExprType.lst) {
        var res = std.math.order(op1.val.lst.items.len, op2.val.lst.items.len);
        if (res == std.math.Order.eq) {
            for (op1.val.lst.items, 0..) |item, index| {
                res = try order_impl(ev, env, &.{ item, op2.val.lst.items[index] });
                if (res != std.math.Order.eq) {
                    return res;
                }
            }
        }
        return res;
    } else {
        return std.math.Order.lt;
    }
}

/// Returns an numeric expression with values -1, 0, 1 to represent <, =, > respectively
pub fn order(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    return switch (try order_impl(ev, env, &.{ try ev.eval(env, args[0]), try ev.eval(env, args[1]) })) {
        std.math.Order.lt => return ast.makeNumExpr(-1),
        std.math.Order.eq => return ast.makeNumExpr(0),
        std.math.Order.gt => return ast.makeNumExpr(1),
    };
}

/// Turn a boolean into #f or #t
fn boolExpr(val: bool) *Expr {
    return if (val) ast.getIntrinsic(.@"#t") else ast.getIntrinsic(.@"#f");
}

/// Check for equality. If the order operation fails, such as incompatiable types, false is returned.
pub fn @"="(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    // return boolExpr((order_impl(ev, env, &.{ try ev.eval(env, args[0]), try ev.eval(env, args[1]) }) catch return ast.getIntrinsic(.@"#f")) == std.math.Order.eq);
    return boolExpr((try order_impl(ev, env, &.{ try ev.eval(env, args[0]), try ev.eval(env, args[1]) })) == std.math.Order.eq);
}

/// Swap the values of two variables in the current or a parent environment
pub fn @"swap!"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    try requireType(ev, args[0], ExprType.sym);
    try requireType(ev, args[1], ExprType.sym);
    const v0 = env.lookup(args[0].val.sym, true) orelse return ExprErrors.InvalidArgumentType;
    const v1 = env.lookup(args[1].val.sym, true) orelse return ExprErrors.InvalidArgumentType;

    try env.put(args[0].val.sym, v1);
    try env.put(args[1].val.sym, v0);
    return ast.getIntrinsic(.nil);
}

/// Returns #t if the two arguments references the same Expr in memory, on other words
/// pointer equality.
/// (^= nil nil) -> #t
pub fn @"^="(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    return boolExpr((try ev.eval(env, args[0])) == (try ev.eval(env, args[1])));
}

/// Compare floats with a small relative epsilon comparison. An optional third argument overrides the tolerance.
/// If the input are symbols, case-insensitive comparison is performed.
pub fn @"~="(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    const op1 = try ev.eval(env, args[0]);
    const op2 = try ev.eval(env, args[1]);
    if (op1.val == ExprType.num and op2.val == ExprType.num) {
        var tolerance: f64 = 1e-7;
        if (args.len == 3) {
            const tolerance_expr = try ev.eval(env, args[2]);
            if (tolerance_expr.val == ExprType.num) {
                tolerance = tolerance_expr.val.num;
            }
        }

        return boolExpr(std.math.approxEqRel(f64, op1.val.num, op2.val.num, tolerance));
    } else if (op1.val == ExprType.sym and op2.val == ExprType.sym) {
        return boolExpr(std.ascii.eqlIgnoreCase(op1.val.sym, op2.val.sym));
    } else {
        return ExprErrors.InvalidArgumentType;
    }
}

pub fn @"number?"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return switch (arg.val) {
        ExprType.num => ast.getIntrinsic(.@"#t"),
        else => ast.getIntrinsic(.@"#f"),
    };
}

pub fn @"symbol?"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return switch (arg.val) {
        ExprType.sym => ast.getIntrinsic(.@"#t"),
        else => ast.getIntrinsic(.@"#f"),
    };
}

pub fn @"list?"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return boolExpr(arg.val == ExprType.lst);
}

pub fn @"hashmap?"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return boolExpr(arg.val == ExprType.map);
}

pub fn @"error?"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return boolExpr(arg.val == ExprType.err);
}

pub fn @"callable?"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return switch (arg.val) {
        ExprType.fun, ExprType.lam, ExprType.mac => ast.getIntrinsic(.@"#t"),
        else => ast.getIntrinsic(.@"#f"),
    };
}

pub fn @"opaque?"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return switch (arg.val) {
        ExprType.any => ast.getIntrinsic(.@"#t"),
        else => ast.getIntrinsic(.@"#f"),
    };
}

/// (gensym) will generate a unique identifier
/// TODO: take an optional prefix, like Racket, e.g. (gensym "apple") -> apple_41
pub fn gensym(ev: *Interpreter, _: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(0, args);
    ev.gensym_seq += 1;
    const sym = try std.fmt.allocPrint(gc.allocator(), "gensym_{d}", .{ev.gensym_seq});
    return ast.makeAtomAndTakeOwnership(sym);
}

/// Renders the expression and wraps it in double quotes, returning a new atom
pub fn @"double-quote"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);

    const rendered = try render(ev, env, arg);
    const double_quoted = try std.fmt.allocPrint(gc.allocator(), "\"{s}\"", .{rendered});
    return ast.makeAtomAndTakeOwnership(double_quoted);
}

/// Returns the first argument unevaluated. Multiple arguments is an error,
/// though the argument may be a list. (quote (1 2 3)) is the same as '(1 2 3)
pub fn quote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, env };
    try requireExactArgCount(1, args);

    // We must make a fresh copy for quoted lists. Consider (var lst '(1 2 3)) in a lambda. If not making a copy,
    // the list is memoized between calls (by the fact that the parser creates the list once).
    // This is not a problem for (list 1 2 3) since the list function per definition creates a new list on every evaluation.
    // Some Lisps takes the stance that quoted lists are read-only and may or may not return a copy. In Bio,
    // quoted lists are guaranteed to be fresh shallow copies, and can thus be safely modified.
    if (args[0].val == ExprType.lst) {
        return try ast.makeListExpr(args[0].val.lst.items);
    }
    return args[0];
}

/// Unquote is only useful in combination with quasiquoting, see stdQuasiQuote
pub fn unquote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, env, args };
    return ExprErrors.NotInQuasiQuote;
}

/// Unquote with splicing is only useful in combination with quasiquoting, see stdQuasiQuote
pub fn @"unquote-splicing"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, env, args };
    return ExprErrors.NotInQuasiQuote;
}

/// Recursive quasi-quote expansion
///   (quasiquote (1 2 (unquote (+ 1 2)) 4))           -> '(1 2 3 4)
///   (quasiquote (1 2 (+ 1 2) 4))                     -> '(1 2 (+ 1 2) 4)
///   (quasiquote (1 (unquote-splicing (list 2 3)) 4)) -> '(1 2 3 4)
///   `(1 ,@(list 2 3) 4)                              -> '(1 2 3 4)
pub fn quasiquote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);

    const qq_expander = struct {
        fn expand(ev_inner: *Interpreter, env_inner: *Env, expr: *Expr) anyerror!*Expr {
            if (expr.val == ExprType.lst) {
                var result_list = try ast.makeListExpr(null);
                for (expr.val.lst.items) |item| {
                    // May encounter empty lists, such as lambda ()
                    if (item.val == ExprType.lst and item.val.lst.items.len > 0) {
                        if (item.val.lst.items[0].isIntrinsic(.unquote)) {
                            try result_list.val.lst.append(try ev_inner.eval(env_inner, item.val.lst.items[1]));
                            // } else if (item.val.lst.items[0] == &expr_atom_unquote_splicing) {
                        } else if (item.val.lst.items[0].isIntrinsic(.@"unquote-splicing")) {
                            const splicing_list = try ev_inner.eval(env_inner, item.val.lst.items[1]);
                            try requireType(ev_inner, splicing_list, ExprType.lst);
                            for (splicing_list.val.lst.items) |list_item| {
                                try result_list.val.lst.append(list_item);
                            }
                        } else {
                            try result_list.val.lst.append(try expand(ev_inner, env_inner, item));
                        }
                    } else {
                        if (item.val == ExprType.sym and item.isIntrinsic(.unquote)) {
                            return try ev_inner.eval(env_inner, expr.val.lst.items[1]);
                        } else if (item.val == ExprType.sym and item.isIntrinsic(.@"unquote-splicing")) {
                            try ev_inner.printErrorFmt(expr, "unquotes-splice must be called from within a list", .{});
                            return ExprErrors.AlreadyReported;
                        } else {
                            try result_list.val.lst.append(item);
                        }
                    }
                }
                return result_list;
            } else {
                return expr;
            }
        }
    };

    if (ev.verbose) {
        try args[0].print();
        try std.io.getStdOut().writer().print("\n ^ quasiquote pre-expand\n", .{});
    }

    const res = try qq_expander.expand(ev, env, args[0]);

    if (ev.verbose) {
        try res.print();
        try std.io.getStdOut().writer().print("\n ^ quasiquote post-expand\n", .{});
    }

    return res;
}

/// Implements (item-at list index) and (item-at symbol index)
/// If index is out of bounds, nil is returned
pub fn @"item-at"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const index_arg = try ev.eval(env, args[0]);
    const container = try ev.eval(env, args[1]);
    try requireType(ev, index_arg, ExprType.num);
    const index = @as(isize, @intFromFloat(index_arg.val.num));
    switch (container.val) {
        ExprType.lst => |*lst| {
            return if (index >= 0 and index < lst.items.len) lst.items[@as(usize, @intCast(index))] else ast.getIntrinsic(.nil);
        },
        ExprType.sym => |sym| {
            if (index >= 0 and index < sym.len) {
                return try ast.makeAtomByDuplicating(&.{sym[@as(usize, @intCast(index))]});
            } else return ast.getIntrinsic(.nil);
        },
        else => return ast.getIntrinsic(.nil),
    }
}

/// Implements list mutation. If the index is out of bounds, the item is appended or prepended accordingly.
/// The previous value is returned, or nil if index was out of bounds.
/// (item-set 4 list newitem)
pub fn @"item-set"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(3, args);
    const indexArg = try ev.eval(env, args[0]);
    const listArg = try ev.eval(env, args[1]);
    const newItem = try ev.eval(env, args[2]);
    try requireType(ev, indexArg, ExprType.num);
    try requireType(ev, listArg, ExprType.lst);

    // Index may be negative to prepend, so we use isize
    const index = @as(isize, @intFromFloat(indexArg.val.num));
    var lst = &listArg.val.lst;
    if (index >= 0 and index < lst.items.len) {
        const old = lst.items[@as(usize, @intCast(index))];
        lst.items[@as(usize, @intCast(index))] = newItem;
        return old;
    } else if (index >= lst.items.len) {
        try lst.append(newItem);
        return ast.getIntrinsic(.nil);
    } else {
        try lst.insert(0, newItem);
        return ast.getIntrinsic(.nil);
    }
}

/// In-place removal of the n'th item. The removed item is returned, or nil if index is out of bounds.
pub fn @"item-remove!"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const indexArg = try ev.eval(env, args[0]);
    const listArg = try ev.eval(env, args[1]);
    try requireType(ev, indexArg, ExprType.num);
    try requireType(ev, listArg, ExprType.lst);

    // Index may be negative to prepend, so we use isize
    const index = @as(isize, @intFromFloat(indexArg.val.num));
    var lst = &listArg.val.lst;
    if (index >= 0 and index < lst.items.len) {
        return lst.swapRemove(@as(usize, @intCast(index)));
    }
    return ast.getIntrinsic(.nil);
}

/// In-place rotate a list left by the given amount
/// (rotate-left list amount)
pub fn @"rotate-left!"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const listArg = try ev.eval(env, args[0]);
    const amountArg = try ev.eval(env, args[1]);
    try requireType(ev, listArg, ExprType.lst);
    try requireType(ev, amountArg, ExprType.num);

    std.mem.rotate(*Expr, listArg.val.lst.items, @as(usize, @intFromFloat(amountArg.val.num)));
    return listArg;
}

/// Implements (range list start? end?) where negative indices are end-relative
/// If both start and end are missing, return the first element as in (car list)
/// For range results, this produces a new list
pub fn range(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(1, args);
    const listArg = try ev.eval(env, args[0]);
    try requireType(ev, listArg, ExprType.lst);
    const lst = &listArg.val.lst;
    const size = @as(isize, @intCast(lst.items.len));

    if (args.len > 1) {
        const startArg = try ev.eval(env, args[1]);
        try requireType(ev, startArg, ExprType.num);

        var start = @as(isize, @intFromFloat(startArg.val.num));
        var end = val: {
            if (args.len > 2) {
                const endArg = try ev.eval(env, args[2]);
                try requireType(ev, endArg, ExprType.num);
                break :val @min(@as(isize, @intCast(lst.items.len)), @as(isize, @intFromFloat(endArg.val.num)));
            } else break :val @as(isize, @intCast(lst.items.len));
        };

        if (start < 0) {
            start = size + start;
        }
        if (end < 0) {
            end = size + end;
        }
        var res = try ast.makeListExpr(null);
        if (size > 0 and end > 0 and start >= 0 and start < size and end <= size) {
            try res.val.lst.appendSlice(lst.items[@as(usize, @intCast(start))..@as(usize, @intCast(end))]);
            return res;
        } else {
            return ast.getIntrinsic(.nil);
        }
    } else {
        return if (size > 0) lst.items[0] else ast.getIntrinsic(.nil);
    }
}

/// Logical and with short-circuit
pub fn @"and"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    for (args) |expr| {
        const arg = try ev.eval(env, expr);
        if (arg.isIntrinsic(.@"#t")) continue;
        if (arg.isIntrinsic(.@"#f")) return ast.getIntrinsic(.@"#f");
        try ev.printErrorFmt(expr, "Logical and requires boolean expressions", .{});
        return ExprErrors.AlreadyReported;
    }

    return ast.getIntrinsic(.@"#t");
}

/// Logical and with short-circuit
pub fn @"or"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    for (args) |expr| {
        const arg = try ev.eval(env, expr);
        if (arg.isIntrinsic(.@"#f")) continue;
        if (arg.isIntrinsic(.@"#t")) return ast.getIntrinsic(.@"#t");
        try ev.printErrorFmt(expr, "Logical and requires boolean expressions", .{});
        return ExprErrors.AlreadyReported;
    }

    return ast.getIntrinsic(.@"#f");
}

/// Logical not
pub fn not(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);

    if (isFalsy(arg)) return ast.getIntrinsic(.@"#t");
    return ast.getIntrinsic(.@"#f");
}

pub fn @"+"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var sum: f64 = 0;
    for (args) |expr| {
        const arg = try ev.eval(env, expr);
        switch (arg.val) {
            ExprType.num => |num| sum += num,
            else => return ExprErrors.ExpectedNumber,
        }
    }

    return ast.makeNumExpr(sum);
}

pub fn @"-"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var res: f64 = 0;
    for (args, 0..) |expr, index| {
        const arg = try ev.eval(env, expr);
        switch (arg.val) {
            ExprType.num => |num| {
                // In the unary case, 0 is the implicit first operand
                if (index == 0 and args.len > 1) {
                    res = num;
                } else {
                    res -= num;
                }
            },
            else => return ExprErrors.ExpectedNumber,
        }
    }

    return ast.makeNumExpr(res);
}

pub fn @"*"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var sum: f64 = 1;
    for (args) |expr| {
        const arg = try ev.eval(env, expr);
        switch (arg.val) {
            ExprType.num => |num| sum *= num,
            else => return ExprErrors.ExpectedNumber,
        }
    }

    return ast.makeNumExpr(sum);
}

pub fn @"/"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var res: f64 = 0;
    for (args, 0..) |expr, index| {
        const arg = try ev.eval(env, expr);
        switch (arg.val) {
            ExprType.num => |num| {
                if (index == 0) {
                    res = num;
                } else {
                    if (num == 0) {
                        try ev.printErrorFmt(expr, "Division by zero", .{});
                        return ast.getIntrinsic(.nil);
                    }
                    res /= num;
                }
            },
            else => return ExprErrors.ExpectedNumber,
        }
    }

    return ast.makeNumExpr(res);
}

pub fn @"std-math-pow"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const base = try ev.eval(env, args[0]);
    const exp = try ev.eval(env, args[1]);
    try requireType(ev, base, ExprType.num);
    try requireType(ev, exp, ExprType.num);
    return ast.makeNumExpr(std.math.pow(f64, base.val.num, exp.val.num));
}

pub fn @"std-time-now"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, env, args };
    return ast.makeNumExpr(@as(f64, @floatFromInt(std.time.milliTimestamp())));
}

/// Checks for presence by value comparison in hashmaps, lists and symbols; returns #t or #f accordingly
/// (contains? hashmap key)
/// (contains? list item)
/// (contains? somestring 'w)
pub fn @"contains?"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const expr = try ev.eval(env, args[0]);
    const needle = try ev.eval(env, args[1]);
    if (expr.isIntrinsic(.nil)) return try ast.makeNumExpr(@as(f64, @floatFromInt(0)));
    switch (expr.val) {
        ExprType.sym => return if (std.mem.indexOf(u8, expr.val.sym, needle.val.sym)) |_| ast.getIntrinsic(.@"#t") else ast.getIntrinsic(.@"#f"),
        ExprType.lst => {
            for (expr.val.lst.items) |item| {
                if ((try order_impl(ev, env, &.{ needle, item })) == std.math.Order.eq) return ast.getIntrinsic(.@"#t");
            }
            return ast.getIntrinsic(.@"#f");
        },
        ExprType.map => {
            return if (expr.val.map.get(needle)) |_| ast.getIntrinsic(.@"#t") else ast.getIntrinsic(.@"#f");
        },
        else => {
            try ev.printErrorFmt(expr, "len function only works on lists, maps and symbols", .{});
            return ast.getIntrinsic(.@"#f");
        },
    }
}

/// Returns the length of a list or symbol, otherwise nil
/// If input is nil, 0 is returned
pub fn len(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const expr = try ev.eval(env, args[0]);
    if (expr.isIntrinsic(.nil)) return try ast.makeNumExpr(@as(f64, @floatFromInt(0)));
    switch (expr.val) {
        ExprType.sym => return try ast.makeNumExpr(@as(f64, @floatFromInt(expr.val.sym.len))),
        ExprType.lst => {
            return try ast.makeNumExpr(@as(f64, @floatFromInt(expr.val.lst.items.len)));
        },
        ExprType.map => {
            return try ast.makeNumExpr(@as(f64, @floatFromInt(expr.val.map.count())));
        },
        else => {
            try ev.printErrorFmt(expr, "len function only works on lists, maps and symbols", .{});
            return ast.getIntrinsic(.nil);
        },
    }
}

/// Splits an atom's constituents into a list
/// (atom.split 123)  -> '(1 2 3)
/// (atom.split 'abc) -> '(a b c)
/// (atom.split "a string") -> (list "a" " " "s" "t" "r" "i" "n" "g")
pub fn @"atom.split"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const expr = try ev.eval(env, args[0]);
    switch (expr.val) {
        ExprType.sym => {
            const lst = try ast.makeListExpr(null);
            for (expr.val.sym) |item| {
                try lst.val.lst.append(try ast.makeAtomByDuplicating(&.{item}));
            }
            return lst;
        },
        ExprType.num => {
            @panic("Splitting numbers not support yet");
        },
        else => return ast.getIntrinsic(.nil),
    }
}

/// Convert between symbols, numbers and lists. Example: (as number (io.read-line))
/// Returns nil if the conversion fails
pub fn as(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const target_type = args[0];
    const expr = try ev.eval(env, args[1]);
    try requireType(ev, target_type, ExprType.sym);
    if (std.mem.eql(u8, target_type.val.sym, "number")) {
        switch (expr.val) {
            ExprType.num => return expr,
            ExprType.sym => {
                const float = std.fmt.parseFloat(f64, expr.val.sym) catch {
                    return ast.getIntrinsic(.nil);
                };
                return try ast.makeNumExpr(float);
            },
            else => return ast.getIntrinsic(.nil),
        }
    } else if (std.mem.eql(u8, target_type.val.sym, "symbol")) {
        switch (expr.val) {
            ExprType.sym => return expr,
            ExprType.num => |num| {
                const val = try std.fmt.allocPrint(gc.allocator(), "{d}", .{num});
                return ast.makeSymbol(val, true);
            },
            ExprType.lst => |lst| {
                var res = std.ArrayList(u8).init(gc.allocator());
                var exprWriter = res.writer();
                defer res.deinit();
                for (lst.items) |item| {
                    const str = try item.toStringAlloc();
                    try exprWriter.writeAll(str);
                }
                return ast.makeAtomByDuplicating(res.items);
            },
            else => return ast.getIntrinsic(.nil),
        }
    } else if (std.mem.eql(u8, target_type.val.sym, "list")) {
        if (expr.val == ExprType.lst) {
            return expr;
        }
        const lst = try ast.makeListExpr(null);
        try lst.val.lst.append(expr);
        return lst;
    } else {
        try ev.printErrorFmt(expr, "Invalid target type in (as): {s}. Must be number, symbol or list", .{target_type.val.sym});
        return ast.getIntrinsic(.nil);
    }
}

/// Split a symbol into a list of symbols by splitting on one or more a separators
/// (split "a,b,c;d" ",;") => '(a b c d)
pub fn @"string.split"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const input = try ev.eval(env, args[0]);
    const needle = try ev.eval(env, args[1]);
    try requireType(ev, input, ExprType.sym);
    try requireType(ev, needle, ExprType.sym);

    const lst = try ast.makeListExpr(null);
    var it = std.mem.tokenize(u8, input.val.sym, needle.val.sym);
    while (it.next()) |item| {
        if (item.len == 0) {
            try lst.val.lst.append(ast.getIntrinsic(.nil));
        } else {
            try lst.val.lst.append(try ast.makeAtomByDuplicating(item));
        }
    }

    return lst;
}

pub fn @"std-math-floor"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);
    try requireType(ev, arg, ExprType.num);
    return ast.makeNumExpr(@floor(arg.val.num));
}

pub fn @"std-math-round"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);
    try requireType(ev, arg, ExprType.num);
    return ast.makeNumExpr(@round(arg.val.num));
}

fn minMax(ev: *Interpreter, env: *Env, args: []const *Expr, use_order: std.math.Order) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);
    try requireType(ev, arg, ExprType.lst);
    if (arg.val.lst.items.len == 0) {
        return ast.getIntrinsic(.nil);
    }
    var winner: *Expr = arg.val.lst.items[0];
    for (arg.val.lst.items[1..]) |item| {
        if (use_order == try order_impl(ev, env, &.{ try ev.eval(env, winner), try ev.eval(env, item) })) {
            winner = item;
        }
    }

    return winner;
}

/// Find the smallest value in a list.
/// IF the list is empty, nil is returned.
pub fn @"std-list-min-item"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    return minMax(ev, env, args, std.math.Order.gt);
}

/// Find the largest value in a list.
/// IF the list is empty, nil is returned.
pub fn @"std-list-max-item"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    return minMax(ev, env, args, std.math.Order.lt);
}

/// This is called when a lambda is defined, not when it's invoked
/// The first argument must be a list, namely the lambda arguments
pub fn lambda(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    try requireType(ev, args[0], ExprType.lst);

    var expr = try ast.makeLambdaExpr(env);
    try expr.val.lam.appendSlice(args);
    return expr;
}

/// This is called when a macro is defined
/// The first argument must be a list, namely the macro arguments
pub fn macro(ev: *Interpreter, _: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    try requireType(ev, args[0], ExprType.lst);

    var expr = try ast.makeMacroExpr();
    try expr.val.mac.appendSlice(args);
    return expr;
}

/// Evaluate the arguments, returning the last one as the result. If quote and quasiquote
/// expressions are encountered, these are unquoted before evaluation.
pub fn eval(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var res: *Expr = ast.getIntrinsic(.nil);
    for (args) |arg| {
        if (arg.val == ExprType.lst and
            (arg.val.lst.items[0].isIntrinsic(.quote) or arg.val.lst.items[0].isIntrinsic(.quasiquote) or arg.val.lst.items[0].isIntrinsic(.macroexpand)))
        {
            res = try ev.eval(env, try ev.eval(env, arg));
        } else {
            res = try ev.eval(env, arg);
        }
    }
    return res;
}

/// Parses and evaluates a string (that is, a symbol containing Bio source code) representing one or more expressions.
/// The argument can be a literal source string, or an expression producing a source string
pub fn @"eval-string"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(1, args);
    const arg = args[0];
    const input = expr: {
        if (arg.val == ExprType.lst) {
            if (arg.val == ExprType.lst and (arg.val.lst.items[0].isIntrinsic(.quote) or arg.val.lst.items[0].isIntrinsic(.quasiquote))) {
                break :expr try ev.eval(env, try ev.eval(env, arg));
            } else {
                break :expr try ev.eval(env, arg);
            }
        } else if (arg.val == ExprType.sym) {
            if (env.lookup(arg.val.sym, true)) |looked_up| {
                break :expr looked_up;
            } else {
                break :expr arg;
            }
        } else {
            break :expr arg;
        }
    };

    if (input.val == ExprType.sym) {
        var res = ast.getIntrinsic(.nil);
        const expr_list = try ast.Parser.parseMultipleExpressions(input.val.sym, null);
        for (expr_list.val.lst.items) |expr| {
            res = try ev.eval(env, expr);
            if (ev.has_errors) break;
        }
        return res;
        // return try ev.eval(env, try ev.parse(input.val.sym));
    } else {
        return input;
    }
}

/// (apply fn arg1 ... args) where the last argument must be a list, which is the common contract in Lisps
/// Behaves as if fn is called with arguments from the list produced by (append (list arg1 ...) args)
pub fn apply(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);

    const last_arg_list = try ev.eval(env, args[args.len - 1]);
    try requireType(ev, last_arg_list, ExprType.lst);
    var fncall = try ast.makeListExpr(null);
    try fncall.val.lst.append(args[0]);

    if (args.len > 2) {
        for (args[1 .. args.len - 1]) |arg| {
            try fncall.val.lst.append(arg);
        }
    }

    for (last_arg_list.val.lst.items) |item| {
        try fncall.val.lst.append(item);
    }

    return try ev.eval(env, fncall);
}

/// Create a new list: (list 1 2 3 'a (* 3 x))
pub fn list(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var lst = try ast.makeListExpr(null);
    for (args) |arg| {
        try lst.val.lst.append(try ev.eval(env, arg));
    }
    return lst;
}

/// True if all bytes in an symbol, when considered ascii characters, are lowercase
pub fn @"std-string-lowercase?"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const sym = try ev.eval(env, args[0]);
    try requireType(ev, sym, ExprType.sym);

    for (sym.val.sym) |c| {
        if (std.ascii.isUpper(c)) return ast.getIntrinsic(.@"#f");
    }
    return ast.getIntrinsic(.@"#t");
}

/// True if all bytes in an symbol, when considered ascii characters, are uppercase
pub fn @"std-string-uppercase?"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const sym = try ev.eval(env, args[0]);
    try requireType(ev, sym, ExprType.sym);

    for (sym.val.sym) |c| {
        if (std.ascii.isLower(c)) return ast.getIntrinsic(.@"#f");
    }
    return ast.getIntrinsic(.@"#t");
}

/// Returns a copy of a symbol with each byte ascii-lowercased
pub fn @"std-string-lowercase"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const sym = try ev.eval(env, args[0]);
    try requireType(ev, sym, ExprType.sym);
    const result = try std.ascii.allocLowerString(gc.allocator(), sym.val.sym);
    return try ast.makeAtomAndTakeOwnership(result);
}

/// Returns a copy of a symbol with each byte ascii-uppercased
pub fn @"std-string-uppercase"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const sym = try ev.eval(env, args[0]);
    try requireType(ev, sym, ExprType.sym);
    const result = try std.ascii.allocUpperString(gc.allocator(), sym.val.sym);
    return try ast.makeAtomAndTakeOwnership(result);
}

/// Create a new hashmap: (std-hashmap-new (1 2) (a 3)))
pub fn @"std-hashmap-new"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var hmap = try ast.makeHashmapExpr(null);
    for (args) |arg| {
        try requireType(ev, arg, ExprType.lst);
        try hmap.val.map.put(try ev.eval(env, arg.val.lst.items[0]), try ev.eval(env, arg.val.lst.items[1]));
    }
    return hmap;
}

/// Returns the keys of the hashmap as a list
pub fn @"std-hashmap-keys"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    var container = try ev.eval(env, args[0]);
    var keylist = try ast.makeListExpr(null);
    for (container.val.map.keys()) |k| {
        try keylist.val.lst.append(k);
    }
    return keylist;
}

/// (map.put mymap 1 "abc")
/// Returns the previous value if present, or nil
pub fn @"std-hashmap-put"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(3, args);
    const m = try ev.eval(env, args[0]);
    try requireType(ev, m, ExprType.map);
    const k = try ev.eval(env, args[1]);
    const v = try ev.eval(env, args[2]);
    const previous = m.val.map.get(k);
    try m.val.map.put(k, v);
    return if (previous) |p| p else ast.getIntrinsic(.nil);
}

/// (map.get mymap 1)
/// Returns the matching value, otherwise nil
pub fn @"std-hashmap-get"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const m = try ev.eval(env, args[0]);
    try requireType(ev, m, ExprType.map);
    const k = try ev.eval(env, args[1]);
    const v = m.val.map.get(k);
    return if (v) |val| val else ast.getIntrinsic(.nil);
}

/// (map.remove mymap 1)
/// Returns #t if the entry existed and was removed, otherwise #f
pub fn @"std-hashmap-remove"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const m = try ev.eval(env, args[0]);
    try requireType(ev, m, ExprType.map);
    const k = try ev.eval(env, args[1]);
    return boolExpr(m.val.map.swapRemove(k));
}

/// Removes all items and returns the number of items removed
pub fn @"std-hashmap-clear"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const m = try ev.eval(env, args[0]);
    try requireType(ev, m, ExprType.map);
    const count = try ast.makeNumExpr(@as(f64, @floatFromInt(m.val.map.count())));
    m.val.map.clearAndFree();
    return count;
}

/// Clones the expression
/// NOTE: Currently, only hashmaps are supported
pub fn clone(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const m = try ev.eval(env, args[0]);
    try requireType(ev, m, ExprType.map);

    var hmap = try ast.makeHashmapExpr(null);
    hmap.val.map = try m.val.map.clone();
    return hmap;
}

/// Loop from n to m or until &break is encountered
/// (loop '(0 10) body goes here)      -> loops 10 times
/// (loop '(10 0) body goes here)      -> loops 10 times
/// (loop 'idx '(10 0) body goes here) -> loops 10 times, current iteration count goes into the idx variable
/// (loop '() body goes here (if cond &break)) -> loops until &break is encountered
pub fn loop(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    // loop_arg1 is either a loop index variable name (a symbol we don't look up), or the criteria list
    // If the first, then use stdDefine, i.e same as (var idx 0)
    const loop_arg1 = try ev.eval(env, args[0]);
    var critera: ?*Expr = null;
    var index_variable: ?*Expr = null;
    var index_increment: f64 = 1;

    if (loop_arg1.val == ExprType.lst) {
        critera = try ev.eval(env, args[0]);
    } else if (loop_arg1.val == ExprType.sym) {
        const num_expr = try ast.makeNumExpr(0);
        index_variable = try putEnv(ev, env, &.{ loop_arg1, num_expr }, true);

        critera = try ev.eval(env, args[1]);
        try requireType(ev, critera.?, ExprType.lst);
    } else {
        return ExprErrors.InvalidArgumentType;
    }

    // Criteria is a list of 2 items, giving start and stop indices, or an empty list for infinite loops
    if (critera.?.val.lst.items.len > 0) try requireMinimumArgCount(2, critera.?.val.lst.items);

    var start: f64 = 0;
    var end: f64 = 0;
    var infinite = true;

    if (critera.?.val.lst.items.len > 0) {
        infinite = false;
        try requireMinimumArgCount(2, critera.?.val.lst.items);
        const first = try ev.eval(env, critera.?.val.lst.items[0]);
        const second = try ev.eval(env, critera.?.val.lst.items[1]);
        try requireType(ev, first, ExprType.num);
        try requireType(ev, second, ExprType.num);
        start = @min(first.val.num, second.val.num);
        end = @max(first.val.num, second.val.num);

        // Countdown?
        if (first.val.num > second.val.num) {
            if (index_variable) |iv| {
                index_increment = -1;
                iv.val.num = end - 1;
            }
        }
    }

    var last: *Expr = ast.getIntrinsic(.nil);
    done: while (infinite or (start < end)) : (start += 1) {
        if (ev.exit_code) |_| break;
        // Evaluate loop body
        for (args[1..]) |item| {
            if (ev.exit_code) |_| break;
            last = try ev.eval(env, item);
            if (ev.break_seen) {
                ev.break_seen = false;
                break :done;
            }
        }

        if (index_variable) |iv| {
            iv.val.num += index_increment;
        }
    }

    return last;
}

/// Creates a new list, or updates an existing one if used with &mut, and populates it with the arguments.
/// If any arguments are lists, their elements are spliced into the result list. nil arguments are ignored.
/// (append '(1 2) '(3 4)) -> (1 2 3 4)
/// (append '(1 2 3) '4) -> (1 2 3 4)
/// (append &mut mylist '(3 4))
pub fn append(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);

    const Handler = struct {
        pub fn handle(ip: *Interpreter, environment: *Env, lst: *Expr, arg: *Expr) !void {
            const evaled = try ip.eval(environment, arg);
            if (evaled.val == ExprType.lst) {
                for (evaled.val.lst.items) |item| {
                    try lst.val.lst.append(item);
                }
            } else {
                const expr = try ip.eval(environment, arg);
                if (!expr.isNil()) {
                    try lst.val.lst.append(expr);
                }
            }
        }
    };

    var target_list: ?*Expr = null;
    var start_index: usize = 0;
    if (args[0].isIntrinsic(.@"&mut")) {
        target_list = try ev.eval(env, args[1]);
        if (target_list.?.val == ExprType.lst) {
            start_index = 2;
        } else {
            target_list = try ast.makeListExpr(null);
            start_index = 1;
        }
    } else {
        target_list = try ast.makeListExpr(null);
    }

    for (args[start_index..]) |arg| {
        try Handler.handle(ev, env, target_list.?, arg);
    }
    return target_list.?;
}

/// Put a variable into the given environment. If the symbol is already in the environment, the
/// `allow_redefinition` flag decides between overwritting and emitting an error.
fn putEnv(ev: *Interpreter, env: *Env, args: []const *Expr, allow_redefinition: bool) anyerror!*Expr {
    // Non-recursive lookup, because Bio allows shadowing
    if (env.lookup(args[0].val.sym, false) != null and !allow_redefinition) {
        try ev.printErrorFmt(args[0], "{s} is already defined", .{args[0].val.sym});
        return ExprErrors.AlreadyReported;
    }
    if (args.len > 1) {
        const value = try ev.eval(env, args[1]);
        try env.putWithSymbol(args[0], value);
        return value;
    } else {
        try env.putWithSymbol(args[0], ast.getIntrinsic(.nil));
        return ast.getIntrinsic(.nil);
    }
}

/// Adds a new binding to the current environment
pub fn define(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMaximumArgCount(2, args);
    try requireType(ev, args[0], ExprType.sym);
    return try putEnv(ev, env, args, false);
}

/// Define new variables through destructuring a list. Evaluates to the list expression.
/// (vars a b c '(1 2 3))
pub fn vars(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    const lst = try ev.eval(env, args[args.len - 1]);
    try requireType(ev, lst, ExprType.lst);

    if (lst.val.lst.items.len != args.len - 1) {
        try ev.printErrorFmt(args[0], "vars variable count {d} does not match list size {d}", .{ args.len - 1, lst.val.lst.items.len });
        return ExprErrors.AlreadyReported;
    }

    for (lst.val.lst.items, 0..) |item, i| {
        try env.putWithSymbol(args[i], item);
    }

    return lst;
}

/// Replace binding in the current environment, or another environment if specified.
/// In the current environment:
///   (set! mylocal 4)
/// In a different environment:
///   (set! window width 50)
pub fn @"set!"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    if (args.len == 2) {
        try requireType(ev, args[0], ExprType.sym);
        return env.replace(args[0], try ev.eval(env, args[1])) catch |err| {
            try ev.printErrorFmt(args[0], "{s}: Could not set variable {s}. Make sure it is defined in the active environment.", .{ ast.errString(err), args[0].val.sym });
            return ExprErrors.AlreadyReported;
        };
    } else if (args.len == 3) {
        try requireType(ev, args[1], ExprType.sym);
        var target_env = try ev.eval(env, args[0]);
        try requireType(ev, target_env, ExprType.env);
        return target_env.val.env.replace(args[1], try ev.eval(env, args[2])) catch |err| {
            try ev.printErrorFmt(args[0], "{s}: Could not set variable {s}. Make sure it is defined in the active environment.", .{ ast.errString(err), args[0].val.sym });
            return ExprErrors.AlreadyReported;
        };
    } else {
        return ExprErrors.InvalidArgumentCount;
    }
}

/// Remove binding if it exists
pub fn @"unset!"(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    try requireType(ev, args[0], ExprType.sym);
    _ = env.replace(args[0], null) catch return ast.getIntrinsic(.nil);
    return ast.getIntrinsic(.nil);
}
