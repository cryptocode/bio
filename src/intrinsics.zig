const std = @import("std");
usingnamespace @import("ast.zig");
usingnamespace @import("interpreter.zig");
usingnamespace @import("gc.zig");
usingnamespace @import("sourcelocation.zig");

// Intrinsic symbol- and function expressions. These expressions are not registered with
// the GC and are thus considered pinned and never attemped deallocated.
pub var expr_atom_last_eval = Expr{ .val = ExprValue{ .sym = "#?" } };
pub var expr_atom_last_try_err = Expr{ .val = ExprValue{ .sym = "#!" } };
pub var expr_atom_last_try_value = Expr{ .val = ExprValue{ .sym = "#value" } };
pub var expr_atom_quote = Expr{ .val = ExprValue{ .sym = "quote" } };
pub var expr_atom_quasi_quote = Expr{ .val = ExprValue{ .sym = "quasiquote" } };
pub var expr_atom_unquote = Expr{ .val = ExprValue{ .sym = "unquote" } };
pub var expr_atom_unquote_splicing = Expr{ .val = ExprValue{ .sym = "unquote-splicing" } };
pub var expr_atom_list = Expr{ .val = ExprValue{ .sym = "list" } };
pub var expr_atom_if = Expr{ .val = ExprValue{ .sym = "if" } };
pub var expr_atom_cond = Expr{ .val = ExprValue{ .sym = "cond" } };
pub var expr_atom_begin = Expr{ .val = ExprValue{ .sym = "begin" } };
pub var expr_atom_false = Expr{ .val = ExprValue{ .sym = "#f" } };
pub var expr_atom_true = Expr{ .val = ExprValue{ .sym = "#t" } };
pub var expr_atom_nil = Expr{ .val = ExprValue{ .sym = "nil" } };
pub var expr_atom_rest = Expr{ .val = ExprValue{ .sym = "&rest" } };
pub var expr_std_math_pi = Expr{ .val = ExprValue{ .num = std.math.pi } };
pub var expr_std_math_e = Expr{ .val = ExprValue{ .num = std.math.e } };
pub var expr_std_import = Expr{ .val = ExprValue{ .fun = stdImport } };
pub var expr_std_exit = Expr{ .val = ExprValue{ .fun = stdExit } };
pub var expr_std_verbose = Expr{ .val = ExprValue{ .fun = stdVerbose } };
pub var expr_std_assert_true = Expr{ .val = ExprValue{ .fun = stdAssertTrue } };
pub var expr_std_is_number = Expr{ .val = ExprValue{ .fun = stdIsNumber } };
pub var expr_std_is_symbol = Expr{ .val = ExprValue{ .fun = stdIsSymbol } };
pub var expr_std_is_list = Expr{ .val = ExprValue{ .fun = stdIsList } };
pub var expr_std_is_callable = Expr{ .val = ExprValue{ .fun = stdIsCallable } };
pub var expr_std_gensym = Expr{ .val = ExprValue{ .fun = stdGenSym } };
pub var expr_std_quote = Expr{ .val = ExprValue{ .fun = stdQuote } };
pub var expr_std_unquote = Expr{ .val = ExprValue{ .fun = stdUnquote } };
pub var expr_std_unquote_splicing = Expr{ .val = ExprValue{ .fun = stdUnquoteSplicing } };
pub var expr_std_quasi_quote = Expr{ .val = ExprValue{ .fun = stdQuasiQuote } };
pub var expr_std_double_quote = Expr{ .val = ExprValue{ .fun = stdDoubleQuote } };
pub var expr_std_len = Expr{ .val = ExprValue{ .fun = stdLen } };
pub var expr_std_range = Expr{ .val = ExprValue{ .fun = stdRange } };
pub var expr_std_string = Expr{ .val = ExprValue{ .fun = stdString } };
pub var expr_std_print = Expr{ .val = ExprValue{ .fun = stdPrint } };
pub var expr_std_read_line = Expr{ .val = ExprValue{ .fun = stdReadline } };
pub var expr_std_env = Expr{ .val = ExprValue{ .fun = stdEnv } };
pub var expr_std_self = Expr{ .val = ExprValue{ .fun = stdSelf } };
pub var expr_std_define = Expr{ .val = ExprValue{ .fun = stdDefine } };
pub var expr_std_lambda = Expr{ .val = ExprValue{ .fun = stdLambda } };
pub var expr_std_macro = Expr{ .val = ExprValue{ .fun = stdMacro } };
pub var expr_std_eval_string = Expr{ .val = ExprValue{ .fun = stdEvalString } };
pub var expr_std_eval = Expr{ .val = ExprValue{ .fun = stdEval } };
pub var expr_std_apply = Expr{ .val = ExprValue{ .fun = stdApply } };
pub var expr_std_list = Expr{ .val = ExprValue{ .fun = stdList } };
pub var expr_std_append = Expr{ .val = ExprValue{ .fun = stdAppend } };
pub var expr_std_unset = Expr{ .val = ExprValue{ .fun = stdUnset } };
pub var expr_std_error = Expr{ .val = ExprValue{ .fun = stdError } };
pub var expr_std_try = Expr{ .val = ExprValue{ .fun = stdTry } };
pub var expr_std_set = Expr{ .val = ExprValue{ .fun = stdSet } };
pub var expr_std_sum = Expr{ .val = ExprValue{ .fun = stdSum } };
pub var expr_std_sub = Expr{ .val = ExprValue{ .fun = stdSub } };
pub var expr_std_mul = Expr{ .val = ExprValue{ .fun = stdMul } };
pub var expr_std_div = Expr{ .val = ExprValue{ .fun = stdDiv } };
pub var expr_std_pow = Expr{ .val = ExprValue{ .fun = stdPow } };
pub var expr_std_time_now = Expr{ .val = ExprValue{ .fun = stdTimeNow } };
pub var expr_std_floor = Expr{ .val = ExprValue{ .fun = stdFloor } };
pub var expr_std_as = Expr{ .val = ExprValue{ .fun = stdAs } };
pub var expr_std_order = Expr{ .val = ExprValue{ .fun = stdOrder } };
pub var expr_std_eq = Expr{ .val = ExprValue{ .fun = stdEq } };
pub var expr_std_eq_approx = Expr{ .val = ExprValue{ .fun = stdEqApprox } };
pub var expr_std_run_gc = Expr{ .val = ExprValue{ .fun = stdRunGc } };
pub var expr_std_file_open = Expr{ .val = ExprValue{ .fun = stdFileOpen } };
pub var expr_std_file_close = Expr{ .val = ExprValue{ .fun = stdFileClose } };
pub var expr_std_file_read_line = Expr{ .val = ExprValue{ .fun = stdFileReadLine } };
pub var expr_std_file_write_line = Expr{ .val = ExprValue{ .fun = stdFileWriteLine } };

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

pub fn requireType(ev: *Interpreter, expr: *Expr, etype: ExprType) !void {
    if (expr.val != etype) {
        try ev.printErrorFmt(&expr.src, "Expected {}, got argument type: {}, actual value:\n", .{ etype, std.meta.activeTag(expr.val) });
        try expr.print();
        return ExprErrors.AlreadyReported;
    }
}

/// Signals the interpreter to terminate with an exit code. We don't simply terminate
/// the process here, as we're running with leak detection, but rather sets and exit code
/// which causes the eval loop to exit. This helps stress testing the GC/cleanup logic.
pub fn stdExit(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var exit_code: u8 = 0;
    if (args.len > 0 and args[0].val == ExprType.num) {
        exit_code = @floatToInt(u8, args[0].val.num);
    }

    ev.exit_code = exit_code;
    return &expr_atom_nil;
}

/// Open a file for reading and writing, create it if necessary. This produces an "any"
/// expression, where the value is an opaque pointer that's casted back to its actual
/// type when needed, such as in stdFileReadline.
pub fn stdFileOpen(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const filename_expr = try ev.eval(env, args[0]);
    if (filename_expr.val == ExprType.sym) {
        var out: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        var path = try std.fs.path.resolve(allocator, &.{filename_expr.val.sym});
        defer allocator.free(path);

        var file = try allocator.create(std.fs.File);
        errdefer allocator.destroy(file);
        file.* = std.fs.createFileAbsolute(path, .{ .truncate = false, .read = true }) catch |err| {
            try ev.printErrorFmt(&filename_expr.src, "Could not open file: {}\n", .{err});
            return err;
        };
        var expr = try Expr.create(true);
        expr.val = ExprValue{ .any = @ptrToInt(file) };
        return expr;
    }
    return &expr_atom_nil;
}

/// Close a file, and deallocate the associated File object
pub fn stdFileClose(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const file_ptr = try ev.eval(env, args[0]);
    try requireType(ev, file_ptr, ExprType.any);
    const file = @intToPtr(*std.fs.File, file_ptr.val.any);
    file.close();
    allocator.destroy(file);
    return &expr_atom_nil;
}

/// Reads a line from the file
pub fn stdFileReadLine(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const file_ptr = try ev.eval(env, args[0]);
    try requireType(ev, file_ptr, ExprType.any);
    const file = @intToPtr(*std.fs.File, file_ptr.val.any);
    if (file.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(usize))) |maybe| {
        if (maybe) |line| {
            return makeAtomAndTakeOwnership(line);
        } else {
            return try makeError(try makeAtomByDuplicating("EOF"));
        }
    } else |err| {
        return try makeError(try makeAtomByDuplicating("Could not read from file"));
    }
}

/// Appends a line to the file
pub fn stdFileWriteLine(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const file_ptr = try ev.eval(env, args[0]);
    const line_to_write = try ev.eval(env, args[1]);
    try requireType(ev, file_ptr, ExprType.any);
    try requireType(ev, line_to_write, ExprType.sym);
    const file = @intToPtr(*std.fs.File, file_ptr.val.any);
    try file.seekFromEnd(0);
    try file.writer().writeAll(line_to_write.val.sym);
    _ = try file.writer().write("\n");
    return &expr_atom_nil;
}

/// Import and evaluate a Bio file
pub fn stdImport(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const filename_expr = try ev.eval(env, args[0]);
    if (filename_expr.val == ExprType.sym) {
        var out: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        var path = std.fs.realpath(filename_expr.val.sym, &out) catch |err| switch (err) {
            std.os.RealPathError.FileNotFound => {
                try ev.printErrorFmt(&filename_expr.src, "File not found: {s}\n", .{filename_expr.val.sym});
                return ExprErrors.AlreadyReported;
            },
            else => return err,
        };
        try SourceLocation.push(path[0..]);
        defer SourceLocation.pop();

        const file = std.fs.openFileAbsolute(path, .{}) catch |err| {
            try ev.printErrorFmt(&filename_expr.src, "Could not open file: {}\n", .{err});
            return err;
        };
        defer file.close();

        const reader = file.reader();
        var res: *Expr = &expr_atom_nil;
        while (!ev.has_errors) {
            if (ev.readBalancedExpr(&reader, "")) |maybe| {
                if (maybe) |input| {
                    defer allocator.free(input);
                    if (try ev.parseAndEvalExpression(input)) |e| {
                        res = e;
                        try ev.env.put("#?", res);
                    }
                } else {
                    break;
                }
            } else |err| {
                try ev.printErrorFmt(&filename_expr.src, "(import) could not read expression: {}\n", .{err});
                break;
            }
        }
        return res;
    } else {
        return ExprErrors.InvalidArgumentType;
    }
}

pub fn stdRunGc(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try gc.run(true);
    return &expr_atom_nil;
}

pub fn stdVerbose(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    ev.verbose = !ev.verbose;
    const bool_str = if (ev.verbose) "on " else "off";
    try std.io.getStdOut().writer().print("Verbosity is now {s}\n", .{bool_str});
    return &expr_atom_nil;
}

pub fn stdAssertTrue(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    if ((try ev.eval(env, args[0])) != &expr_atom_true) {
        try std.io.getStdOut().writer().print("Assertion failed {s} line {d}\n", .{ args[0].src.file, args[0].src.line });
        std.process.exit(0);
    }
    return &expr_atom_true;
}

/// Renders the expression as a string and returns an owned slice.
/// For now, only newline escapes are done (this should be extended to handle all of them)
fn render(ev: *Interpreter, env: *Env, expr: *Expr) ![]u8 {
    const str = try expr.toStringAlloc();
    defer allocator.free(str);
    return try std.mem.replaceOwned(u8, allocator, str, "\\n", "\n");
}

/// Implements (string expr...), i.e. rendering of expressions as strings
pub fn stdString(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var builder = std.ArrayList(u8).init(allocator);
    defer builder.deinit();
    const writer = builder.writer();

    for (args) |expr, index| {
        const value = try ev.eval(env, expr);
        const rendered = try render(ev, env, value);
        defer allocator.free(rendered);
        try writer.writeAll(rendered);
    }
    return makeAtomByDuplicating(builder.items);
}

/// Implements (print expr...)
pub fn stdPrint(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    for (args) |expr, index| {
        const value = try ev.eval(env, expr);
        const rendered = try render(ev, env, value);
        defer allocator.free(rendered);
        try std.io.getStdOut().writer().print("{s}", .{rendered});
        if (index + 1 < args.len) {
            try std.io.getStdOut().writer().print(" ", .{});
        }
    }
    return &expr_atom_nil;
}

/// Implements (readline)
pub fn stdReadline(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    if (try std.io.getStdIn().reader().readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(u32))) |line| {
        return try makeAtomAndTakeOwnership(line);
    }
    return &expr_atom_nil;
}

/// Returns the current environment as an expression, allowing the user to make constructs
/// such as modules and object instances.
pub fn stdSelf(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .env = env };
    return expr;
}

/// Print environments. Runs the GC to minimize the environment listing, unless no-gc is passed.
pub fn stdEnv(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    if (!(args.len > 0 and args[0].val == ExprType.sym and std.mem.eql(u8, args[0].val.sym, "no-gc"))) {
        try gc.run(false);
    }

    // Print out all environments, including which environment is the parent.
    for (gc.registered_envs.items) |registered_env| {
        try std.io.getStdOut().writer().print("Environment for {s}: {*}\n", .{ registered_env.name, registered_env });

        for (registered_env.map.items()) |item| {
            try std.io.getStdOut().writer().writeByteNTimes(' ', 4);
            try std.io.getStdOut().writer().print("{s} = ", .{item.key.val.sym});
            try item.value.print();
            if (ev.verbose) {
                try std.io.getStdOut().writer().print(", env {*}", .{item.value.env});
            }
            try std.io.getStdOut().writer().print("\n", .{});
        }

        if (registered_env.parent) |parent| {
            try std.io.getStdOut().writer().writeByteNTimes(' ', 4);
            try std.io.getStdOut().writer().print("Parent environment is {s}: {*}\n", .{ parent.name, parent });
        }

        try std.io.getStdOut().writer().print("\n", .{});
    }
    return &expr_atom_nil;
}

/// Like (if), but the branch is chosen based on error state
pub fn stdTry(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);

    // Select branch based on the presence of an error
    var branch: usize = 1;
    const result = try ev.eval(env, args[0]);
    if (result.val == ExprType.err) {
        try ev.env.put(expr_atom_last_try_err.val.sym, result);
        branch += 1;
    } else {
        try ev.env.put(expr_atom_last_try_value.val.sym, result);
    }

    // The error branch is optional
    if (branch < args.len) {
        return try ev.eval(env, args[branch]);
    } else {
        return &expr_atom_nil;
    }
}

/// Create an error expression
pub fn stdError(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    return try makeError(try ev.eval(env, args[0]));
}

pub fn isEmptyList(expr: *Expr) bool {
    return (expr.val == ExprType.lst and expr.val.lst.items.len == 0);
}

pub fn isFalsy(expr: *Expr) bool {
    return expr == &expr_atom_false or expr == &expr_atom_nil or isEmptyList(expr);
}

/// Ordering, where lists are compared recurisively
fn order(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!std.math.Order {
    try requireExactArgCount(2, args);
    const op1 = args[0];
    const op2 = args[1];

    if (op1.val == ExprType.num and op2.val == ExprType.num) {
        return std.math.order(op1.val.num, op2.val.num);
    } else if (op1 == &expr_atom_nil and op2 == &expr_atom_nil) {
        return std.math.Order.eq;
    } else if (op1 == &expr_atom_nil and op2 == &expr_atom_false) {
        return std.math.Order.eq;
    } else if (op1 == &expr_atom_true and op2 == &expr_atom_true) {
        return std.math.Order.eq;
    } else if (op1 == &expr_atom_true) {
        return std.math.Order.lt;
    } else if (op1 == &expr_atom_false and (op2 == &expr_atom_false or op2 == &expr_atom_nil)) {
        return std.math.Order.eq;
    } else if (op1 == &expr_atom_false) {
        return std.math.Order.lt;
    } else if (op1.val == ExprType.sym and op2.val == ExprType.sym) {
        return std.mem.order(u8, op1.val.sym, op2.val.sym);
    } else if (op1 == &expr_atom_nil) {
        return if (isEmptyList(op2)) std.math.Order.eq else std.math.Order.lt;
    } else if (op2 == &expr_atom_nil) {
        return if (isEmptyList(op1)) std.math.Order.eq else std.math.Order.gt;
    } else if (op1.val == ExprType.lst and op2.val == ExprType.lst) {
        var res = std.math.order(op1.val.lst.items.len, op2.val.lst.items.len);
        if (res == std.math.Order.eq) {
            for (op1.val.lst.items) |item, index| {
                res = try order(ev, env, &.{ item, op2.val.lst.items[index] });
                if (res != std.math.Order.eq) {
                    return res;
                }
            }
        }
        return res;
    } else {
        return ExprErrors.InvalidArgumentType;
    }
}

/// Returns an numeric expression with values -1, 0, 1 to represent <, =, > respectively
pub fn stdOrder(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    return switch (try order(ev, env, &.{ try ev.eval(env, args[0]), try ev.eval(env, args[1]) })) {
        std.math.Order.lt => return makeNumExpr(-1),
        std.math.Order.eq => return makeNumExpr(0),
        std.math.Order.gt => return makeNumExpr(1),
    };
}

/// Turn a boolean into #f or #t
fn boolExpr(val: bool) *Expr {
    return if (val) &expr_atom_true else &expr_atom_false;
}

/// Check for equality. If the order operation fails, such as incompatiable types, false is returned.
pub fn stdEq(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    return boolExpr((order(ev, env, &.{ try ev.eval(env, args[0]), try ev.eval(env, args[1]) }) catch |_| return &expr_atom_false) == std.math.Order.eq);
}

/// Compare floats with a small relative epsilon comparison. An optional third argument overrides the tolerance.
pub fn stdEqApprox(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    const op1 = try ev.eval(env, args[0]);
    const op2 = try ev.eval(env, args[1]);
    var tolerance: f64 = 1e-7;
    if (args.len == 3) {
        const tolerance_expr = try ev.eval(env, args[2]);
        if (tolerance_expr.val == ExprType.num) {
            tolerance = tolerance_expr.val.num;
        }
    }

    if (op1.val == ExprType.num and op2.val == ExprType.num) {
        return boolExpr(std.math.approxEqRel(f64, op1.val.num, op2.val.num, tolerance));
    } else {
        return ExprErrors.InvalidArgumentType;
    }
}

pub fn stdIsNumber(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return switch (arg.val) {
        ExprType.num => &expr_atom_true,
        else => &expr_atom_false,
    };
}

pub fn stdIsSymbol(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return switch (arg.val) {
        ExprType.sym => &expr_atom_true,
        else => &expr_atom_false,
    };
}

pub fn stdIsList(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return if (arg.val == ExprType.lst) &expr_atom_true else &expr_atom_false;
}

pub fn stdIsCallable(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return switch (arg.val) {
        ExprType.fun, ExprType.lam, ExprType.mac => &expr_atom_true,
        else => &expr_atom_false,
    };
}

/// (gensym) will generate a unique identifier
pub fn stdGenSym(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(0, args);
    ev.gensym_seq += 1;
    const sym = try std.fmt.allocPrint(allocator, "gensym_{d}", .{ev.gensym_seq});
    return makeAtomAndTakeOwnership(sym);
}

/// Renders the expression and wraps it in double quotes, returning a new atom
pub fn stdDoubleQuote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);

    const rendered = try render(ev, env, arg);
    defer allocator.free(rendered);

    const double_quoted = try std.fmt.allocPrint(allocator, "\"{s}\"", .{rendered});
    return makeAtomAndTakeOwnership(double_quoted);
}

/// Returns the first argument unevaluated. Multiple arguments is an error,
/// though the argument may be a list. (quote (1 2 3)) -> '(1 2 3)
pub fn stdQuote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    return args[0];
}

/// Unquote is only useful in combination with quasiquoting, see stdQuasiQuote
pub fn stdUnquote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try ev.printErrorFmt(SourceLocation.current(), "Can only use unquote inside a quasiquote expression\n", .{});
    return ExprErrors.AlreadyReported;
}

/// Unquote with splicing is only useful in combination with quasiquoting, see stdQuasiQuote
pub fn stdUnquoteSplicing(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try ev.printErrorFmt(SourceLocation.current(), "Can only use unquote-splicing inside a quasiquote expression\n", .{});
    return ExprErrors.AlreadyReported;
}

/// Recursive quasi-quote expansion
///   (quasiquote (1 2 (unquote (+ 1 2)) 4)) -> '(1 2 3 4)
///   (quasiquote (1 (unquote-splicing (list 2 3)) 4)) -> '(1 2 3 4)
pub fn stdQuasiQuote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);

    const qq_expander = struct {
        fn expand(ev_inner: *Interpreter, env_inner: *Env, expr: *Expr) anyerror!*Expr {
            if (expr.val == ExprType.lst) {
                var result_list = try makeListExpr(null);
                for (expr.val.lst.items) |item, index| {
                    // May encounter empty lists, such as lambda ()
                    if (item.val == ExprType.lst and item.val.lst.items.len > 0) {
                        if (item.val.lst.items[0] == &expr_atom_unquote) {
                            try result_list.val.lst.append(try ev_inner.eval(env_inner, item.val.lst.items[1]));
                        } else if (item.val.lst.items[0] == &expr_atom_unquote_splicing) {
                            const list = try ev_inner.eval(env_inner, item.val.lst.items[1]);
                            try requireType(ev_inner, list, ExprType.lst);
                            for (list.val.lst.items) |list_item| {
                                try result_list.val.lst.append(list_item);
                            }
                        } else {
                            try result_list.val.lst.append(try expand(ev_inner, env_inner, item));
                        }
                    } else {
                        if (item.val == ExprType.sym and item == &expr_atom_unquote) {
                            return try ev_inner.eval(env_inner, expr.val.lst.items[1]);
                        } else if (item.val == ExprType.sym and item == &expr_atom_unquote_splicing) {
                            try ev_inner.printErrorFmt(&expr.src, "unquotes-splice must be called from within a list\n", .{});
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

/// Implements (range list start? end?) where negative indices are end-relative
/// If both start and end are missing, return the first element as in (car list)
pub fn stdRange(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(1, args);
    const listArg = try ev.eval(env, args[0]);
    try requireType(ev, listArg, ExprType.lst);
    const list = listArg.val.lst;
    const size = @intCast(isize, list.items.len);

    if (args.len > 1) {
        const startArg = try ev.eval(env, args[1]);
        try requireType(ev, startArg, ExprType.num);

        var start = @floatToInt(isize, startArg.val.num);
        var end = val: {
            if (args.len > 2) {
                const endArg = try ev.eval(env, args[2]);
                try requireType(ev, endArg, ExprType.num);
                break :val std.math.min(@intCast(isize, list.items.len), @floatToInt(isize, endArg.val.num));
            } else break :val @intCast(isize, list.items.len);
        };

        if (start < 0) {
            start = size + start;
        }
        if (end < 0) {
            end = size + end;
        }
        var res = try makeListExpr(null);
        if (size > 0 and end > 0 and start >= 0 and start < size and end <= size) {
            try res.val.lst.appendSlice(list.items[@intCast(usize, start)..@intCast(usize, end)]);
            return res;
        } else {
            return &expr_atom_nil;
        }
    } else {
        return if (size > 0) list.items[0] else &expr_atom_nil;
    }
}

pub fn stdSum(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var sum: f64 = 0;
    for (args) |expr| {
        const arg = try ev.eval(env, expr);
        switch (arg.val) {
            ExprType.num => |num| sum += num,
            else => return ExprErrors.ExpectedNumber,
        }
    }

    return makeNumExpr(sum);
}

pub fn stdSub(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var res: f64 = 0;
    for (args) |expr, index| {
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

    return makeNumExpr(res);
}

pub fn stdMul(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var sum: f64 = 1;
    for (args) |expr| {
        const arg = try ev.eval(env, expr);
        switch (arg.val) {
            ExprType.num => |num| sum *= num,
            else => return ExprErrors.ExpectedNumber,
        }
    }

    return makeNumExpr(sum);
}

pub fn stdDiv(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var res: f64 = 0;
    for (args) |expr, index| {
        const arg = try ev.eval(env, expr);
        switch (arg.val) {
            ExprType.num => |num| {
                if (index == 0) {
                    res = num;
                } else {
                    if (num == 0) {
                        try ev.printErrorFmt(&expr.src, "Division by zero\n", .{});
                        return &expr_atom_nil;
                    }
                    res /= num;
                }
            },
            else => return ExprErrors.ExpectedNumber,
        }
    }

    return makeNumExpr(res);
}

pub fn stdPow(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    try requireType(ev, args[0], ExprType.num);
    try requireType(ev, args[1], ExprType.num);
    return makeNumExpr(std.math.pow(f64, args[0].val.num, args[1].val.num));
}

pub fn stdTimeNow(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    return makeNumExpr(@intToFloat(f64, std.time.milliTimestamp()));
}

/// Returns the length of a list or symbol, otherwise nil
pub fn stdLen(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const expr = try ev.eval(env, args[0]);
    switch (expr.val) {
        ExprType.sym => return try makeNumExpr(@intToFloat(f64, expr.val.sym.len)),
        ExprType.lst => {
            std.debug.print("Converting {d} to float\n", .{expr.val.lst.items.len});
            return try makeNumExpr(@intToFloat(f64, expr.val.lst.items.len));
        },
        else => {
            try ev.printErrorFmt(&expr.src, "len function only works on lists and symbols\n", .{});
            return &expr_atom_nil;
        },
    }
}

/// Convert between symbols, numbers and lists, such as (as number (readline))
pub fn stdAs(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const target_type = args[0];
    const expr = try ev.eval(env, args[1]);
    try requireType(ev, target_type, ExprType.sym);
    if (std.mem.eql(u8, target_type.val.sym, "number")) {
        switch (expr.val) {
            ExprType.num => return expr,
            ExprType.sym => return try makeNumExpr(try std.fmt.parseFloat(f64, expr.val.sym)),
            else => return &expr_atom_nil,
        }
    } else if (std.mem.eql(u8, target_type.val.sym, "symbol")) {
        switch (expr.val) {
            ExprType.sym => return expr,
            ExprType.num => |num| {
                const val = try std.fmt.allocPrint(allocator, "{d}", .{num});
                return makeAtomLiteral(val, true);
            },
            ExprType.lst => |lst| {
                var res = std.ArrayList(u8).init(allocator);
                var exprWriter = res.writer();
                defer res.deinit();
                for (lst.items) |item| {
                    const str = try item.toStringAlloc();
                    defer allocator.free(str);
                    try exprWriter.writeAll(str);
                }
                return makeAtomByDuplicating(res.items);
            },
            else => return &expr_atom_nil,
        }
    } else if (std.mem.eql(u8, target_type.val.sym, "list")) {
        if (expr.val == ExprType.lst) {
            return expr;
        }
        const list = try makeListExpr(null);
        try list.val.lst.append(expr);
        return list;
    } else {
        try ev.printErrorFmt(&expr.src, "Invalid target type in (as): {s}. Must be number, symbol or list\n", .{target_type.val.sym});
        return &expr_atom_nil;
    }
}

pub fn stdFloor(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);
    try requireType(ev, arg, ExprType.num);
    return makeNumExpr(@floor(arg.val.num));
}

/// This is called when a lambda is defined, not when it's invoked
/// The first argument must be a list, namely the lambda arguments
pub fn stdLambda(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    try requireType(ev, args[0], ExprType.lst);

    var expr = try makeLambdaExpr(env);
    try expr.val.lam.appendSlice(args);
    return expr;
}

/// This is called when a macro is defined
/// The first argument must be a list, namely the macro arguments
pub fn stdMacro(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    try requireType(ev, args[0], ExprType.lst);

    var expr = try makeMacroExpr();
    try expr.val.mac.appendSlice(args);
    return expr;
}

/// Evaluate the arguments, returning the last one as the result. If quote and quasiquote
/// expressions are encountered, these are unquoted before evaluation.
pub fn stdEval(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var res: *Expr = &expr_atom_nil;
    for (args) |arg| {
        if (arg.val == ExprType.lst and (arg.val.lst.items[0] == &expr_atom_quote or arg.val.lst.items[0] == &expr_atom_quasi_quote)) {
            res = try ev.eval(env, try ev.eval(env, arg));
        } else {
            res = try ev.eval(env, arg);
        }
    }
    return res;
}

/// Parses and evaluates a string (that is, a symbol containing Bio source code)
/// The argument can be a literal source string, or an expression producing a source string
pub fn stdEvalString(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(1, args);
    const arg = args[0];
    const input = expr: {
        if (arg.val == ExprType.lst) {
            if (arg.val == ExprType.lst and (arg.val.lst.items[0] == &expr_atom_quote or arg.val.lst.items[0] == &expr_atom_quasi_quote)) {
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
        return try ev.eval(env, try ev.parse(input.val.sym));
    } else {
        return input;
    }
}

/// (apply fn arg1 ... args) where the last argument must be a list, which is the common contract in Lisps
/// Behaves as if fn is called with arguments from the list produced by (append (list arg1 ...) args)
pub fn stdApply(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);

    const last_arg_list = try ev.eval(env, args[args.len - 1]);
    try requireType(ev, last_arg_list, ExprType.lst);
    var fncall = try makeListExpr(null);
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
pub fn stdList(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var list = try makeListExpr(null);
    for (args) |arg| {
        try list.val.lst.append(try ev.eval(env, arg));
    }
    return list;
}

/// Creates a new list and populates it with the arguments. If any arguments are
/// lists, their elements are spliced into the result list. nil arguments are ignored.
/// (append '(1 2) '(3 4)) -> (1 2 3 4)
pub fn stdAppend(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var list = try makeListExpr(null);
    for (args) |arg| {
        const evaled = try ev.eval(env, arg);
        if (evaled.val == ExprType.lst) {
            for (evaled.val.lst.items) |item| {
                try list.val.lst.append(item);
            }
        } else {
            const expr = try ev.eval(env, arg);
            if (expr != &expr_atom_nil) {
                try list.val.lst.append(expr);
            }
        }
    }
    return list;
}

/// Adds a new binding to the current environment
pub fn stdDefine(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    try requireType(ev, args[0], ExprType.sym);

    if (env.lookup(args[0].val.sym, false) != null) {
        try ev.printErrorFmt(&args[0].src, "{s} is already defined\n", .{args[0].val.sym});
        return ExprErrors.AlreadyReported;
    }
    var value = try ev.eval(env, args[1]);
    try env.putWithSymbol(args[0], value);
    return value;
}

/// Replace binding
pub fn stdSet(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    try requireType(ev, args[0], ExprType.sym);
    return env.replace(args[0], try ev.eval(env, args[1]));
}

/// Remove binding if it exists
pub fn stdUnset(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    try requireType(ev, args[0], ExprType.sym);
    _ = env.replace(args[0], null);
    return &expr_atom_nil;
}
