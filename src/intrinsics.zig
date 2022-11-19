const std = @import("std");
const ast = @import("ast.zig");
const interpreter = @import("interpreter.zig");
const mem = @import("gc.zig");
const SourceLocation = @import("sourcelocation.zig").SourceLocation;
const Env = ast.Env;
const Expr = ast.Expr;
const ExprValue = ast.ExprValue;
const ExprType = ast.ExprType;
const ExprErrors = ast.ExprErrors;
const Interpreter = interpreter.Interpreter;

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
pub var expr_atom_mut = Expr{ .val = ExprValue{ .sym = "&mut" } };
pub var expr_atom_break = Expr{ .val = ExprValue{ .sym = "&break" } };
pub var expr_atom_macroexpand = Expr{ .val = ExprValue{ .sym = "macroexpand" } };
pub var expr_std_math_pi = Expr{ .val = ExprValue{ .num = std.math.pi } };
pub var expr_std_math_e = Expr{ .val = ExprValue{ .num = std.math.e } };
pub var expr_std_import = Expr{ .val = ExprValue{ .fun = stdImport } };
pub var expr_std_exit = Expr{ .val = ExprValue{ .fun = stdExit } };
pub var expr_std_verbose = Expr{ .val = ExprValue{ .fun = stdVerbose } };
pub var expr_std_assert_true = Expr{ .val = ExprValue{ .fun = stdAssertTrue } };
pub var expr_std_is_number = Expr{ .val = ExprValue{ .fun = stdIsNumber } };
pub var expr_std_is_symbol = Expr{ .val = ExprValue{ .fun = stdIsSymbol } };
pub var expr_std_is_list = Expr{ .val = ExprValue{ .fun = stdIsList } };
pub var expr_std_is_hashmap = Expr{ .val = ExprValue{ .fun = stdIsHashmap } };
pub var expr_std_is_err = Expr{ .val = ExprValue{ .fun = stdIsError } };
pub var expr_std_is_callable = Expr{ .val = ExprValue{ .fun = stdIsCallable } };
pub var expr_std_is_lowercase = Expr{ .val = ExprValue{ .fun = stdIsLowercase } };
pub var expr_std_is_uppercase = Expr{ .val = ExprValue{ .fun = stdIsUppercase } };
pub var expr_std_lowercase = Expr{ .val = ExprValue{ .fun = stdLowercase } };
pub var expr_std_uppercase = Expr{ .val = ExprValue{ .fun = stdUppercase } };
pub var expr_std_gensym = Expr{ .val = ExprValue{ .fun = stdGenSym } };
pub var expr_std_quote = Expr{ .val = ExprValue{ .fun = stdQuote } };
pub var expr_std_unquote = Expr{ .val = ExprValue{ .fun = stdUnquote } };
pub var expr_std_unquote_splicing = Expr{ .val = ExprValue{ .fun = stdUnquoteSplicing } };
pub var expr_std_quasi_quote = Expr{ .val = ExprValue{ .fun = stdQuasiQuote } };
pub var expr_std_double_quote = Expr{ .val = ExprValue{ .fun = stdDoubleQuote } };
pub var expr_std_len = Expr{ .val = ExprValue{ .fun = stdLen } };
pub var expr_std_contains = Expr{ .val = ExprValue{ .fun = stdContains } };
pub var expr_std_clone = Expr{ .val = ExprValue{ .fun = stdClone } };
pub var expr_std_range = Expr{ .val = ExprValue{ .fun = stdRange } };
pub var expr_std_rotate_left = Expr{ .val = ExprValue{ .fun = stdRotateLeft } };
pub var expr_std_item_at = Expr{ .val = ExprValue{ .fun = stdItemAt } };
pub var expr_std_item_set = Expr{ .val = ExprValue{ .fun = stdItemSet } };
pub var expr_std_item_remove = Expr{ .val = ExprValue{ .fun = stdItemRemove } };
pub var expr_std_string = Expr{ .val = ExprValue{ .fun = stdString } };
pub var expr_std_print = Expr{ .val = ExprValue{ .fun = stdPrint } };
pub var expr_std_env = Expr{ .val = ExprValue{ .fun = stdEnv } };
pub var expr_std_self = Expr{ .val = ExprValue{ .fun = stdSelf } };
pub var expr_std_parent = Expr{ .val = ExprValue{ .fun = stdParent } };
pub var expr_std_define = Expr{ .val = ExprValue{ .fun = stdDefine } };
pub var expr_std_vars = Expr{ .val = ExprValue{ .fun = stdVars } };
pub var expr_std_lambda = Expr{ .val = ExprValue{ .fun = stdLambda } };
pub var expr_std_macro = Expr{ .val = ExprValue{ .fun = stdMacro } };
pub var expr_std_eval_string = Expr{ .val = ExprValue{ .fun = stdEvalString } };
pub var expr_std_eval = Expr{ .val = ExprValue{ .fun = stdEval } };
pub var expr_std_apply = Expr{ .val = ExprValue{ .fun = stdApply } };
pub var expr_std_list = Expr{ .val = ExprValue{ .fun = stdList } };
pub var expr_std_map_new = Expr{ .val = ExprValue{ .fun = stdHashmapNew } };
pub var expr_std_map_put = Expr{ .val = ExprValue{ .fun = stdHashmapPut } };
pub var expr_std_map_get = Expr{ .val = ExprValue{ .fun = stdHashmapGet } };
pub var expr_std_map_remove = Expr{ .val = ExprValue{ .fun = stdHashmapRemove } };
pub var expr_std_map_clear = Expr{ .val = ExprValue{ .fun = stdHashmapClear } };
pub var expr_std_map_keys = Expr{ .val = ExprValue{ .fun = stdHashmapKeys } };
pub var expr_std_loop = Expr{ .val = ExprValue{ .fun = stdLoop } };
pub var expr_std_split = Expr{ .val = ExprValue{ .fun = stdSplit } };
pub var expr_std_append = Expr{ .val = ExprValue{ .fun = stdAppend } };
pub var expr_std_unset = Expr{ .val = ExprValue{ .fun = stdUnset } };
pub var expr_std_error = Expr{ .val = ExprValue{ .fun = stdError } };
pub var expr_std_try = Expr{ .val = ExprValue{ .fun = stdTry } };
pub var expr_std_set = Expr{ .val = ExprValue{ .fun = stdSet } };
pub var expr_std_logical_and = Expr{ .val = ExprValue{ .fun = stdLogicalAnd } };
pub var expr_std_logical_or = Expr{ .val = ExprValue{ .fun = stdLogicalOr } };
pub var expr_std_logical_not = Expr{ .val = ExprValue{ .fun = stdLogicalNot } };
pub var expr_std_sum = Expr{ .val = ExprValue{ .fun = stdSum } };
pub var expr_std_sub = Expr{ .val = ExprValue{ .fun = stdSub } };
pub var expr_std_mul = Expr{ .val = ExprValue{ .fun = stdMul } };
pub var expr_std_div = Expr{ .val = ExprValue{ .fun = stdDiv } };
pub var expr_std_pow = Expr{ .val = ExprValue{ .fun = stdPow } };
pub var expr_std_time_now = Expr{ .val = ExprValue{ .fun = stdTimeNow } };
pub var expr_std_floor = Expr{ .val = ExprValue{ .fun = stdFloor } };
pub var expr_std_round = Expr{ .val = ExprValue{ .fun = stdRound } };
pub var expr_std_min = Expr{ .val = ExprValue{ .fun = stdMin } };
pub var expr_std_max = Expr{ .val = ExprValue{ .fun = stdMax } };
pub var expr_std_as = Expr{ .val = ExprValue{ .fun = stdAs } };
pub var expr_std_split_atom = Expr{ .val = ExprValue{ .fun = stdSplitAtom } };
pub var expr_std_swap = Expr{ .val = ExprValue{ .fun = stdSwap } };
pub var expr_std_order = Expr{ .val = ExprValue{ .fun = stdOrder } };
pub var expr_std_eq = Expr{ .val = ExprValue{ .fun = stdEq } };
pub var expr_std_eq_approx = Expr{ .val = ExprValue{ .fun = stdEqApprox } };
pub var expr_std_eq_reference = Expr{ .val = ExprValue{ .fun = stdEqReference } };
pub var expr_std_run_gc = Expr{ .val = ExprValue{ .fun = stdRunGc } };
pub var expr_std_file_open = Expr{ .val = ExprValue{ .fun = stdFileOpen } };
pub var expr_std_file_close = Expr{ .val = ExprValue{ .fun = stdFileClose } };
pub var expr_std_file_read_line = Expr{ .val = ExprValue{ .fun = stdFileReadLine } };
pub var expr_std_file_write_line = Expr{ .val = ExprValue{ .fun = stdFileWriteLine } };
pub var expr_std_file_read_byte = Expr{ .val = ExprValue{ .fun = stdFileReadByte } };

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
pub fn stdExit(ev: *Interpreter, _: *Env, args: []const *Expr) anyerror!*Expr {
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
        var path = try std.fs.path.resolve(mem.allocator, &.{filename_expr.val.sym});
        defer mem.allocator.free(path);

        var file = try mem.allocator.create(std.fs.File);
        errdefer mem.allocator.destroy(file);
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
    mem.allocator.destroy(file);
    return &expr_atom_nil;
}

/// Reads a byte from the given file
pub fn stdFileReadByte(ev: *Interpreter, env: *Env, args: []const *Expr) !*Expr {
    try requireExactArgCount(1, args);
    const file_ptr = try ev.eval(env, args[0]);
    try requireType(ev, file_ptr, ExprType.any);
    const file = @intToPtr(*std.fs.File, file_ptr.val.any);
    if (file.reader().readByte()) |byte| {
        return ast.makeAtomByDuplicating(&.{byte});
    } else |e| switch (e) {
        error.EndOfStream => return try ast.makeError(try ast.makeAtomByDuplicating("EOF")),
        else => return try ast.makeError(try ast.makeAtomByDuplicating("Could not read from file")),
    }
}

/// Reads a line from the given file, or from stdin if no argument is given
pub fn stdFileReadLine(ev: *Interpreter, env: *Env, args: []const *Expr) !*Expr {
    var reader: std.fs.File.Reader = std.io.getStdIn().reader();
    if (args.len > 0) {
        try requireExactArgCount(1, args);
        const file_ptr = try ev.eval(env, args[0]);
        try requireType(ev, file_ptr, ExprType.any);
        const file = @intToPtr(*std.fs.File, file_ptr.val.any);

        reader = file.reader();
    }

    if (reader.readUntilDelimiterOrEofAlloc(mem.allocator, '\n', std.math.maxInt(usize))) |maybe| {
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
pub fn stdFileWriteLine(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(1, args);
    const line_to_write = try ev.eval(env, args[args.len - 1]);
    try requireType(ev, line_to_write, ExprType.sym);

    var writer: std.fs.File.Writer = std.io.getStdOut().writer();
    if (args.len > 1) {
        try requireExactArgCount(2, args);
        const file_ptr = try ev.eval(env, args[0]);
        try requireType(ev, file_ptr, ExprType.any);
        const file = @intToPtr(*std.fs.File, file_ptr.val.any);
        try file.seekFromEnd(0);

        writer = file.writer();
    }

    try writer.writeAll(line_to_write.val.sym);
    _ = try writer.write("\n");
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
                    defer mem.allocator.free(input);
                    if (try ev.parseAndEvalExpression(input)) |e| {
                        res = e;
                        try ev.env.put("#?", res);
                    }
                } else {
                    break;
                }
            } else |err| {
                try ev.printErrorFmt(SourceLocation.current(), "", .{});
                try ev.printError(err);
                break;
            }
        }
        return res;
    } else {
        return ExprErrors.InvalidArgumentType;
    }
}

pub fn stdRunGc(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, env, args };
    try mem.gc.run(true);
    return &expr_atom_nil;
}

pub fn stdVerbose(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ env, args };
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
    _ = &.{ ev, env };
    const str = try expr.toStringAlloc();
    defer mem.allocator.free(str);
    return try std.mem.replaceOwned(u8, mem.allocator, str, "\\n", "\n");
}

/// Implements (string expr...), i.e. rendering of expressions as strings
pub fn stdString(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var builder = std.ArrayList(u8).init(mem.allocator);
    defer builder.deinit();
    const writer = builder.writer();

    for (args) |expr| {
        const value = try ev.eval(env, expr);
        const rendered = try render(ev, env, value);
        defer mem.allocator.free(rendered);
        try writer.writeAll(rendered);
    }
    return ast.makeAtomByDuplicating(builder.items);
}

/// Implements (print expr...)
pub fn stdPrint(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    for (args) |expr, index| {
        const value = try ev.eval(env, expr);
        const rendered = try render(ev, env, value);
        defer mem.allocator.free(rendered);
        try std.io.getStdOut().writer().print("{s}", .{rendered});
        if (index + 1 < args.len) {
            try std.io.getStdOut().writer().print(" ", .{});
        }
    }
    return &expr_atom_nil;
}

/// Returns the current environment as an expression, allowing the user to make constructs
/// such as modules and object instances.
pub fn stdSelf(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, args };
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .env = env };
    return expr;
}

/// Returns the parent environment, or nil if the environment is the root
pub fn stdParent(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, args };
    var expr = try Expr.create(true);
    if (env.parent) |p| {
        expr.val = ExprValue{ .env = p };
        return expr;
    }
    return &expr_atom_nil;
}

/// Print environments. Runs the GC to minimize the environment listing, unless no-gc is passed.
pub fn stdEnv(ev: *Interpreter, _: *Env, args: []const *Expr) anyerror!*Expr {
    if (!(args.len > 0 and args[0].val == ExprType.sym and std.mem.eql(u8, args[0].val.sym, "no-gc"))) {
        try mem.gc.run(false);
    }

    // Print out all environments, including which environment is the parent.
    for (mem.gc.registered_envs.items) |registered_env| {
        try std.io.getStdOut().writer().print("Environment for {s}: {*}\n", .{ registered_env.name, registered_env });

        var iter = registered_env.map.iterator();
        while (iter.next()) |item| {
            try std.io.getStdOut().writer().writeByteNTimes(' ', 4);
            try std.io.getStdOut().writer().print("{s} = ", .{item.key_ptr.*.val.sym});
            try item.value_ptr.*.print();
            if (ev.verbose) {
                try std.io.getStdOut().writer().print(", env {*}", .{item.value_ptr.*.env});
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
    return try ast.makeError(try ev.eval(env, args[0]));
}

pub fn isEmptyList(expr: *Expr) bool {
    return (expr.val == ExprType.lst and expr.val.lst.items.len == 0);
}

pub fn isError(expr: *Expr) bool {
    return expr.val == ExprType.err;
}

pub fn isFalsy(expr: *Expr) bool {
    return expr == &expr_atom_false or expr == &expr_atom_nil or isEmptyList(expr) or isError(expr);
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
    } else if (op1 == &expr_atom_false and isFalsy(op2)) { // and (op2 == &expr_atom_false or op2 == &expr_atom_nil)) {
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
        std.math.Order.lt => return ast.makeNumExpr(-1),
        std.math.Order.eq => return ast.makeNumExpr(0),
        std.math.Order.gt => return ast.makeNumExpr(1),
    };
}

/// Turn a boolean into #f or #t
fn boolExpr(val: bool) *Expr {
    return if (val) &expr_atom_true else &expr_atom_false;
}

/// Check for equality. If the order operation fails, such as incompatiable types, false is returned.
pub fn stdEq(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    return boolExpr((order(ev, env, &.{ try ev.eval(env, args[0]), try ev.eval(env, args[1]) }) catch return &expr_atom_false) == std.math.Order.eq);
}

/// Swap the values of two variables in the current or a parent environment
pub fn stdSwap(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    try requireType(ev, args[0], ExprType.sym);
    try requireType(ev, args[1], ExprType.sym);
    const v0 = env.lookup(args[0].val.sym, true) orelse return ExprErrors.InvalidArgumentType;
    const v1 = env.lookup(args[1].val.sym, true) orelse return ExprErrors.InvalidArgumentType;

    try env.put(args[0].val.sym, v1);
    try env.put(args[1].val.sym, v0);
    return &expr_atom_nil;
}

/// Returns #t if the two arguments evaluate to the exact same object
/// This is mostly useful for debugging Bio itself
/// (^= nil nil) -> #t
pub fn stdEqReference(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    return boolExpr((try ev.eval(env, args[0])) == (try ev.eval(env, args[1])));
}

/// Compare floats with a small relative epsilon comparison. An optional third argument overrides the tolerance.
/// If the input are symbols, case-insensitive comparison is performed.
pub fn stdEqApprox(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
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
    return boolExpr(arg.val == ExprType.lst);
}

pub fn stdIsHashmap(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return boolExpr(arg.val == ExprType.map);
}

pub fn stdIsError(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return boolExpr(arg.val == ExprType.err);
}

pub fn stdIsCallable(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    const arg = try ev.eval(env, args[0]);
    return switch (arg.val) {
        ExprType.fun, ExprType.lam, ExprType.mac => &expr_atom_true,
        else => &expr_atom_false,
    };
}

/// (gensym) will generate a unique identifier
pub fn stdGenSym(ev: *Interpreter, _: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(0, args);
    ev.gensym_seq += 1;
    const sym = try std.fmt.allocPrint(mem.allocator, "gensym_{d}", .{ev.gensym_seq});
    return ast.makeAtomAndTakeOwnership(sym);
}

/// Renders the expression and wraps it in double quotes, returning a new atom
pub fn stdDoubleQuote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);

    const rendered = try render(ev, env, arg);
    defer mem.allocator.free(rendered);

    const double_quoted = try std.fmt.allocPrint(mem.allocator, "\"{s}\"", .{rendered});
    return ast.makeAtomAndTakeOwnership(double_quoted);
}

/// Returns the first argument unevaluated. Multiple arguments is an error,
/// though the argument may be a list. (quote (1 2 3)) -> '(1 2 3)
pub fn stdQuote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, env };
    try requireExactArgCount(1, args);

    // We must make a fresh copy for quoted lists. Consider (var lst '(1 2 3)) in a lambda. If not making a copy,
    // the list would be memoized between calls. This is not a problem for (list 1 2 3) since the list function
    // per definition creates a new list on every evaluation.
    if (args[0].val == ExprType.lst) {
        return try ast.makeListExpr(args[0].val.lst.items);
    }
    return args[0];
}

/// Unquote is only useful in combination with quasiquoting, see stdQuasiQuote
pub fn stdUnquote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ env, args };
    try ev.printErrorFmt(SourceLocation.current(), "Can only use unquote inside a quasiquote expression\n", .{});
    return ExprErrors.AlreadyReported;
}

/// Unquote with splicing is only useful in combination with quasiquoting, see stdQuasiQuote
pub fn stdUnquoteSplicing(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ env, args };
    try ev.printErrorFmt(SourceLocation.current(), "Can only use unquote-splicing inside a quasiquote expression\n", .{});
    return ExprErrors.AlreadyReported;
}

/// Recursive quasi-quote expansion
///   (quasiquote (1 2 (unquote (+ 1 2)) 4))           -> '(1 2 3 4)
///   (quasiquote (1 2 (+ 1 2) 4))                     -> '(1 2 (+ 1 2) 4)
///   (quasiquote (1 (unquote-splicing (list 2 3)) 4)) -> '(1 2 3 4)
///   `(1 ,@(list 2 3) 4)                              -> '(1 2 3 4)
pub fn stdQuasiQuote(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);

    const qq_expander = struct {
        fn expand(ev_inner: *Interpreter, env_inner: *Env, expr: *Expr) anyerror!*Expr {
            if (expr.val == ExprType.lst) {
                var result_list = try ast.makeListExpr(null);
                for (expr.val.lst.items) |item| {
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

/// Implements (item-at list index) and (item-at symbol index)
/// If index is out of bounds, nil is returned
pub fn stdItemAt(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const index_arg = try ev.eval(env, args[0]);
    const container = try ev.eval(env, args[1]);
    try requireType(ev, index_arg, ExprType.num);
    const index = @floatToInt(isize, index_arg.val.num);
    switch (container.val) {
        ExprType.lst => |*list| {
            return if (index >= 0 and index < list.items.len) list.items[@intCast(usize, index)] else &expr_atom_nil;
        },
        ExprType.sym => |sym| {
            if (index >= 0 and index < sym.len) {
                //return try ast.makeListExpr(&.{ &expr_atom_quote, try ast.makeAtomByDuplicating(&.{sym[@intCast(usize, index)]}) });
                return try ast.makeAtomByDuplicating(&.{sym[@intCast(usize, index)]});
            } else return &expr_atom_nil;
        },
        else => return &expr_atom_nil,
    }
}

/// Implements list mutation. If the index is out of bounds, the item is appended or prepended accordingly.
/// The previous value is returned, or nil if index was out of bounds.
/// (item-set 4 list newitem)
pub fn stdItemSet(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(3, args);
    const indexArg = try ev.eval(env, args[0]);
    const listArg = try ev.eval(env, args[1]);
    const newItem = try ev.eval(env, args[2]);
    try requireType(ev, indexArg, ExprType.num);
    try requireType(ev, listArg, ExprType.lst);

    // Index may be negative to prepend, so we use isize
    const index = @floatToInt(isize, indexArg.val.num);
    var list = &listArg.val.lst;
    if (index >= 0 and index < list.items.len) {
        var old = list.items[@intCast(usize, index)];
        list.items[@intCast(usize, index)] = newItem;
        return old;
    } else if (index >= list.items.len) {
        try list.append(newItem);
        return &expr_atom_nil;
    } else {
        try list.insert(0, newItem);
        return &expr_atom_nil;
    }
}

/// In-place removal of the n'th item. The removed item is returned, or nil if index is out of bounds.
pub fn stdItemRemove(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const indexArg = try ev.eval(env, args[0]);
    const listArg = try ev.eval(env, args[1]);
    try requireType(ev, indexArg, ExprType.num);
    try requireType(ev, listArg, ExprType.lst);

    // Index may be negative to prepend, so we use isize
    const index = @floatToInt(isize, indexArg.val.num);
    var list = &listArg.val.lst;
    if (index >= 0 and index < list.items.len) {
        return list.swapRemove(@intCast(usize, index));
    }
    return &expr_atom_nil;
}

/// In-place rotate a list left by the given amount
/// (rotate-left list amount)
pub fn stdRotateLeft(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const listArg = try ev.eval(env, args[0]);
    const amountArg = try ev.eval(env, args[1]);
    try requireType(ev, listArg, ExprType.lst);
    try requireType(ev, amountArg, ExprType.num);

    std.mem.rotate(*Expr, listArg.val.lst.items, @floatToInt(usize, amountArg.val.num));
    return listArg;
}

/// Implements (range list start? end?) where negative indices are end-relative
/// If both start and end are missing, return the first element as in (car list)
/// For range results, this produces a new list
pub fn stdRange(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(1, args);
    const listArg = try ev.eval(env, args[0]);
    try requireType(ev, listArg, ExprType.lst);
    const list = &listArg.val.lst;
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
        var res = try ast.makeListExpr(null);
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

/// Logical and with short-circuit
pub fn stdLogicalAnd(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    for (args) |expr| {
        const arg = try ev.eval(env, expr);
        if (arg == &expr_atom_true) continue;
        if (arg == &expr_atom_false) return &expr_atom_false;
        try ev.printErrorFmt(&expr.src, "Logical and requires boolean expressions\n", .{});
        return ExprErrors.AlreadyReported;
    }

    return &expr_atom_true;
}

/// Logical and with short-circuit
pub fn stdLogicalOr(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    for (args) |expr| {
        const arg = try ev.eval(env, expr);
        if (arg == &expr_atom_false) continue;
        if (arg == &expr_atom_true) return &expr_atom_true;
        try ev.printErrorFmt(&expr.src, "Logical and requires boolean expressions\n", .{});
        return ExprErrors.AlreadyReported;
    }

    return &expr_atom_false;
}

/// Logical not
pub fn stdLogicalNot(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);

    if (isFalsy(arg)) return &expr_atom_true;
    return &expr_atom_false;
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

    return ast.makeNumExpr(sum);
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

    return ast.makeNumExpr(res);
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

    return ast.makeNumExpr(sum);
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

    return ast.makeNumExpr(res);
}

pub fn stdPow(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const base = try ev.eval(env, args[0]);
    const exp = try ev.eval(env, args[1]);
    try requireType(ev, base, ExprType.num);
    try requireType(ev, exp, ExprType.num);
    return ast.makeNumExpr(std.math.pow(f64, base.val.num, exp.val.num));
}

pub fn stdTimeNow(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    _ = &.{ ev, env, args };
    return ast.makeNumExpr(@intToFloat(f64, std.time.milliTimestamp()));
}

/// Checks for presence by value comparison in hashmaps, lists and symbols; returns #t or #f accordingly
/// (contains? hashmap key)
/// (contains? list item)
/// (contains? somestring 'w)
pub fn stdContains(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const expr = try ev.eval(env, args[0]);
    const needle = try ev.eval(env, args[1]);
    if (expr == &expr_atom_nil) return try ast.makeNumExpr(@intToFloat(f64, 0));
    switch (expr.val) {
        ExprType.sym => return if (std.mem.indexOf(u8, expr.val.sym, needle.val.sym)) |_| &expr_atom_true else &expr_atom_false,
        ExprType.lst => {
            for (expr.val.lst.items) |item| {
                if ((try order(ev, env, &.{ needle, item })) == std.math.Order.eq) return &expr_atom_true;
            }
            return &expr_atom_false;
        },
        ExprType.map => {
            return if (expr.val.map.get(needle)) |_| &expr_atom_true else &expr_atom_false;
        },
        else => {
            try ev.printErrorFmt(&expr.src, "len function only works on lists, maps and symbols\n", .{});
            return &expr_atom_false;
        },
    }
}

/// Returns the length of a list or symbol, otherwise nil
/// If input is nil, 0 is returned
pub fn stdLen(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const expr = try ev.eval(env, args[0]);
    if (expr == &expr_atom_nil) return try ast.makeNumExpr(@intToFloat(f64, 0));
    switch (expr.val) {
        ExprType.sym => return try ast.makeNumExpr(@intToFloat(f64, expr.val.sym.len)),
        ExprType.lst => {
            return try ast.makeNumExpr(@intToFloat(f64, expr.val.lst.items.len));
        },
        ExprType.map => {
            return try ast.makeNumExpr(@intToFloat(f64, expr.val.map.count()));
        },
        else => {
            try ev.printErrorFmt(&expr.src, "len function only works on lists, maps and symbols\n", .{});
            return &expr_atom_nil;
        },
    }
}

/// Splits an atom's constituents into a list
/// (atom.split 123)  -> '(1 2 3)
/// (atom.split 'abc) -> '(a b c)
/// (atom.split "a string") -> (list "a" " " "s" "t" "r" "i" "n" "g")
pub fn stdSplitAtom(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const expr = try ev.eval(env, args[0]);
    switch (expr.val) {
        ExprType.sym => {
            const list = try ast.makeListExpr(null);
            for (expr.val.sym) |item| {
                try list.val.lst.append(try ast.makeAtomByDuplicating(&.{item}));
            }
            return list;
        },
        ExprType.num => {
            @panic("Splitting numbers not support yet");
        },
        else => return &expr_atom_nil,
    }
}

/// Convert between symbols, numbers and lists. Example: (as number (io.read-line))
/// Returns nil if the conversion fails
pub fn stdAs(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const target_type = args[0];
    const expr = try ev.eval(env, args[1]);
    try requireType(ev, target_type, ExprType.sym);
    if (std.mem.eql(u8, target_type.val.sym, "number")) {
        switch (expr.val) {
            ExprType.num => return expr,
            ExprType.sym => {
                const float = std.fmt.parseFloat(f64, expr.val.sym) catch {
                    return &expr_atom_nil;
                };
                return try ast.makeNumExpr(float);
            },
            else => return &expr_atom_nil,
        }
    } else if (std.mem.eql(u8, target_type.val.sym, "symbol")) {
        switch (expr.val) {
            ExprType.sym => return expr,
            ExprType.num => |num| {
                const val = try std.fmt.allocPrint(mem.allocator, "{d}", .{num});
                return ast.makeAtomLiteral(val, true);
            },
            ExprType.lst => |lst| {
                var res = std.ArrayList(u8).init(mem.allocator);
                var exprWriter = res.writer();
                defer res.deinit();
                for (lst.items) |item| {
                    const str = try item.toStringAlloc();
                    defer mem.allocator.free(str);
                    try exprWriter.writeAll(str);
                }
                return ast.makeAtomByDuplicating(res.items);
            },
            else => return &expr_atom_nil,
        }
    } else if (std.mem.eql(u8, target_type.val.sym, "list")) {
        if (expr.val == ExprType.lst) {
            return expr;
        }
        const list = try ast.makeListExpr(null);
        try list.val.lst.append(expr);
        return list;
    } else {
        try ev.printErrorFmt(&expr.src, "Invalid target type in (as): {s}. Must be number, symbol or list\n", .{target_type.val.sym});
        return &expr_atom_nil;
    }
}

/// Split symbol into a list of symbols by splitting on one or more a separators
/// (split "a,b,c;d" ",;") => '(a b c d)
pub fn stdSplit(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const input = try ev.eval(env, args[0]);
    const needle = try ev.eval(env, args[1]);
    try requireType(ev, input, ExprType.sym);
    try requireType(ev, needle, ExprType.sym);

    const list = try ast.makeListExpr(null);
    var it = std.mem.tokenize(u8, input.val.sym, needle.val.sym);
    while (it.next()) |item| {
        if (item.len == 0) {
            try list.val.lst.append(&expr_atom_nil);
        } else {
            try list.val.lst.append(try ast.makeAtomByDuplicating(item));
        }
    }

    return list;
}

pub fn stdFloor(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);
    try requireType(ev, arg, ExprType.num);
    return ast.makeNumExpr(@floor(arg.val.num));
}

pub fn stdRound(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);
    try requireType(ev, arg, ExprType.num);
    return ast.makeNumExpr(@round(arg.val.num));
}

pub fn minMax(ev: *Interpreter, env: *Env, args: []const *Expr, use_order: std.math.Order) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const arg = try ev.eval(env, args[0]);
    try requireType(ev, arg, ExprType.lst);

    var winner: *Expr = arg.val.lst.items[0];
    for (arg.val.lst.items[1..]) |item| {
        if (use_order == try order(ev, env, &.{ try ev.eval(env, winner), try ev.eval(env, item) })) {
            winner = item;
        }
    }

    return winner;
}

/// Find the smallest value in a list
pub fn stdMin(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    return minMax(ev, env, args, std.math.Order.gt);
}

/// Find the largest value in a list
pub fn stdMax(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    return minMax(ev, env, args, std.math.Order.lt);
}

/// This is called when a lambda is defined, not when it's invoked
/// The first argument must be a list, namely the lambda arguments
pub fn stdLambda(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    try requireType(ev, args[0], ExprType.lst);

    var expr = try ast.makeLambdaExpr(env);
    try expr.val.lam.appendSlice(args);
    return expr;
}

/// This is called when a macro is defined
/// The first argument must be a list, namely the macro arguments
pub fn stdMacro(ev: *Interpreter, _: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    try requireType(ev, args[0], ExprType.lst);

    var expr = try ast.makeMacroExpr();
    try expr.val.mac.appendSlice(args);
    return expr;
}

/// Evaluate the arguments, returning the last one as the result. If quote and quasiquote
/// expressions are encountered, these are unquoted before evaluation.
pub fn stdEval(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var res: *Expr = &expr_atom_nil;
    for (args) |arg| {
        if (arg.val == ExprType.lst and (arg.val.lst.items[0] == &expr_atom_quote or arg.val.lst.items[0] == &expr_atom_quasi_quote or arg.val.lst.items[0] == &expr_atom_macroexpand)) {
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
pub fn stdList(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var list = try ast.makeListExpr(null);
    for (args) |arg| {
        try list.val.lst.append(try ev.eval(env, arg));
    }
    return list;
}

/// True if all bytes in an symbol, when considered ascii characters, are lowercase
pub fn stdIsLowercase(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    var sym = try ev.eval(env, args[0]);
    try requireType(ev, sym, ExprType.sym);

    for (sym.val.sym) |c| {
        if (std.ascii.isUpper(c)) return &expr_atom_false;
    }
    return &expr_atom_true;
}

/// True if all bytes in an symbol, when considered ascii characters, are uppercase
pub fn stdIsUppercase(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    var sym = try ev.eval(env, args[0]);
    try requireType(ev, sym, ExprType.sym);

    for (sym.val.sym) |c| {
        if (std.ascii.isLower(c)) return &expr_atom_false;
    }
    return &expr_atom_true;
}

/// Returns a copy of a symbol with each byte ascii-lowercased
pub fn stdLowercase(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    var sym = try ev.eval(env, args[0]);
    try requireType(ev, sym, ExprType.sym);
    const result = try std.ascii.allocLowerString(mem.allocator, sym.val.sym);
    return try ast.makeAtomAndTakeOwnership(result);
}

/// Returns a copy of a symbol with each byte ascii-uppercased
pub fn stdUppercase(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    var sym = try ev.eval(env, args[0]);
    try requireType(ev, sym, ExprType.sym);
    const result = try std.ascii.allocUpperString(mem.allocator, sym.val.sym);
    return try ast.makeAtomAndTakeOwnership(result);
}

/// Create a new hashmap: (hashmap.new (1 2) (a 3)))
pub fn stdHashmapNew(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    var hmap = try ast.makeHashmapExpr(null);
    for (args) |arg| {
        try requireType(ev, arg, ExprType.lst);
        try hmap.val.map.put(try ev.eval(env, arg.val.lst.items[0]), try ev.eval(env, arg.val.lst.items[1]));
    }
    return hmap;
}

/// Returns the keys of the hashmap as a list
pub fn stdHashmapKeys(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
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
pub fn stdHashmapPut(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(3, args);
    const m = try ev.eval(env, args[0]);
    try requireType(ev, m, ExprType.map);
    const k = try ev.eval(env, args[1]);
    const v = try ev.eval(env, args[2]);
    const previous = m.val.map.get(k);
    try m.val.map.put(k, v);
    return if (previous) |p| p else &expr_atom_nil;
}

/// (map.get mymap 1)
/// Returns the matching value, otherwise nil
pub fn stdHashmapGet(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const m = try ev.eval(env, args[0]);
    try requireType(ev, m, ExprType.map);
    const k = try ev.eval(env, args[1]);
    const v = m.val.map.get(k);
    return if (v) |val| val else &expr_atom_nil;
}

/// (map.remove mymap 1)
/// Returns #t if the entry existed and was removed, otherwise #f
pub fn stdHashmapRemove(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(2, args);
    const m = try ev.eval(env, args[0]);
    try requireType(ev, m, ExprType.map);
    const k = try ev.eval(env, args[1]);
    return boolExpr(m.val.map.swapRemove(k));
}

/// Removes all items and returns the number of items removed
pub fn stdHashmapClear(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const m = try ev.eval(env, args[0]);
    try requireType(ev, m, ExprType.map);
    const count = try ast.makeNumExpr(@intToFloat(f64, m.val.map.count()));
    m.val.map.clearAndFree();
    return count;
}

/// Clones the expression
/// NOTE: Currently, only hashmaps are supported
pub fn stdClone(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireExactArgCount(1, args);
    const m = try ev.eval(env, args[0]);
    try requireType(ev, m, ExprType.map);

    var hmap = try ast.makeHashmapExpr(null);
    hmap.val.map = try m.val.map.clone();
    return hmap;
}

/// Loop from n to m or until &break is encountered
/// (loop '(0 9) body goes here) -> loops 10 times
/// (loop '(9 0) body goes here) -> loops 10 times
/// (loop idx '(9 0) body goes here) -> loops 10 times, current iteration count goes into the idx variable
/// (loop '() body goes here (if cond &break)) -> loops until &break is encountered
pub fn stdLoop(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    // loop_arg1 is either a loop index variable name (a symbol we don't look up), or the criteria list
    // If the first, then use stdDefine, i.e same as (var idx 0)
    var loop_arg1 = try ev.eval(env, args[0]);
    var critera: ?*Expr = null;
    var index_variable: ?*Expr = null;
    var index_increment: f64 = 1;

    if (loop_arg1.val == ExprType.lst) {
        critera = try ev.eval(env, args[0]);
    } else if (loop_arg1.val == ExprType.sym) {
        var num_expr = try ast.makeNumExpr(0);
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
        var first = try ev.eval(env, critera.?.val.lst.items[0]);
        var second = try ev.eval(env, critera.?.val.lst.items[1]);
        try requireType(ev, first, ExprType.num);
        try requireType(ev, second, ExprType.num);
        start = std.math.min(first.val.num, second.val.num);
        end = std.math.max(first.val.num, second.val.num);

        // Countdown?
        if (first.val.num > second.val.num) {
            if (index_variable) |iv| {
                index_increment = -1;
                iv.val.num = end - 1;
            }
        }
    }

    var last: *Expr = &expr_atom_nil;
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
pub fn stdAppend(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);

    const Handler = struct {
        pub fn handle(ip: *Interpreter, environment: *Env, list: *Expr, arg: *Expr) !void {
            const evaled = try ip.eval(environment, arg);
            if (evaled.val == ExprType.lst) {
                for (evaled.val.lst.items) |item| {
                    try list.val.lst.append(item);
                }
            } else {
                const expr = try ip.eval(environment, arg);
                if (expr != &expr_atom_nil) {
                    try list.val.lst.append(expr);
                }
            }
        }
    };

    var target_list: ?*Expr = null;
    var start_index: usize = 0;
    if (args[0] == &expr_atom_mut) {
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
    if (env.lookup(args[0].val.sym, false) != null and !allow_redefinition) {
        try ev.printErrorFmt(&args[0].src, "{s} is already defined\n", .{args[0].val.sym});
        return ExprErrors.AlreadyReported;
    }
    if (args.len > 1) {
        var value = try ev.eval(env, args[1]);
        try env.putWithSymbol(args[0], value);
        return value;
    } else {
        try env.putWithSymbol(args[0], &expr_atom_nil);
        return &expr_atom_nil;
    }
}

/// Adds a new binding to the current environment
pub fn stdDefine(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(1, args);
    try requireType(ev, args[0], ExprType.sym);
    return try putEnv(ev, env, args, false);
}

/// Define new variables through destructuring a list. Evaluates to the list expression.
/// (vars a b c '(1 2 3))
pub fn stdVars(ev: *Interpreter, env: *Env, args: []const *Expr) anyerror!*Expr {
    try requireMinimumArgCount(2, args);
    var list = try ev.eval(env, args[args.len - 1]);
    try requireType(ev, list, ExprType.lst);

    if (list.val.lst.items.len != args.len - 1) {
        try ev.printErrorFmt(&args[0].src, "vars variable count {d} does not match list size {d}\n", .{ args.len - 1, list.val.lst.items.len });
        return ExprErrors.AlreadyReported;
    }

    for (list.val.lst.items) |item, i| {
        try env.putWithSymbol(args[i], item);
    }

    return list;
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
