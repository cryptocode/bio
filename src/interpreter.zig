const std = @import("std");
const ast = @import("ast.zig");
const intrinsics = @import("intrinsics.zig");
const gc = @import("boehm.zig");
const util = @import("util.zig");
const linereader = @import("linereader.zig");
const Token = ast.Token;
const Expr = ast.Expr;
const ExprType = ast.ExprType;
const ExprValue = ast.ExprValue;
const Env = ast.Env;
const ExprErrors = ast.ExprErrors;

const isFalsy = util.isFalsy;
const requireExactArgCount = util.requireExactArgCount;
const requireMinimumArgCount = util.requireMinimumArgCount;
const requireType = util.requireType;

/// The expression reader and evaluator
pub const Interpreter = struct {
    env: *Env,
    exit_code: ?u8 = null,
    gensym_seq: u64 = 0,
    verbose: bool = false,
    /// Reset before every evaluation, and set if an error occurs during the evaluation.
    has_errors: bool = false,
    break_seen: bool = false,
    allocator: std.mem.Allocator,

    /// Set up the root environment by binding a core set of intrinsics.
    /// The rest of the standard Bio functions are loaded from std.lisp
    pub fn init() !*Interpreter {
        var allocator = gc.allocator();
        var instance = try allocator.create(Interpreter);
        instance.* = Interpreter{
            .env = try ast.makeEnv(null, "global"),
            .allocator = gc.allocator(),
        };

        // Make all built-in functions, symbols and numeric constants available in the root environment
        try instance.env.populateWithIntrinsics();

        return instance;
    }

    pub fn deinit(_: *Interpreter) void {}

    /// Print user friendly errors
    pub fn printError(_: *Interpreter, err: anyerror) !void {
        if (err == ExprErrors.AlreadyReported) return;
        try std.io.getStdErr().writer().print("{s}\n", .{ast.errString(err)});
    }

    /// Print a formatted runtime error message, including source information.
    pub fn printErrorFmt(self: *Interpreter, expr: *Expr, comptime fmt: []const u8, args: anytype) !void {
        var stdout = std.io.getStdErr().writer();
        if (expr.tok) |tok| {
            const file = if (tok.file.path) |path| path else "eval";
            const info = ast.SourceInfo.compute(tok);
            try stdout.print("\x1b[1m{s}:{d}:{d}: error: ", .{ file, info.line + 1, info.column + 1 });
            try stdout.print(fmt, args);

            // var line = try gc.allocator().dupe(u8, info.source_line);
            const content = tok.file.content;
            if (info.line_start <= tok.start and tok.end <= info.line_end) {
                const indentation = "    ";
                try stdout.print("\n", .{});
                try stdout.print("\x1b[0m\n{s}", .{indentation});
                try stdout.print("{s}", .{content[info.line_start..tok.start]});
                try stdout.print("\x1b[92;4m{s}\x1b[0m", .{content[tok.start..tok.end]});
                try stdout.print("{s}\n", .{content[tok.end..info.line_end]});
                // Print ^----- indicator
                const caret_offset = tok.start - info.line_start;
                const filler = try gc.allocator().alloc(u8, caret_offset);
                @memset(filler, ' ');
                try stdout.print("\x1b[92m{s}{s}✗~~~\x1b[0m", .{ indentation, filler });
            }
        } else try stdout.print(fmt, args);
        try stdout.print("\n", .{});
        self.has_errors = true;
    }

    /// Evaluate an expression
    pub fn eval(self: *Interpreter, environment: *Env, expr: *Expr) anyerror!*Expr {
        var maybe_next: ?*Expr = expr;
        var env: *Env = environment;
        var func_lookup_env: ?*Env = null;
        var seen_macro_expand = false;
        self.has_errors = false;

        tailcall_optimization_loop: while (maybe_next) |e| {
            if (self.exit_code) |_| {
                return ast.getIntrinsic(.nil);
            }
            if (e.isIntrinsic(.@"&break")) {
                self.break_seen = true;
                return ast.getIntrinsic(.nil);
            }
            switch (e.val) {
                ExprValue.num, ExprValue.env, ExprValue.any => {
                    return e;
                },
                ExprValue.sym => |sym| {
                    if (env.lookup(sym, true)) |val| {
                        return val;
                    } else {
                        try self.printErrorFmt(e, "{s} is not defined", .{sym});
                        return ast.getIntrinsic(.nil);
                    }
                },
                ExprValue.lst => |list| {
                    if (list.items.len == 0) {
                        return ast.getIntrinsic(.nil);
                    }

                    const args_slice = list.items[1..];
                    if (list.items[0].isIntrinsic(.macroexpand)) {
                        // Signal that we don't want to evaluate the expression returned by the macro
                        seen_macro_expand = true;
                        maybe_next = args_slice[args_slice.len - 1];
                        continue;
                    } else if (list.items[0].isIntrinsic(.begin)) {
                        var res: *Expr = ast.getIntrinsic(.nil);
                        for (args_slice[0 .. args_slice.len - 1]) |arg| {
                            res = try self.eval(env, arg);
                        }
                        maybe_next = args_slice[args_slice.len - 1];
                        continue;
                    } else if (list.items[0].isIntrinsic(.cond)) {
                        try requireMinimumArgCount(2, args_slice);

                        const else_branch = args_slice[args_slice.len - 1];
                        if (else_branch.val != ExprType.lst or else_branch.val.lst.items.len != 1) {
                            try self.printErrorFmt(e, "Last expression in cond must be a single-expression list", .{});
                            return ExprErrors.AlreadyReported;
                        }

                        for (args_slice) |branch| {
                            if (branch == else_branch) {
                                maybe_next = branch.val.lst.items[0];
                                continue :tailcall_optimization_loop;
                            } else if (branch.val == ExprType.lst) {
                                try requireExactArgCount(2, branch.val.lst.items);

                                const predicate = try self.eval(env, branch.val.lst.items[0]);
                                if (predicate.isIntrinsic(.@"#t")) {
                                    maybe_next = branch.val.lst.items[1];
                                    continue :tailcall_optimization_loop;
                                }
                            } else {
                                try self.printErrorFmt(e, "Invalid switch syntax", .{});
                                return ExprErrors.AlreadyReported;
                            }
                        }
                        return ast.getIntrinsic(.nil);
                    } else if (list.items[0].isIntrinsic(.@"if")) {
                        try requireMinimumArgCount(2, args_slice);

                        var branch: usize = 1;
                        const predicate = try self.eval(env, args_slice[0]);
                        if (!predicate.isIntrinsic(.@"#t")) {
                            // Anything not defined as falsy is considered true in a boolean context
                            if (isFalsy(predicate)) {
                                branch += 1;
                            }
                        }

                        if (branch < args_slice.len) {
                            maybe_next = args_slice[branch];
                            continue :tailcall_optimization_loop;
                        } else {
                            return ast.getIntrinsic(.nil);
                        }
                    }

                    // Look up intrinsic, lambda, macro or env by name. If not found, the lookup has already reported the error.
                    var func = try self.eval(if (func_lookup_env) |fle| fle else env, list.items[0]);
                    func_lookup_env = null;
                    if (func.isIntrinsic(.nil)) {
                        return ast.getIntrinsic(.nil);
                    }

                    const kind = func.val;
                    switch (kind) {
                        // Look up a symbol or call a function in the given environment
                        ExprValue.env => |target_env| {
                            if (args_slice.len == 0 or (args_slice[0].val != ExprType.sym and args_slice[0].val != ExprType.lst)) {
                                const arg_str = if (args_slice.len > 0)
                                    try args_slice[0].toStringAlloc()
                                else
                                    "[no argument]";

                                try self.printErrorFmt(e, "Missing symbol or call in environment lookup: {s}", .{arg_str});
                                return ExprErrors.AlreadyReported;
                            }

                            if (args_slice[0].val == ExprType.lst) {
                                maybe_next = args_slice[0];
                                func_lookup_env = target_env;
                                continue :tailcall_optimization_loop;
                            } else if (target_env.lookup(args_slice[0].val.sym, false)) |match| {
                                return match;
                            } else {
                                try self.printErrorFmt(e, "Symbol not found in given environment: {s}", .{args_slice[0].val.sym});
                                return ExprErrors.AlreadyReported;
                            }
                        },

                        // Evaluate an intrinsic function
                        ExprValue.fun => |fun| {
                            return fun(self, env, args_slice) catch |err| {
                                try self.printErrorFmt(e, "{s}", .{ast.errString(err)});
                                return ast.getIntrinsic(.nil);
                            };
                        },

                        // Evaluate a previously defined lambda or macro
                        ExprValue.lam, ExprValue.mac => |fun| {
                            try requireType(self, fun.items[0], ExprType.lst);
                            const parent_env = if (kind == ExprType.lam) func.env else env;
                            const kind_str = if (kind == ExprType.lam) "lambda" else "macro";

                            var local_env = try ast.makeEnv(parent_env, kind_str);
                            const formals = fun.items[0].val.lst.items;
                            var formal_param_count = formals.len;
                            var logical_arg_count = args_slice.len;
                            var eval_formal_count: usize = 0;

                            // Bind arguments to the new environment
                            for (formals, 0..) |param, index| {
                                if (param.val == ExprType.sym) {
                                    if (param.isIntrinsic(.@"&rest")) {
                                        formal_param_count -= 1;
                                        var rest_args = try ast.makeListExpr(null);
                                        for (args_slice[index..]) |rest_param| {
                                            logical_arg_count -= 1;
                                            if (kind == ExprType.lam) {
                                                try rest_args.val.lst.append(try self.eval(env, rest_param));
                                            } else {
                                                try rest_args.val.lst.append(rest_param);
                                            }
                                        }

                                        logical_arg_count += 1;
                                        try local_env.putWithSymbol(formals[index + 1], rest_args);
                                        break;
                                    }

                                    if (param.isIntrinsic(.@"&eval")) {
                                        eval_formal_count += 1;
                                        formal_param_count -= 1;
                                        continue;
                                    }
                                    // Arguments are eagerly evaluated for lambdas, lazily for macros (that is, the expression is passed unevaluated)
                                    // If a macro formal parameter is preceded by "&eval", then that argument is eagerly evaluated anyway.
                                    if (index - eval_formal_count < args_slice.len) {
                                        if (kind == ExprType.lam or eval_formal_count > 0) {
                                            try local_env.putWithSymbol(param, try self.eval(env, args_slice[index - eval_formal_count]));
                                            eval_formal_count = 0;
                                        } else {
                                            try local_env.putWithSymbol(param, args_slice[index]);
                                        }
                                    }
                                } else {
                                    try self.printErrorFmt(e, "Formal parameter to {s} is not a symbol: {s}", .{ kind_str, try param.toStringAlloc() });
                                }
                            }

                            if (logical_arg_count != formal_param_count) {
                                try self.printErrorFmt(e, "{s} received {d} arguments, expected {d}", .{ kind_str, args_slice.len, formal_param_count });
                                return ast.getIntrinsic(.nil);
                            }

                            // Evaluate body, except the last expression which is TCO'ed
                            var result: *Expr = undefined;
                            for (fun.items[1 .. fun.items.len - 1]) |body_expr| {
                                result = self.eval(local_env, body_expr) catch |err| {
                                    try self.printErrorFmt(body_expr, "{s}: Could not evaluate {s} body", .{ ast.errString(err), kind_str });
                                    return ast.getIntrinsic(.nil);
                                };
                                if (self.has_errors) {
                                    return ast.getIntrinsic(.nil);
                                }
                            }

                            // For lambdas, we set up the next iteration to eval the last expression, while for
                            // macros we just evaluate it on the assumption it's a quoted expression, which is
                            // then evaluated in the next TCO iteration.
                            const last_expr = fun.items[fun.items.len - 1];
                            if (kind == ExprValue.lam) {
                                env = local_env;
                                maybe_next = last_expr;
                                continue;
                            } else {
                                result = self.eval(local_env, last_expr) catch |err| {
                                    try self.printErrorFmt(last_expr, "{s}: Could not evaluate {s} body", .{ ast.errString(err), kind_str });
                                    return ast.getIntrinsic(.nil);
                                };

                                if (seen_macro_expand) {
                                    return result;
                                }

                                env = parent_env.?;
                                maybe_next = result;
                                continue;
                            }
                        },
                        else => {
                            try self.printErrorFmt(e, "Not a function or macro: {s}", .{try list.items[0].toStringAlloc()});
                            return ast.getIntrinsic(.nil);
                        },
                    }
                    return ast.getIntrinsic(.nil);
                },
                // This allows us to create intrinsics that pass intrinsics as arguments
                ExprValue.fun => {
                    return e;
                },
                else => {
                    try self.printErrorFmt(e, "Invalid expression", .{});
                    return ast.getIntrinsic(.nil);
                },
            }
        }
        return ast.getIntrinsic(.nil);
    }

    // REPL
    pub fn readEvalPrint(self: *Interpreter) !void {
        const logo =
            \\
            \\       ..          ..
            \\     λλ   '(λλ.     `λλ
            \\    λλ        λ       λλ             Bio is a Lisp written in Zig
            \\   λλ'        λλ      `λλ         Docs at github.com/cryptocode/bio
            \\   λλ        BIO!      λλ
            \\   λλ       λλ'`λ      λλ           Use arrow up/down for history
            \\   λλ.     ,λ'  λλ    ,λλ          You can also run bio files with
            \\    λλ    λλ'    λλ) .λλ                   "bio run <file>"
            \\     λλ .           .λλ
            \\        ``         ''
        ;

        try std.io.getStdOut().writer().print("{s}\n\n", .{logo});
        var buf = std.ArrayList(u8).init(gc.allocator());

        main_loop: while (true) {
            buf.clearRetainingCapacity();
            var file = ast.File{ .content = "" };
            var parser = ast.Parser.init(&file);
            try linereader.linenoise_wrapper.printPrompt("bio: ");

            while (true) {
                // We're not actually seeing \n, instead linenoise hands us an EOF error, which we ignore
                linereader.linenoise_reader.streamUntilDelimiter(buf.writer(), '\n', null) catch {};

                parser.updateSource(buf.items);
                parser.parse() catch |err| {
                    try self.printError(err);
                    continue :main_loop;
                };

                if (parser.missingRightParen()) {
                    try linereader.linenoise_wrapper.printPrompt("     ");
                } else break;
            }

            _ = try linereader.linenoise_wrapper.addToHistory(buf.items);

            for (parser.cur.val.lst.items) |expr| {
                var res = self.eval(self.env, expr) catch |err| {
                    try self.printError(err);
                    break;
                };
                // Update the last expression and print it
                try self.env.put("#?", res);
                if (!res.isNil() or self.verbose) {
                    try res.print();
                }
                try std.io.getStdOut().writer().print("\n", .{});

                if (self.exit_code) |exit_code| {
                    self.deinit();
                    std.process.exit(exit_code);
                }
            }
        }
        try std.io.getStdOut().writer().print("Got {s}\n", .{buf.items});
    }
};
