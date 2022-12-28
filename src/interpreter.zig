const std = @import("std");
const ast = @import("ast.zig");
const intrinsics = @import("intrinsics.zig");
const mem = @import("gc.zig");
const linereader = @import("linereader.zig");
const SourceLocation = @import("sourcelocation.zig").SourceLocation;
const Expr = ast.Expr;
const ExprType = ast.ExprType;
const ExprValue = ast.ExprValue;
const Env = ast.Env;
const ExprErrors = ast.ExprErrors;

/// The expression reader and evaluator
pub const Interpreter = struct {
    env: *Env,
    exit_code: ?u8 = null,
    gensym_seq: u64 = 0,
    verbose: bool = false,
    has_errors: bool = false,
    break_seen: bool = false,

    /// Set up the root environment by binding a core set of intrinsics.
    /// The rest of the standard Bio functions are loaded from std.lisp
    pub fn init() !Interpreter {
        mem.gc = try mem.GC.init();
        SourceLocation.initStack();

        var instance = Interpreter{ .env = try ast.makeEnv(null, "global") };
        try instance.env.put("import", &intrinsics.expr_std_import);
        try instance.env.put("exit", &intrinsics.expr_std_exit);
        try instance.env.put("gc", &intrinsics.expr_std_run_gc);
        try instance.env.put("#f", &intrinsics.expr_atom_false);
        try instance.env.put("#t", &intrinsics.expr_atom_true);
        try instance.env.put("#?", &intrinsics.expr_atom_nil);
        try instance.env.put("#!", &intrinsics.expr_atom_nil);
        try instance.env.put("&break", &intrinsics.expr_atom_break);
        try instance.env.put("nil", &intrinsics.expr_atom_nil);
        try instance.env.put("math.pi", &intrinsics.expr_std_math_pi);
        try instance.env.put("math.e", &intrinsics.expr_std_math_e);
        try instance.env.put("math.floor", &intrinsics.expr_std_floor);
        try instance.env.put("math.round", &intrinsics.expr_std_round);
        try instance.env.put("math.min", &intrinsics.expr_std_min);
        try instance.env.put("math.max", &intrinsics.expr_std_max);
        try instance.env.put("math.pow", &intrinsics.expr_std_pow);
        try instance.env.put("time.now", &intrinsics.expr_std_time_now);
        try instance.env.put("number?", &intrinsics.expr_std_is_number);
        try instance.env.put("symbol?", &intrinsics.expr_std_is_symbol);
        try instance.env.put("list?", &intrinsics.expr_std_is_list);
        try instance.env.put("hashmap?", &intrinsics.expr_std_is_hashmap);
        try instance.env.put("error?", &intrinsics.expr_std_is_err);
        try instance.env.put("callable?", &intrinsics.expr_std_is_callable);
        try instance.env.put("opaque?", &intrinsics.expr_std_is_opaque);
        try instance.env.put("lowercase?", &intrinsics.expr_std_is_lowercase);
        try instance.env.put("uppercase?", &intrinsics.expr_std_is_uppercase);
        try instance.env.put("lowercase", &intrinsics.expr_std_lowercase);
        try instance.env.put("uppercase", &intrinsics.expr_std_uppercase);
        try instance.env.put("verbose", &intrinsics.expr_std_verbose);
        try instance.env.put("assert", &intrinsics.expr_std_assert_true);
        try instance.env.put("gensym", &intrinsics.expr_std_gensym);
        try instance.env.put("print", &intrinsics.expr_std_print);
        try instance.env.put("string", &intrinsics.expr_std_string);
        try instance.env.put("as", &intrinsics.expr_std_as);
        try instance.env.put("len", &intrinsics.expr_std_len);
        try instance.env.put("contains?", &intrinsics.expr_std_contains);
        try instance.env.put("clone", &intrinsics.expr_std_clone);
        try instance.env.put("env", &intrinsics.expr_std_env);
        try instance.env.put("self", &intrinsics.expr_std_self);
        try instance.env.put("parent", &intrinsics.expr_std_parent);
        try instance.env.put("quote", &intrinsics.expr_std_quote);
        try instance.env.put("quasiquote", &intrinsics.expr_std_quasi_quote);
        try instance.env.put("unquote", &intrinsics.expr_std_unquote);
        try instance.env.put("unquote-splicing", &intrinsics.expr_std_unquote_splicing);
        try instance.env.put("double-quote", &intrinsics.expr_std_double_quote);
        try instance.env.put("range", &intrinsics.expr_std_range);
        try instance.env.put("rotate-left!", &intrinsics.expr_std_rotate_left);
        try instance.env.put("env.swap!", &intrinsics.expr_std_swap);
        try instance.env.put("item-at", &intrinsics.expr_std_item_at);
        try instance.env.put("item-set", &intrinsics.expr_std_item_set);
        try instance.env.put("item-remove!", &intrinsics.expr_std_item_remove);
        try instance.env.put("define", &intrinsics.expr_std_define);
        try instance.env.put("var", &intrinsics.expr_std_define);
        try instance.env.put("vars", &intrinsics.expr_std_vars);
        try instance.env.put("lambda", &intrinsics.expr_std_lambda);
        try instance.env.put("macro", &intrinsics.expr_std_macro);
        try instance.env.put("λ", &intrinsics.expr_std_lambda);
        try instance.env.put("apply", &intrinsics.expr_std_apply);
        try instance.env.put("list", &intrinsics.expr_std_list);
        try instance.env.put("hashmap.new", &intrinsics.expr_std_map_new);
        try instance.env.put("hashmap.put", &intrinsics.expr_std_map_put);
        try instance.env.put("hashmap.get", &intrinsics.expr_std_map_get);
        try instance.env.put("hashmap.remove", &intrinsics.expr_std_map_remove);
        try instance.env.put("hashmap.clear", &intrinsics.expr_std_map_clear);
        try instance.env.put("hashmap.keys", &intrinsics.expr_std_map_keys);
        try instance.env.put("loop", &intrinsics.expr_std_loop);
        try instance.env.put("append", &intrinsics.expr_std_append);
        try instance.env.put("eval", &intrinsics.expr_std_eval);
        try instance.env.put("eval-string", &intrinsics.expr_std_eval_string);
        try instance.env.put("string.split", &intrinsics.expr_std_split);
        try instance.env.put("atom.split", &intrinsics.expr_std_split_atom);
        try instance.env.put("set!", &intrinsics.expr_std_set);
        try instance.env.put("unset!", &intrinsics.expr_std_unset);
        try instance.env.put("try", &intrinsics.expr_std_try);
        try instance.env.put("error", &intrinsics.expr_std_error);
        try instance.env.put("and", &intrinsics.expr_std_logical_and);
        try instance.env.put("or", &intrinsics.expr_std_logical_or);
        try instance.env.put("not", &intrinsics.expr_std_logical_not);
        try instance.env.put("+", &intrinsics.expr_std_sum);
        try instance.env.put("-", &intrinsics.expr_std_sub);
        try instance.env.put("*", &intrinsics.expr_std_mul);
        try instance.env.put("/", &intrinsics.expr_std_div);
        try instance.env.put("=", &intrinsics.expr_std_eq);
        try instance.env.put("~=", &intrinsics.expr_std_eq_approx);
        try instance.env.put("^=", &intrinsics.expr_std_eq_reference);
        try instance.env.put("order", &intrinsics.expr_std_order);
        try instance.env.put("io.open-file", &intrinsics.expr_std_file_open);
        try instance.env.put("io.close-file", &intrinsics.expr_std_file_close);
        try instance.env.put("io.read-line", &intrinsics.expr_std_file_read_line);
        try instance.env.put("io.write-line", &intrinsics.expr_std_file_write_line);
        try instance.env.put("io.read-byte", &intrinsics.expr_std_file_read_byte);

        return instance;
    }

    /// Perform a full GC sweep and check for leaks
    pub fn deinit(_: *Interpreter) void {
        mem.gc.deinit();
        SourceLocation.deinitStack();
        if (!@import("builtin").is_test and mem.gpa.deinit()) {
            std.io.getStdOut().writer().print("Memory leaks detected\n", .{}) catch unreachable;
        }
    }

    /// Print user friendly errors
    pub fn printError(_: *Interpreter, err: anyerror) !void {
        const err_str = switch (err) {
            ExprErrors.AlreadyReported => return,
            ExprErrors.InvalidArgumentCount => "Invalid argument count",
            ExprErrors.InvalidArgumentType => "Invalid argument type",
            ExprErrors.ExpectedNumber => "Expected a number",
            ExprErrors.ExpectedBool => "Expected a boolean expression",
            ExprErrors.UnexpectedRightParen => "Unexpected )",
            ExprErrors.MissingRightParen => "Missing )",
            ExprErrors.SyntaxError => "Syntax error while parsing",
            ExprErrors.Eof => "End of file",
            else => "Unknown",
        };
        try std.io.getStdOut().writer().print("{s}\n", .{err_str});
    }

    /// Print a formatted error message, prefixed with source location
    pub fn printErrorFmt(self: *Interpreter, src_loc: *SourceLocation, comptime fmt: []const u8, args: anytype) !void {
        try std.io.getStdOut().writer().print("ERROR: {s}, line {d}: ", .{ src_loc.file, std.math.max(1, src_loc.line) });
        try std.io.getStdOut().writer().print(fmt, args);
        // for (SourceLocation.stack.items) |loc| {
        //     try std.io.getStdOut().writer().print("    {s}:{d}", .{loc.file, std.math.max(1, loc.line)});
        // }
        self.has_errors = true;
    }

    /// Run GC if needed, then parse and evaluate the expression
    pub fn parseAndEvalExpression(self: *Interpreter, line: []const u8) anyerror!?*Expr {
        mem.gc.runIfNeeded() catch {};
        var input = std.mem.trimRight(u8, line, "\r\n");

        // Ignore empty lines and comments
        if (input.len == 0 or input[0] == ';') {
            return null;
        }
        var expr = try self.parse(input);
        return try self.eval(self.env, expr);
    }

    /// Parse Bio source code into Expr objects
    pub fn parse(self: *Interpreter, input: []const u8) !*Expr {
        var it = Lisperator{
            .index = 0,
            .buffer = input,
        };
        return self.read(&it);
    }

    /// Evaluate an expression
    pub fn eval(self: *Interpreter, environment: *Env, expr: *Expr) anyerror!*Expr {
        var maybe_next: ?*Expr = expr;
        var env: *Env = environment;
        var seen_macro_expand = false;
        self.has_errors = false;

        tailcall_optimization_loop: while (maybe_next) |e| {
            if (self.exit_code) |_| {
                return &intrinsics.expr_atom_nil;
            }
            if (e == &intrinsics.expr_atom_break) {
                self.break_seen = true;
                return &intrinsics.expr_atom_nil;
            }
            switch (e.val) {
                ExprValue.num, ExprValue.env, ExprValue.any => {
                    return e;
                },
                ExprValue.sym => |sym| {
                    if (env.lookup(sym, true)) |val| {
                        return val;
                    } else {
                        try self.printErrorFmt(&e.src, "{s} is not defined\n", .{sym});
                        return &intrinsics.expr_atom_nil;
                    }
                },
                ExprValue.lst => |list| {
                    if (list.items.len == 0) {
                        return &intrinsics.expr_atom_nil;
                    }

                    const args_slice = list.items[1..];
                    if (list.items[0] == &intrinsics.expr_atom_macroexpand) {
                        // Signal that we don't want to evaluate the expression returned by the macro
                        seen_macro_expand = true;
                        maybe_next = args_slice[args_slice.len - 1];
                        continue;
                    } else if (list.items[0] == &intrinsics.expr_atom_begin) {
                        var res: *Expr = &intrinsics.expr_atom_nil;
                        for (args_slice[0 .. args_slice.len - 1]) |arg| {
                            res = try self.eval(env, arg);
                        }
                        maybe_next = args_slice[args_slice.len - 1];
                        continue;
                    } else if (list.items[0] == &intrinsics.expr_atom_cond) {
                        try intrinsics.requireMinimumArgCount(2, args_slice);

                        const else_branch = args_slice[args_slice.len - 1];
                        if (else_branch.val != ExprType.lst or else_branch.val.lst.items.len != 1) {
                            try self.printErrorFmt(&e.src, "Last expression in cond must be a single-expression list\n", .{});
                            return ExprErrors.AlreadyReported;
                        }

                        for (args_slice) |branch| {
                            if (branch == else_branch) {
                                maybe_next = branch.val.lst.items[0];
                                continue :tailcall_optimization_loop;
                            } else if (branch.val == ExprType.lst) {
                                try intrinsics.requireExactArgCount(2, branch.val.lst.items);

                                const predicate = try self.eval(env, branch.val.lst.items[0]);
                                if (predicate == &intrinsics.expr_atom_true) {
                                    maybe_next = branch.val.lst.items[1];
                                    continue :tailcall_optimization_loop;
                                }
                            } else {
                                try self.printErrorFmt(&e.src, "Invalid switch syntax\n", .{});
                                return ExprErrors.AlreadyReported;
                            }
                        }
                        return &intrinsics.expr_atom_nil;
                    } else if (list.items[0] == &intrinsics.expr_atom_if) {
                        try intrinsics.requireMinimumArgCount(2, args_slice);

                        var branch: usize = 1;
                        const predicate = try self.eval(env, args_slice[0]);
                        if (predicate != &intrinsics.expr_atom_true) {
                            // Anything not defined as falsy is considered true in a boolean context
                            if (intrinsics.isFalsy(predicate)) {
                                branch += 1;
                            }
                        }

                        if (branch < args_slice.len) {
                            maybe_next = args_slice[branch];
                            continue :tailcall_optimization_loop;
                        } else {
                            return &intrinsics.expr_atom_nil;
                        }
                    }

                    // Look up std function or lambda. If not found, the lookup has already reported the error.
                    var func = try self.eval(env, list.items[0]);
                    if (func == &intrinsics.expr_atom_nil) {
                        return &intrinsics.expr_atom_nil;
                    }

                    const kind = func.val;
                    switch (kind) {
                        ExprValue.env => |target_env| {
                            if (args_slice.len == 0 or (args_slice[0].val != ExprType.sym and args_slice[0].val != ExprType.lst)) {
                                try self.printErrorFmt(&e.src, "Missing symbol or call in environment lookup: ", .{});
                                if (args_slice.len > 0) {
                                    try args_slice[0].print();
                                } else {
                                    try self.printErrorFmt(&e.src, "[no argument]\n", .{});
                                }
                                return ExprErrors.AlreadyReported;
                            }

                            if (args_slice[0].val == ExprType.lst) {
                                maybe_next = args_slice[0];
                                env = target_env;
                                continue :tailcall_optimization_loop;
                            } else if (target_env.lookup(args_slice[0].val.sym, false)) |match| {
                                return match;
                            } else {
                                try self.printErrorFmt(&e.src, "Symbol not found in given environment: {s}\n", .{args_slice[0].val.sym});
                                return ExprErrors.AlreadyReported;
                            }
                        },

                        // Evaluate an intrinsic function
                        ExprValue.fun => |fun| {
                            return fun(self, env, args_slice) catch |err| {
                                try self.printErrorFmt(&e.src, "", .{});
                                try self.printError(err);
                                return &intrinsics.expr_atom_nil;
                            };
                        },

                        // Evaluate a previously defined lambda or macro
                        ExprValue.lam, ExprValue.mac => |fun| {
                            try intrinsics.requireType(self, fun.items[0], ExprType.lst);
                            const parent_env = if (kind == ExprType.lam) func.env else env;
                            const kind_str = if (kind == ExprType.lam) "lambda" else "macro";

                            var local_env = try ast.makeEnv(parent_env, kind_str);
                            var formal_param_count = fun.items[0].val.lst.items.len;
                            var logical_arg_count = args_slice.len;

                            // Bind arguments to the new environment
                            for (fun.items[0].val.lst.items) |param, index| {
                                if (param.val == ExprType.sym) {
                                    if (param == &intrinsics.expr_atom_rest) {
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
                                        try local_env.putWithSymbol(fun.items[0].val.lst.items[index + 1], rest_args);
                                        break;
                                    }

                                    // Arguments are eagerly evaluated for lambdas, lazily for macros
                                    if (index < args_slice.len) {
                                        if (kind == ExprType.lam) {
                                            try local_env.putWithSymbol(param, try self.eval(env, args_slice[index]));
                                        } else {
                                            try local_env.putWithSymbol(param, args_slice[index]);
                                        }
                                    }
                                } else {
                                    try self.printErrorFmt(&e.src, "Formal parameter to {s} is not a symbol: ", .{kind_str});
                                    try param.print();
                                }
                            }

                            if (logical_arg_count != formal_param_count) {
                                try self.printErrorFmt(&e.src, "{s} received {d} arguments, expected {d}\n", .{ kind_str, args_slice.len, formal_param_count });
                                return &intrinsics.expr_atom_nil;
                            }

                            // Evaluate body, except the last expression which is TCO'ed
                            var result: *Expr = undefined;
                            for (fun.items[1 .. fun.items.len - 1]) |body_expr| {
                                result = self.eval(local_env, body_expr) catch |err| {
                                    try self.printErrorFmt(&body_expr.src, "Could not evaluate {s} body:", .{kind_str});
                                    try self.printError(err);
                                    return &intrinsics.expr_atom_nil;
                                };
                                if (self.has_errors) {
                                    std.debug.print("--- There are errors\n", .{});
                                    return &intrinsics.expr_atom_nil;
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
                                    try self.printErrorFmt(&last_expr.src, "Could not evaluate {s} body:", .{kind_str});
                                    try self.printError(err);
                                    return &intrinsics.expr_atom_nil;
                                };

                                if (seen_macro_expand) {
                                    return result;
                                }

                                env = local_env;
                                maybe_next = result;
                                continue;
                            }
                        },
                        else => {
                            try self.printErrorFmt(&e.src, "Not a function or macro: ", .{});
                            try list.items[0].print();
                            return &intrinsics.expr_atom_nil;
                        },
                    }
                    return &intrinsics.expr_atom_nil;
                },
                // This allows us to create intrinsics that pass intrinsics as arguments
                ExprValue.fun => {
                    return e;
                },
                else => {
                    try self.printErrorFmt(&e.src, "Invalid expression: {}\n", .{e});
                    return &intrinsics.expr_atom_nil;
                },
            }
        }
        return &intrinsics.expr_atom_nil;
    }

    /// Recursively read expressions
    pub fn read(self: *Interpreter, it: *Lisperator) anyerror!*Expr {
        if (it.next()) |val| {
            if (val.len > 0) {
                switch (val[0]) {
                    '(' => {
                        var list = try ast.makeListExpr(null);

                        while (it.peek()) |peek| {
                            if (peek.len == 0) {
                                return ExprErrors.SyntaxError;
                            }
                            if (peek[0] != ')') {
                                try list.val.lst.append(try self.read(it));
                            } else {
                                break;
                            }
                        }
                        _ = it.next();
                        return list;
                    },
                    ')' => {
                        return ExprErrors.UnexpectedRightParen;
                    },
                    ',' => {
                        var unquote_op = &intrinsics.expr_atom_unquote;
                        var index_adjust: usize = 1;

                        // The Lisperator has no understanding of the , and ,@ prefixes, so we adjust it manually
                        // For instance, if the current token is ,@abc then the next token will be abc
                        if (val.len > 1 and val[1] == '@') {
                            unquote_op = &intrinsics.expr_atom_unquote_splicing;
                            index_adjust += 1;
                        }

                        if (index_adjust > 0) {
                            it.index = it.prev_index + index_adjust;
                        }

                        return try ast.makeListExpr(&.{ unquote_op, try self.read(it) });
                    },
                    '\'' => {
                        return try ast.makeListExpr(&.{ &intrinsics.expr_atom_quote, try self.read(it) });
                    },
                    '`' => {
                        return try ast.makeListExpr(&.{ &intrinsics.expr_atom_quasi_quote, try self.read(it) });
                    },
                    '"' => {
                        return try ast.makeListExpr(&.{ &intrinsics.expr_atom_quote, try ast.makeAtomByDuplicating(val[1..val.len]) });
                    },
                    else => {
                        return ast.makeAtomByDuplicating(val);
                    },
                }
            } else {
                return ExprErrors.SyntaxError;
            }
        } else {
            return ExprErrors.Eof;
        }
    }

    /// This function helps us read a Bio expression that may span multiple lines
    /// If too many )'s are detected, an error is returned. We make sure to not
    /// count parenthesis inside string literals.
    pub fn readBalancedExpr(self: *Interpreter, reader: anytype, prompt: []const u8) anyerror!?[]u8 {
        var balance: isize = 0;
        var expr = std.ArrayList(u8).init(mem.allocator);
        defer expr.deinit();
        var expr_writer = expr.writer();

        try linereader.linenoise_wrapper.printPrompt(prompt);
        reader_loop: while (true) {
            if (reader.readUntilDelimiterOrEofAlloc(mem.allocator, '\n', 2048)) |maybe| {
                if (maybe) |line| {
                    defer mem.allocator.free(line);
                    SourceLocation.current().line += 1;

                    var only_seen_ws = true;
                    var inside_string = false;
                    for (line) |char| {
                        if (char == ';' and only_seen_ws) {
                            continue :reader_loop;
                        }
                        only_seen_ws = only_seen_ws and std.ascii.isWhitespace(char);

                        if (char == '"') {
                            inside_string = !inside_string;
                        }

                        if (!inside_string) {
                            if (char == '(') {
                                balance += 1;
                            } else if (char == ')') {
                                balance -= 1;
                            }
                        }
                    }

                    if (expr.items.len > 0) {
                        try expr_writer.writeAll(" ");
                    }

                    try expr_writer.writeAll(line);
                } else {
                    if (balance > 0 and prompt.len == 0) {
                        return ExprErrors.MissingRightParen;
                    }
                    return null;
                }
            } else |err| {
                try self.printErrorFmt(SourceLocation.current(), "readUntilDelimiterOrEofAlloc failed {}\n", .{err});
            }

            if (balance <= 0) {
                break;
            } else {
                linereader.linenoise_wrapper.hidePrompt();
            }
        }

        if (balance < 0) {
            std.debug.print("Missing )\n", .{});
            return ExprErrors.MissingRightParen;
        }

        return try expr.toOwnedSlice();
    }

    /// REPL
    pub fn readEvalPrint(self: *Interpreter) !void {
        const logo =
            \\
            \\       ..           ..
            \\     pd   '(Ob.      `bq
            \\    6P        M        YA             Bio is a Lisp written in Zig
            \\   6M'        db       `Mb         Docs at github.com/cryptocode/bio
            \\   MN        BIO.       8M
            \\   MN       AM'`M       8M           Use arrow up/down for history
            \\   YM.     ,M'  db     ,M9          You can also run bio files with
            \\    Mb    JM'    Yb./ .dM                   "bio run <file>"
            \\     Yq .            .pY
            \\        ``          ''
        ;

        try std.io.getStdOut().writer().print("{s}\n\n", .{logo});
        while (true) {
            if (self.readBalancedExpr(&linereader.linenoise_reader, "bio> ")) |maybe| {
                if (maybe) |input| {
                    defer mem.allocator.free(input);
                    _ = try linereader.linenoise_wrapper.addToHistory(input);
                    var maybeResult = self.parseAndEvalExpression(input) catch |err| {
                        try self.printErrorFmt(SourceLocation.current(), "read-eval failed: \n", .{});
                        try self.printError(err);
                        continue;
                    };

                    if (maybeResult) |res| {
                        // Update the last expression and print it
                        try self.env.put("#?", res);
                        if (res != &intrinsics.expr_atom_nil or self.verbose) {
                            try res.print();
                        }
                        try std.io.getStdOut().writer().print("\n\n", .{});
                    }

                    if (self.exit_code) |exit_code| {
                        // Defer is not called as exit is [noreturn]
                        mem.allocator.free(input);
                        self.deinit();
                        std.process.exit(exit_code);
                    }
                }
            } else |err| {
                try self.printError(err);
            }
        }
    }
};

/// A tokenizing iterator for Lisp expression
pub const Lisperator = struct {
    buffer: []const u8,
    index: usize = 0,
    prev_index: usize = 0,

    pub fn peek(self: *Lisperator) ?[]const u8 {
        const index_now = self.index;
        const val = self.next();
        self.index = index_now;
        return val;
    }

    /// Returns a slice of the next token, or null if tokenizing is complete
    pub fn next(self: *Lisperator) ?[]const u8 {
        if (self.index >= self.buffer.len) {
            return null;
        }

        while (self.index + 1 < self.buffer.len and (self.buffer[self.index] == ' ' or self.buffer[self.index] == '\t')) {
            self.index += 1;
        }

        var start = self.index;
        self.prev_index = start;

        if (self.buffer[self.index] == '"') {
            while (self.index + 1 < self.buffer.len and (self.buffer[self.index + 1] != '"')) {
                self.index += 1;
            }
            if (self.index + 1 == self.buffer.len or self.buffer[self.index + 1] != '"') {
                std.io.getStdOut().writer().print("Unterminated string literal\n", .{}) catch unreachable;
                return null;
            }
            self.index += 1;
            defer self.index += 1;
            return self.buffer[start..self.index];
        }

        if (self.buffer[self.index] == '(' or self.buffer[self.index] == ')' or self.buffer[self.index] == '\'' or self.buffer[self.index] == '`') {
            self.index += 1;
            return self.buffer[start .. start + 1];
        } else {
            if (std.mem.indexOfAnyPos(u8, self.buffer, start, " \t)(")) |delim_start| {
                const res = self.buffer[start..delim_start];
                self.index = delim_start;
                return std.mem.trim(u8, res, "\r\n\t ");
            } else if (self.index <= self.buffer.len) {
                return std.mem.trim(u8, self.buffer[start..self.buffer.len], "\r\n\t ");
            } else {
                return null;
            }
        }
    }
};
