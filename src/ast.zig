const std = @import("std");
usingnamespace @import("interpreter.zig");
usingnamespace @import("intrinsics.zig");
usingnamespace @import("gc.zig");
usingnamespace @import("sourcelocation.zig");

pub const ExprErrors = error{ AlreadyReported, UnexpectedRightParen, ExpectedNumber, InvalidArgumentType, InvalidArgumentCount, SyntaxError, Eof };
pub const ExprType = enum { sym, num, lst, lam, mac, fun, env, err, any };
pub const ExprValue = union(ExprType) {
    sym: []const u8,
    num: f64,
    lst: std.ArrayList(*Expr),
    lam: std.ArrayList(*Expr),
    mac: std.ArrayList(*Expr),
    fun: fn (evaluator: *Interpreter, env: *Env, []const *Expr) anyerror!*Expr,
    env: *Env,
    err: *Expr,
    any: usize,
};

/// A Bio expression with a value and an optional environment (lambda expressions
/// must know in which environment they were defined)
pub const Expr = struct {
    val: ExprValue,
    env: ?*Env = null,
    src: SourceLocation = .{},

    /// Create a new expression with an undefined value
    pub fn create(register_with_gc: bool) !*Expr {
        var self = try allocator.create(Expr);
        if (register_with_gc) {
            gc.inc();
            try gc.registered_expr.append(self);
        }
        self.* = Expr{ .val = undefined };
        self.src.file = SourceLocation.current().file;
        self.src.line = SourceLocation.current().line;
        self.src.col = SourceLocation.current().col;
        return self;
    }

    /// Called by the GC to clean up expression resources. Note that symbols
    /// are deallocated by sweeping the internalized string map.
    pub fn deinit(self: *Expr) void {
        if (self.val == ExprType.lst) {
            self.val.lst.deinit();
        } else if (self.val == ExprType.lam) {
            self.val.lam.deinit();
        } else if (self.val == ExprType.mac) {
            self.val.mac.deinit();
        }
    }

    /// Returns an owned string representation of this expression
    pub fn toStringAlloc(self: *Expr) anyerror![]u8 {
        switch (self.val) {
            ExprValue.sym => return try std.fmt.allocPrint(allocator, "{s}", .{self.val.sym}),
            ExprValue.num => return try std.fmt.allocPrint(allocator, "{d}", .{self.val.num}),
            ExprValue.lam => return try std.fmt.allocPrint(allocator, "<lambda>", .{}),
            ExprValue.mac => return try std.fmt.allocPrint(allocator, "<macro>", .{}),
            ExprValue.fun => return try std.fmt.allocPrint(allocator, "<function>", .{}),
            ExprValue.env => return try std.fmt.allocPrint(allocator, "<env>", .{}),
            ExprValue.any => return try std.fmt.allocPrint(allocator, "<any>", .{}),
            ExprValue.err => |err_expr| {
                const err_str = try err_expr.toStringAlloc();
                defer allocator.free(err_str);
                return try std.fmt.allocPrint(allocator, "{s}", .{err_str});
            },
            ExprValue.lst => |lst| {
                var buf = std.ArrayList(u8).init(allocator);
                defer buf.deinit();
                var bufWriter = buf.writer();

                try bufWriter.writeAll("(");
                for (lst.items) |item, index| {
                    const itemBuf = try item.toStringAlloc();
                    defer allocator.free(itemBuf);
                    try bufWriter.writeAll(itemBuf);
                    if (index + 1 < lst.items.len) {
                        try bufWriter.writeAll(" ");
                    }
                }
                try bufWriter.writeAll(")");

                return buf.toOwnedSlice();
            },
        }
    }

    /// Prints the expression to stdout
    pub fn print(self: *Expr) anyerror!void {
        const str = try self.toStringAlloc();
        defer allocator.free(str);
        try std.io.getStdOut().writer().print("{s}", .{str});
    }
};

/// Environment for variable bindings. Instances are named to get friendly debugging output.
pub const Env = struct {
    const hash_util = struct {
        pub fn hash(_: hash_util, key: *Expr) u32 {
            std.debug.assert(key.val == ExprType.sym);
            return @truncate(u32, std.hash.Wyhash.hash(0, key.val.sym));
        }
        pub fn eql(_: hash_util, first: *Expr, second: *Expr) bool {
            std.debug.assert(first.val == ExprType.sym and second.val == ExprType.sym);
            return std.mem.eql(u8, first.val.sym, second.val.sym);
        }
    };

    map: std.ArrayHashMap(*Expr, *Expr, hash_util, true),
    parent: ?*Env = null,
    name: []const u8,

    pub fn deinit(self: *Env) void {
        self.map.deinit();
    }

    /// Put symbol/value by first interning the symbol
    pub fn put(self: *Env, key: []const u8, val: *Expr) !void {
        const binding_expr = try makeAtomByDuplicating(key);
        try self.putWithSymbol(binding_expr, val);
    }

    /// Put symbol/value
    pub fn putWithSymbol(self: *Env, variable_name: *Expr, val: *Expr) anyerror!void {
        try self.map.put(variable_name, val);
    }

    /// Look up a variable in this or a parent environment
    pub fn lookup(self: *Env, sym: []const u8, recursive: bool) ?*Expr {
        var lookupSym = Expr{ .val = ExprValue{ .sym = sym } };
        if (self.map.get(&lookupSym)) |val| {
            return val;
        } else if (self.parent) |parent| {
            return if (recursive) parent.lookup(sym, recursive) else null;
        } else {
            return null;
        }
    }

    /// Recursively search for the binding, replace it if found.
    /// If the new value is null, the binding is removed instead.
    pub fn replace(self: *Env, var_name: *Expr, val: ?*Expr) *Expr {
        if (self.map.get(var_name)) |_| {
            if (val) |value| {
                self.putWithSymbol(var_name, value) catch return &expr_atom_nil;
                return value;
            } else {
                _ = self.map.swapRemove(var_name);
            }
        } else if (self.parent) |parent| {
            return parent.replace(var_name, val);
        }

        return &expr_atom_nil;
    }
};

/// Make an environment expression
pub fn makeEnv(parent: ?*Env, name: []const u8) !*Env {
    var environment = try allocator.create(Env);
    environment.parent = parent;
    environment.map = @TypeOf(environment.map).init(allocator);
    environment.name = name;
    try gc.registered_envs.append(environment);
    return environment;
}

/// Duplicates the input and return an atom
pub fn makeAtomByDuplicating(literal: []const u8) anyerror!*Expr {
    return try makeAtomImplementation(literal, false);
}

/// Takes ownership of the input and returns an atom
pub fn makeAtomAndTakeOwnership(literal: []const u8) anyerror!*Expr {
    return try makeAtomImplementation(literal, true);
}

/// Make and return a potentially interned atom (symbol or number)
fn makeAtomImplementation(literal: []const u8, take_ownership: bool) anyerror!*Expr {
    const intrinsic_atoms: []const *Expr = &.{
        &expr_atom_quasi_quote, &expr_atom_quote, &expr_atom_unquote,   &expr_atom_unquote_splicing, &expr_atom_list,
        &expr_atom_if,          &expr_atom_cond,  &expr_atom_begin,     &expr_atom_nil,              &expr_atom_rest,
        &expr_atom_true,        &expr_atom_false, &expr_atom_last_eval, &expr_atom_last_try_err,     &expr_atom_last_try_value,
    };

    // Lazy initialization of the interned intrinsics map
    if (interned_intrinsics.count() == 0) {
        for (intrinsic_atoms) |atom| {
            try interned_intrinsics.put(allocator, atom.val.sym, atom);
        }
    }

    return interned_intrinsics.get(literal) orelse {
        // Zig's parseFloat is too lenient and accepts input like "." and "--"
        // For Bio, we require at least one digit.
        if (std.mem.indexOfAny(u8, literal, "0123456789")) |_| {
            if (std.fmt.parseFloat(f64, literal)) |num| {
                defer {
                    if (take_ownership) {
                        allocator.free(literal);
                    }
                }
                const internalizable = @floor(num) == num and !std.math.isInf(num) and num > -1024 and num < 1024;
                if (internalizable) {
                    if (interned_nums.get(@floatToInt(i16, num))) |expr| {
                        return expr;
                    }
                }

                var expr = try Expr.create(false);
                expr.val = ExprValue{ .num = num };
                if (internalizable) {
                    try interned_nums.put(allocator, @floatToInt(i16, num), expr);
                } else {
                    try gc.registered_expr.append(expr);
                }
                return expr;
            } else |_| {}
        }

        return try makeAtomLiteral(literal, take_ownership);
    };
}

/// Make an interned literal atom
pub fn makeAtomLiteral(literal: []const u8, take_ownership: bool) anyerror!*Expr {
    var sym = sym_blk: {
        const maybe_entry = interned_syms.getEntry(literal);
        if (maybe_entry) |entry| {
            // There's already an entry, free the input if we're supposed to take ownership
            if (take_ownership) {
                allocator.free(literal);
            }
            break :sym_blk entry.key_ptr.*;
        } else {
            const res = if (take_ownership) literal else try allocator.dupe(u8, literal);
            try interned_syms.put(allocator, res, {});
            break :sym_blk res;
        }
    };

    var expr = try Expr.create(true);
    expr.val = ExprValue{ .sym = sym };
    return expr;
}

/// Make a list expression
pub fn makeListExpr(initial_expressions: ?[]const *Expr) !*Expr {
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .lst = std.ArrayList(*Expr).init(allocator) };
    if (initial_expressions) |expressions| {
        for (expressions) |e| {
            try expr.val.lst.append(e);
        }
    }
    return expr;
}

/// Make a lambda expression
pub fn makeLambdaExpr(env: *Env) !*Expr {
    var expr = try Expr.create(true);

    // This is a crucial detail: we're recording the environment that existed at the
    // time of lambda definition. This will be the parent environment whenever we
    // are invoking the lambda in Interpreter#eval
    expr.env = env;
    expr.val = ExprValue{ .lam = std.ArrayList(*Expr).init(allocator) };
    return expr;
}

/// Make a macro expression
pub fn makeMacroExpr() !*Expr {
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .mac = std.ArrayList(*Expr).init(allocator) };
    return expr;
}

/// Make a numeric expression
pub fn makeNumExpr(num: f64) !*Expr {
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .num = num };
    return expr;
}

/// Make an error expression
pub fn makeError(expr: *Expr) !*Expr {
    var error_expr = try Expr.create(true);
    error_expr.val = ExprValue{ .err = expr };
    return error_expr;
}
