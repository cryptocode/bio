const std = @import("std");
const interpreter = @import("interpreter.zig");
const intrinsics = @import("intrinsics.zig");
const sourcelocation = @import("sourcelocation.zig");
const gc = @import("boehm.zig");

const SourceLocation = sourcelocation.SourceLocation;
var interned_intrinsics: std.StringArrayHashMapUnmanaged(*Expr) = .{};
var interned_syms: std.StringArrayHashMapUnmanaged(void) = .{};
var interned_nums: std.AutoHashMapUnmanaged(i16, *Expr) = .{};

pub const IntrinsicFn = *const fn (evaluator: *interpreter.Interpreter, env: *Env, []const *Expr) anyerror!*Expr;
pub const ExprErrors = error{ AlreadyReported, MissingRightParen, UnexpectedRightParen, ExpectedNumber, ExpectedBool, InvalidArgumentType, InvalidArgumentCount, SyntaxError, Eof, BindingNotFound };
pub const ExprType = enum(u8) { sym, num, lst, map, lam, mac, fun, env, err, any };
pub const ExprValue = union(ExprType) {
    sym: []const u8,
    num: f64,
    lst: std.ArrayList(*Expr),
    map: std.ArrayHashMap(*Expr, *Expr, Expr.HashUtil, true),
    lam: std.ArrayList(*Expr),
    mac: std.ArrayList(*Expr),
    fun: IntrinsicFn,
    env: *Env,
    err: *Expr,
    /// Type-erased value, such as a file descriptor or a pointer to a struct
    any: usize,
};

/// A Bio expression with a value and an optional environment (lambda expressions
/// must know in which environment they were defined)
pub const Expr = struct {
    /// Hashmap support for Expr
    pub const HashUtil = struct {
        pub fn hash(_: HashUtil, key: *Expr) u32 {
            if (key.val == ExprType.sym) {
                return @as(u32, @truncate(std.hash.Wyhash.hash(0, key.val.sym)));
            } else if (key.val == ExprType.num) {
                return @as(u32, @truncate(@as(u64, @intFromFloat(key.val.num))));
            }
            @panic("Invalid hash key type");
        }
        pub fn eql(_: HashUtil, first: *Expr, second: *Expr, b_index: usize) bool {
            _ = b_index;
            if (first.val == ExprType.sym) {
                return std.mem.eql(u8, first.val.sym, second.val.sym);
            } else if (first.val == ExprType.num) {
                return first.val.num == second.val.num;
            }
            @panic("Invalid hash key type");
        }
    };

    val: ExprValue,
    env: ?*Env = null,
    src: SourceLocation = .{},

    /// Create a new expression with an undefined value
    pub fn create(_: bool) !*@This() {
        var allocator = gc.allocator();
        var self = try allocator.create(Expr);
        self.* = Expr{ .val = undefined };
        self.src.file = SourceLocation.current().file;
        self.src.line = SourceLocation.current().line;
        self.src.col = SourceLocation.current().col;
        return self;
    }

    /// Called by the GC to clean up expression resources. Note that symbols
    /// are deallocated by sweeping the internalized string map.
    pub fn deinit(self: *@This()) void {
        if (self.val == ExprType.lst) {
            self.val.lst.deinit();
        } else if (self.val == ExprType.map) {
            self.val.map.deinit();
        } else if (self.val == ExprType.lam) {
            self.val.lam.deinit();
        } else if (self.val == ExprType.mac) {
            self.val.mac.deinit();
        }
    }

    /// Returns an owned string representation of this expression
    pub fn toStringAlloc(self: *@This()) anyerror![]u8 {
        var allocator = gc.allocator();
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
                return try std.fmt.allocPrint(allocator, "{s}", .{err_str});
            },
            ExprValue.lst => |lst| {
                var buf = std.ArrayList(u8).init(allocator);
                defer buf.deinit();
                var bufWriter = buf.writer();

                try bufWriter.writeAll("(");
                for (lst.items, 0..) |item, index| {
                    const itemBuf = try item.toStringAlloc();
                    try bufWriter.writeAll(itemBuf);
                    if (index + 1 < lst.items.len) {
                        try bufWriter.writeAll(" ");
                    }
                }
                try bufWriter.writeAll(")");
                return buf.toOwnedSlice();
            },
            ExprValue.map => |map| {
                var buf = std.ArrayList(u8).init(allocator);
                defer buf.deinit();
                var bufWriter = buf.writer();

                // Output is ((key val)(key val)(key val))
                try bufWriter.writeAll("(");
                var it = map.iterator();
                while (it.next()) |entry| {
                    const key = try entry.key_ptr.*.toStringAlloc();
                    const val = try entry.value_ptr.*.toStringAlloc();

                    try bufWriter.writeAll("(");
                    try bufWriter.writeAll(key);
                    try bufWriter.writeAll(" ");
                    try bufWriter.writeAll(val);
                    try bufWriter.writeAll(")");
                }
                try bufWriter.writeAll(")");
                return try buf.toOwnedSlice();
            },
        }
    }

    /// Prints the expression to stdout
    pub fn print(self: *@This()) anyerror!void {
        const str = try self.toStringAlloc();
        try std.io.getStdOut().writer().print("{s}", .{str});
    }
};

/// Environment for variable bindings. Instances are named to get friendly debugging output.
pub const Env = struct {
    // This is the same as Expr.HashUtil, but is duplicated here to dodge a `get_slice_type` compiler bug
    pub const HashUtil = struct {
        pub fn hash(_: HashUtil, key: *Expr) u32 {
            if (key.val == ExprType.sym) {
                return @as(u32, @truncate(std.hash.Wyhash.hash(0, key.val.sym)));
            } else if (key.val == ExprType.num) {
                return @as(u32, @truncate(@as(u64, @intFromFloat(key.val.num))));
            }
            @panic("Invalid hash key type");
        }
        pub fn eql(_: HashUtil, first: *Expr, second: *Expr, b_index: usize) bool {
            _ = b_index;
            if (first.val == ExprType.sym) {
                return std.mem.eql(u8, first.val.sym, second.val.sym);
            } else if (first.val == ExprType.num) {
                return first.val.num == second.val.num;
            }
            @panic("Invalid hash key type");
        }
    };

    map: std.ArrayHashMap(*Expr, *Expr, Env.HashUtil, true),
    parent: ?*@This() = null,
    name: []const u8,

    pub fn deinit(self: *@This()) void {
        self.map.deinit();
    }

    /// Put symbol/value, duplicating the key
    pub fn put(self: *@This(), key: []const u8, val: *Expr) !void {
        const binding_expr = try makeAtomByDuplicating(key);
        try self.putWithSymbol(binding_expr, val);
    }

    /// Put symbol/value
    pub fn putWithSymbol(self: *@This(), variable_name: *Expr, val: *Expr) anyerror!void {
        try self.map.put(variable_name, val);
    }

    /// Look up a variable in this or a parent environment
    pub fn lookup(self: *@This(), sym: []const u8, recursive: bool) ?*Expr {
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
    /// If the binding is found, then then `val` is returned. This makes
    /// it possible to implement macros such as =+
    /// If the binding is not found, an error is returned.
    pub fn replace(self: *Env, var_name: *Expr, val: ?*Expr) !*Expr {
        if (self.map.get(var_name)) |_| {
            if (val) |value| {
                try self.putWithSymbol(var_name, value);
                return value;
            } else {
                _ = self.map.swapRemove(var_name);
                return &intrinsics.expr_atom_nil;
            }
        } else if (self.parent) |parent| {
            return try parent.replace(var_name, val);
        }

        return ExprErrors.BindingNotFound;
    }
};

/// Make an environment expression
pub fn makeEnv(parent: ?*Env, name: []const u8) !*Env {
    var allocator = gc.allocator();
    var environment = try allocator.create(Env);
    environment.parent = parent;
    environment.map = @TypeOf(environment.map).init(allocator);
    environment.name = name;
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
    var allocator = gc.allocator();
    const intrinsic_atoms: []const *Expr = &.{
        &intrinsics.expr_atom_quasi_quote,  &intrinsics.expr_atom_quote,          &intrinsics.expr_atom_unquote, &intrinsics.expr_atom_unquote_splicing, &intrinsics.expr_atom_list,
        &intrinsics.expr_atom_if,           &intrinsics.expr_atom_cond,           &intrinsics.expr_atom_begin,   &intrinsics.expr_atom_nil,              &intrinsics.expr_atom_rest,
        &intrinsics.expr_atom_eval,         &intrinsics.expr_atom_mut,            &intrinsics.expr_atom_true,    &intrinsics.expr_atom_false,            &intrinsics.expr_atom_last_eval,
        &intrinsics.expr_atom_last_try_err, &intrinsics.expr_atom_last_try_value, &intrinsics.expr_atom_break,   &intrinsics.expr_atom_macroexpand,
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
                const internalizable = @floor(num) == num and !std.math.isInf(num) and num > -1024 and num < 1024;
                if (internalizable) {
                    if (interned_nums.get(@as(i16, @intFromFloat(num)))) |expr| {
                        return expr;
                    }
                }

                var expr = try Expr.create(false);
                expr.val = ExprValue{ .num = num };
                if (internalizable) {
                    try interned_nums.put(allocator, @as(i16, @intFromFloat(num)), expr);
                }
                return expr;
            } else |_| {}
        }

        return try makeAtomLiteral(literal, take_ownership);
    };
}

/// Make an interned literal atom
pub fn makeAtomLiteral(literal: []const u8, take_ownership: bool) anyerror!*Expr {
    var allocator = gc.allocator();
    var sym = sym_blk: {
        const maybe_entry = interned_syms.getEntry(literal);
        if (maybe_entry) |entry| {
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
/// If `initial_expressions` is not null, each item as added *without evaluation*
pub fn makeListExpr(initial_expressions: ?[]const *Expr) !*Expr {
    var allocator = gc.allocator();
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .lst = std.ArrayList(*Expr).init(allocator) };
    if (initial_expressions) |expressions| {
        for (expressions) |e| {
            try expr.val.lst.append(e);
        }
    }
    return expr;
}

/// Make a hashmap expression
/// If `initial_expressions` is not null, each item as added *without evaluation*
pub fn makeHashmapExpr(initial_expressions: ?[]const *Expr) !*Expr {
    var allocator = gc.allocator();
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .map = std.ArrayHashMap(*Expr, *Expr, Expr.HashUtil, true).init(allocator) };

    if (initial_expressions) |expressions| {
        for (expressions) |e| {
            try expr.val.map.put(e.val.lst.items[0], e.val.lst.items[1]);
        }
    }
    return expr;
}

/// Make a lambda expression
pub fn makeLambdaExpr(env: *Env) !*Expr {
    var allocator = gc.allocator();
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
    var allocator = gc.allocator();
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
