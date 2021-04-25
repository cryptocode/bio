const std = @import("std");
usingnamespace @import("ast.zig");

pub var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
pub var allocator = &gpa.allocator;
pub var interned_syms: std.StringArrayHashMapUnmanaged(void) = .{};
pub var interned_nums: std.AutoHashMapUnmanaged(i16, *Expr) = .{};
pub var interned_intrinsics: std.StringArrayHashMapUnmanaged(*Expr) = .{};

/// Mark and sweep garbage collector
pub const GC = struct {
    const stdout = std.io.getStdOut().writer();
    allocations: usize,
    registered_expr: std.ArrayList(*Expr) = undefined,
    registered_envs: std.ArrayList(*Env) = undefined,

    pub fn init() !GC {
        return GC{
            .allocations = 0,
            .registered_expr = std.ArrayList(*Expr).init(allocator),
            .registered_envs = std.ArrayList(*Env).init(allocator),
        };
    }

    /// Update stats, which counts towards running the garbage collector
    pub fn inc(self: *GC) void {
        self.allocations += 1;
    }

    pub fn runIfNeeded(self: *GC) !void {
        if (self.allocations % 100_000 == 0) {
            try self.run(false);
        }
    }

    /// Do a full sweep regardless of reachability
    pub fn deinit(self: *GC) void {
        var marked = std.AutoArrayHashMap(*Expr, void).init(allocator);
        defer marked.deinit();
        self.sweep(&marked, false) catch unreachable;
        for (self.registered_envs.items) |env| {
            env.deinit();
            allocator.destroy(env);
        }
        self.registered_envs.deinit();
        self.registered_expr.deinit();
        interned_intrinsics.deinit(allocator);

        for (interned_syms.items()) |entry| {
            allocator.free(entry.key);
        }
        interned_syms.deinit(allocator);

        var it = interned_nums.iterator();
        while (it.next()) |entry| {
            allocator.destroy(entry.value);
        }
        interned_nums.deinit(allocator);
    }

    /// Run mark and sweep over expressions and environments
    pub fn run(self: *GC, print_stats: bool) !void {
        const sweep_count = try self.sweepEnvironments();
        if (print_stats) {
            try std.io.getStdOut().writer().print("Garbage collected {d} environments\n", .{sweep_count});
        }
        var marked = std.AutoArrayHashMap(*Expr, void).init(allocator);
        defer marked.deinit();

        // Start marking reachable objects
        for (self.registered_envs.items) |e| {
            for (e.map.items()) |entry| {
                try self.mark(entry.key, &marked);
                try self.mark(entry.value, &marked);
            }
        }

        // Sweep unreachable objects
        try self.sweep(&marked, print_stats);
    }

    /// Recursively mark environments
    fn markEnvironment(self: *GC, env: *Env, marked: *std.AutoArrayHashMap(*Env, void)) anyerror!void {
        if (!marked.contains(env)) {
            try marked.put(env, {});
            for (env.map.items()) |env_entry| {
                if (env_entry.value.val == ExprType.env) {
                    try self.markEnvironment(env_entry.value.val.env, marked);
                } else if (env_entry.value.env) |actual_env| {
                    try self.markEnvironment(actual_env, marked);
                }
            }
            if (env.parent) |parent_env| {
                try self.markEnvironment(parent_env, marked);
            }
        }
    }

    /// Sweep unmarked environments
    fn sweepEnvironments(self: *GC) !usize {
        var sweep_count: usize = 0;
        if (self.registered_envs.items.len > 1) {
            var root_env = self.registered_envs.items[0];

            var marked = std.AutoArrayHashMap(*Env, void).init(allocator);
            defer marked.deinit();
            try self.markEnvironment(root_env, &marked);

            var idx: usize = 0;
            while (idx < self.registered_envs.items.len) {
                const current_env = self.registered_envs.items[idx];
                if (!marked.contains(current_env)) {
                    _ = self.registered_envs.swapRemove(idx);
                    current_env.deinit();
                    allocator.destroy(current_env);
                    sweep_count += 1;
                } else {
                    idx += 1;
                }
            }
        }
        return sweep_count;
    }

    /// Recursively mark expressions
    fn mark(self: *GC, expr: *Expr, marked: *std.AutoArrayHashMap(*Expr, void)) anyerror!void {
        if (!marked.contains(expr)) {
            try marked.put(expr, {});
            if (expr.val == ExprType.err) {
                try self.mark(expr.val.err, marked);
            } else if (expr.val == ExprType.lst) {
                for (expr.val.lst.items) |item| {
                    try self.mark(item, marked);
                }
            } else if (expr.val == ExprType.lam) {
                for (expr.val.lam.items) |item| {
                    try self.mark(item, marked);
                }
            } else if (expr.val == ExprType.mac) {
                for (expr.val.mac.items) |item| {
                    try self.mark(item, marked);
                }
            }
        }
    }

    /// Sweep unmarked expressions
    fn sweep(self: *GC, marked: *std.AutoArrayHashMap(*Expr, void), print_stats: bool) !void {
        var collected: usize = 0;
        var idx: usize = 0;
        while (idx < self.registered_expr.items.len) {
            const expr = self.registered_expr.items[idx];
            if (!marked.contains(expr)) {
                _ = self.registered_expr.swapRemove(idx);
                expr.deinit();
                allocator.destroy(expr);
                collected += 1;
            } else {
                idx += 1;
            }
        }

        if (print_stats) {
            try std.io.getStdOut().writer().print("Garbage collected {d} items, there's been {d} total allocations\n", .{ collected, self.allocations });
        }
    }
};

pub var gc: GC = undefined;
