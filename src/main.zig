const std = @import("std");
usingnamespace @import("ast.zig");
usingnamespace @import("gc.zig");
usingnamespace @import("interpreter.zig");
usingnamespace @import("intrinsics.zig");

/// Interpreter driver program
pub fn main() !void {
    var interpreter = try Interpreter.init();
    defer interpreter.deinit();

    // Load the standard library
    {
        var args = std.ArrayList(*Expr).init(allocator);
        defer args.deinit();
        
        // Use path of bio binary for standard library path
        const pwd = try std.fs.selfExeDirPathAlloc(allocator);
        defer allocator.free(pwd);
        const stdpath = try std.fs.path.join(allocator, &[_][]const u8{ pwd, "std.lisp" });
        defer allocator.free(stdpath);
        
        var stdlib = try makeListExpr(&.{ &expr_atom_quote, try makeAtomByDuplicating(stdpath) });
        try args.append(stdlib);
        _ = try stdImport(&interpreter, interpreter.env, args.items);
    }

    // Check if "run <file>" was passed
    {
        const process_args = try std.process.argsAlloc(allocator);
        defer std.process.argsFree(allocator, process_args);
        if (process_args.len > 1) {
            if (std.mem.eql(u8, process_args[1], "run") and process_args.len > 2) {
                const load_expr = try std.fmt.allocPrint(allocator, "(import \"{s}\")", .{process_args[2]});
                defer allocator.free(load_expr);
                _ = try interpreter.eval(interpreter.env, try interpreter.parse(load_expr));
                return;
            }
        }
    }

    // Start REPL
    interpreter.readEvalPrint() catch |err| {
        try interpreter.printError(err);
    };
}

// This invokes a test suite written in Bio. The tests will assert on error, which is why we check the exit code.
test "Evaluate standard library and test.lisp" {
    var interpreter = try Interpreter.init();
    defer interpreter.deinit();
    _ = try interpreter.eval(interpreter.env, try interpreter.parse("(begin (import \"std.lisp\") (import \"test.lisp\"))"));
    std.testing.expectEqual(interpreter.exit_code, null);
}

// Leak detection during tests
test "Finalize" {
    _ = gpa.deinit();
}
