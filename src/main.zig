const std = @import("std");
const ast = @import("ast.zig");
const gc = @import("boehm.zig");
const Interpreter = @import("interpreter.zig").Interpreter;
const intrinsics = @import("intrinsics.zig");

/// Interpreter driver program
pub fn main() !void {
    var interpreter = try Interpreter.init();
    defer interpreter.deinit();

    var allocator = gc.allocator();

    // Load the standard library
    {
        var args = std.ArrayList(*ast.Expr).init(allocator);
        defer args.deinit();

        // Use current directory for standard library path
        const cwd_absolute = try std.fs.cwd().realpathAlloc(allocator, ".");
        const stdpath = try std.fs.path.join(allocator, &[_][]const u8{ cwd_absolute, "std.lisp" });

        var stdlib = try ast.makeListExpr(&.{ &intrinsics.expr_atom_quote, try ast.makeAtomByDuplicating(stdpath) });
        try args.append(stdlib);
        _ = try intrinsics.stdImport(&interpreter, interpreter.env, args.items);
    }

    // Check if "run <file>" was passed
    {
        const process_args = try std.process.argsAlloc(allocator);
        if (process_args.len > 1) {
            if (std.mem.eql(u8, process_args[1], "run") and process_args.len > 2) {
                const load_expr = try std.fmt.allocPrint(allocator, "(import \"{s}\")", .{process_args[2]});
                _ = try interpreter.eval(interpreter.env, try interpreter.parse(load_expr));

                // TODO: reinstate
                //try interpreter.vm.run();
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
    try std.testing.expectEqual(interpreter.exit_code, null);
}
