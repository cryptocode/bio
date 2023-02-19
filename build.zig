const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "bio",
        .root_source_file = .{.path = "src/main.zig"},
        .target = target,
        .optimize = optimize,
    });

    exe.linkLibC();
    if (!target.isWindows()) {
        exe.addCSourceFile("deps/linenoise.c", &[_][]const u8{"-std=c99"});
        exe.addIncludePath("deps");
    }
    exe.setOutputDir(".");
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
