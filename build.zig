const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("bio", "src/main.zig");
    exe.linkLibC();
    if (!target.isWindows()) {
        exe.addCSourceFile("deps/linenoise.c", &[_][]const u8{"-std=c99"});
        exe.addIncludeDir("deps");
    }
    exe.setTarget(target);
    exe.setBuildMode(mode);
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
