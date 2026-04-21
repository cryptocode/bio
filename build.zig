const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Boehm GC
    const gc_module = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    const gc = b.addLibrary(.{
        .name = "gc",
        .linkage = .static,
        .root_module = gc_module,
    });
    {
        const cflags = [_][]const u8{"-DNO_EXECUTE_PERMISSION"};
        const libgc_srcs = [_][]const u8{
            "alloc.c",    "reclaim.c", "allchblk.c", "misc.c",     "mach_dep.c", "os_dep.c",
            "mark_rts.c", "headers.c", "mark.c",     "obj_map.c",  "blacklst.c", "finalize.c",
            "new_hblk.c", "dbg_mlc.c", "malloc.c",   "dyn_load.c", "typd_mlc.c", "ptr_chck.c",
            "mallocx.c",
        };

        if (target.result.os.tag.isDarwin()) {
            gc.root_module.linkFramework("CoreServices", .{});
        }
        gc.root_module.addIncludePath(b.path("deps/github.com/ivmai/bdwgc/include"));
        inline for (libgc_srcs) |src| {
            gc.root_module.addCSourceFile(.{ .file = b.path("deps/github.com/ivmai/bdwgc/" ++ src), .flags = &cflags });
        }

        const gc_step = b.step("libgc", "build libgc");
        gc_step.dependOn(&gc.step);
    }

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    const exe = b.addExecutable(.{
        .name = "bio",
        .root_module = exe_module,
    });
    exe.root_module.addIncludePath(b.path("deps/github.com/ivmai/bdwgc/include"));
    exe.root_module.linkLibrary(gc);

    if (target.result.os.tag != .windows) {
        exe.root_module.addCSourceFile(.{ .file = b.path("deps/linenoise.c"), .flags = &[_][]const u8{ "-std=c99", "-Wno-everything" } });
        exe.root_module.addIncludePath(b.path("deps"));
    }

    const inst = b.addInstallArtifact(exe, .{});
    b.getInstallStep().dependOn(&inst.step);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_module = b.createModule(.{
        .root_source_file = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    const tests = b.addTest(.{
        .root_module = test_module,
    });
    tests.root_module.addIncludePath(b.path("deps/github.com/ivmai/bdwgc/include"));
    tests.root_module.linkLibrary(gc);

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);
}
