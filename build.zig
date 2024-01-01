const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const opts = b.addOptions();
    opts.addOption(bool, "tracing", b.option(bool, "tracing", "enable compiler operation tracing for better debugging") orelse false);

    const common = b.addModule("common", .{
        .source_file = .{ .path = "src/common/common.zig" },
        .dependencies = &.{},
    });

    const mod = b.addModule("fsm-compiler", .{
        .source_file = .{ .path = "src/compiler/compiler.zig" },
        .dependencies = &.{
            .{ .name = "build_options", .module = opts.createModule() },
            .{ .name = "common", .module = common },
        },
    });

    const lib = b.addStaticLibrary(.{
        .name = "rlscan",
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    lib.installHeadersDirectory("src/compiler/todo-replace", "");
    lib.addCSourceFile(.{ .file = .{ .path = "src/compiler/todo-replace/rlscan.c" }, .flags = &.{ "-std=c99" } });

    {
        const frontend_exe = b.addExecutable(.{
            .name = "run",
            .root_source_file = .{ .path = "src/frontend/main.zig" },
            .target = target,
            .optimize = optimize,
        });
        frontend_exe.linkLibrary(lib);
        frontend_exe.addModule("common", common);
        frontend_exe.addModule("fsm-compiler", mod);
        const frontend_run = b.addRunArtifact(frontend_exe);
        if (b.args) |args| frontend_run.addArgs(args);
        const run = b.step("run", "Run");
        run.dependOn(&frontend_run.step);
    }

    {
        const run_test = b.step("test", "Run unit tests");
        for ([_][]const u8{
            "src/common/common.zig",
            "src/compiler/compiler.zig",
        }) |src| {
            const tst = b.addTest(.{
                .root_source_file = .{ .path = src },
                .target = target, .optimize = optimize
            });
            tst.linkLibrary(lib);
            const run = b.addRunArtifact(tst);
            run_test.dependOn(&run.step);
        }
    }
}
