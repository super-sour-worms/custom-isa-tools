const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Modules

    const instructions_mod = b.createModule(.{
        .root_source_file = b.path("src/instructions/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const assembler_mod = b.createModule(.{
        .root_source_file = b.path("src/assembler/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    assembler_mod.addImport("instructions", instructions_mod);

    const emulator_mod = b.createModule(.{
        .root_source_file = b.path("src/emulator/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    emulator_mod.addImport("instructions", instructions_mod);

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_mod.addImport("emulator", emulator_mod);
    exe_mod.addImport("assembler", assembler_mod);
    exe_mod.addImport("args", b.dependency("args", .{ .target = target, .optimize = optimize }).module("args"));

    // Compilation

    const exe = b.addExecutable(.{
        .name = "isa-tools",
        .root_module = exe_mod,
    });
    b.installArtifact(exe);

    // Running

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Tests

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(testsForModule(b, instructions_mod));
    test_step.dependOn(testsForModule(b, assembler_mod));
    test_step.dependOn(testsForModule(b, emulator_mod));
    test_step.dependOn(testsForModule(b, exe_mod));
}

fn testsForModule(b: *std.Build, m: *std.Build.Module) *std.Build.Step {
    const unit_tests = b.addTest(.{
        .root_module = m,
    });
    return &b.addRunArtifact(unit_tests).step;
}
