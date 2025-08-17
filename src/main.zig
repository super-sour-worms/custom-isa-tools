pub fn main() !u8 {
    defer _ = debug_allocator.deinit();

    const parsed_args = try args.parseWithVerbForCurrentProcess(struct {}, union(enum) {
        assemble: struct {
            output: []const u8 = "a.bin",
            pub const shorthands = .{ .o = "output" };
        },
        execute: struct {},
    }, allocator, .silent);
    defer parsed_args.deinit();
    if (parsed_args.positionals.len < 1) {
        std.debug.print("Error: excepted file to assemble as an positional argument\n", .{});
        return 1;
    }
    const assembler_source = try readFile(parsed_args.positionals[0]);
    defer allocator.free(assembler_source);
    const binary = try assembler.assemble(assembler_source);
    switch (parsed_args.verb.?) {
        .assemble => |verb| {
            try writeFile(verb.output, binary);
        },
        .execute => {
            var core: emulator.CpuCore = .init();
            @memcpy(core.memory[0..binary.len], binary);
            core.memory[binary.len + 3] = 248;
            core.run() catch |err| {
                std.debug.print("Error: {!}\n", .{err});
            };
            try stdout.print("reg dump: {x:08}\n", .{core.global_regs});
            try stdout.print("mem dump: {x:02}\n", .{core.memory});
        },
    }
    return 0;
}

fn readFile(path: []const u8) ![]const u8 {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const file_size = (try file.metadata()).size();
    const source_buffer = try allocator.alloc(u8, file_size);
    _ = try file.readAll(source_buffer);
    return source_buffer;
}

fn writeFile(path: []const u8, data: []const u8) !void {
    var file: std.fs.File = try std.fs.cwd().createFile(path, .{});
    defer file.close();
    try file.writeAll(data);
}

const std = @import("std");
const emulator = @import("emulator");
const assembler = @import("assembler");
const args = @import("args");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
const allocator = debug_allocator.allocator();
