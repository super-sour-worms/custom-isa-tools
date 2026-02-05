pub fn main() !u8 {
    defer _ = debug_allocator.deinit();
    const parsed_args = try args.parseWithVerbForCurrentProcess(struct {}, union(enum) {
        assemble: struct {
            output: []const u8 = "a.bin",
            pub const shorthands = .{ .o = "output" };
        },
        execute: struct {},
    }, alloc, .silent);
    defer parsed_args.deinit();
    if (parsed_args.positionals.len < 1) {
        log.err("excepted file to assemble as an positional argument\n", .{});
        return 1;
    }

    var source_file = try std.fs.cwd().openFile(parsed_args.positionals[0], .{});
    defer source_file.close();
    var reader = source_file.reader(&[_]u8{});
    const assembler_source = try reader.interface.allocRemaining(alloc, .unlimited);

    defer alloc.free(assembler_source);
    var binary = try assembler.assemble(alloc, assembler_source, parsed_args.positionals[0]);
    defer binary.deinit(alloc);
    switch (parsed_args.verb.?) {
        .assemble => |verb| {
            var file = try std.fs.cwd().createFile(verb.output, .{});
            defer file.close();
            var writer = file.writer(&[_]u8{});
            try writer.interface.writeAll(binary.items);
        },
        .execute => {
            var core: emulator.CpuCore = .{};
            @memcpy(core.memory[0..binary.items.len], binary.items);
            core.memory[binary.items.len + 3] = 0xf8;
            try core.run(alloc);
            log.info("Registers 1-7 dump:", .{});
            for (core.regs[1..8], 1..) |reg, i| {
                log.info("    ({x:08})<r{d}>", .{ reg, i });
            }
            var mem_dump: std.Io.Writer.Allocating = try .initCapacity(alloc, 3 * 48);
            defer mem_dump.deinit();
            for (core.memory[0..32]) |byte| {
                try mem_dump.writer.print("{x:02} ", .{byte});
            }
            log.info("Memory ..32 dump: {s}", .{mem_dump.written()});
        },
    }
    return 0;
}

const std = @import("std");
const log = std.log;

const args = @import("args");

const emulator = @import("emulator");
const assembler = @import("assembler");

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
const alloc = debug_allocator.allocator();
