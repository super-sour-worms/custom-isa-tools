pub fn generate(
    allocator: std.mem.Allocator,
    ast_nodes: []const ast.AstRootNode,
    complexes: []ast.Complex,
) !std.ArrayList(u8) {
    var labels, const len = try collectLabelsAndGetLen(allocator, ast_nodes);
    defer labels.deinit();
    var binary: std.ArrayList(u8) = try .initCapacity(allocator, len);
    errdefer binary.deinit(allocator);

    var addr: u32 = 0;
    const complexes_context: ast.ComplexesContext = .{
        .complexes = complexes,
        .labels = &labels,
        .current_address = &addr,
    };
    for (ast_nodes) |root_node| {
        addr = @intCast(binary.items.len);
        switch (root_node) {
            .alu_instruction => |alu_instruction| {
                const opcode = @intFromEnum(alu_instruction.mnemonic.toBinaryField());
                const bytes: [4]u8 = @bitCast(Instruction{ .type = .alu, .payload = .{
                    .s_or_m_format = try alu_instruction.s_format.toInstructionField(opcode, complexes_context),
                } });
                try binary.appendSlice(allocator, &bytes);
            },

            .mem_instruction => |mem_instruction| {
                const opcode = @intFromEnum(mem_instruction.mnemonic.toBinaryField());
                const bytes: [4]u8 = @bitCast(Instruction{ .type = .mem, .payload = .{
                    .s_or_m_format = try mem_instruction.m_format.toInstructionField(opcode, complexes_context),
                } });
                try binary.appendSlice(allocator, &bytes);
            },

            .binary_directive => {
                return error.Unimplemented;
            },

            .align_directive => {
                return error.Unimplemented;
            },

            .label_definition => {}, // ignore
            // binary.appendSlice(allocator, []);
        }
    }
    return binary;
}

fn collectLabelsAndGetLen(allocator: std.mem.Allocator, ast_nodes: []const ast.AstRootNode) !struct { std.StringHashMap(u32), u32 } {
    var labels: std.StringHashMap(u32) = .init(allocator);
    var address: u32 = 0;
    for (ast_nodes) |node| switch (node) {
        .alu_instruction, .mem_instruction => {
            address += 4;
        },
        .binary_directive => |directive| {
            const repeat_times: usize = if (directive.repeat_times) |times| @intCast(times) else 1;
            const values_size = switch (directive.values) {
                .u8 => |v| v.items.len,
                .i8 => |v| v.items.len,
                .u16 => |v| v.items.len * 2,
                .i16 => |v| v.items.len * 2,
                .u32 => |v| v.items.len * 4,
                .i32 => |v| v.items.len * 4,
            };
            address += @intCast(values_size * repeat_times);
        },
        .align_directive => |directive| {
            const alignment: u32 = @intCast(directive.alignment);
            address += alignment - (address % alignment);
        },
        .label_definition => |label_definition| {
            if (try labels.fetchPut(label_definition, address) != null) {
                return McdError.LabelRedifinition;
            }
        },
    };
    return .{ labels, address };
}

const McdError = error{LabelRedifinition};

const std = @import("std");

const instructions = @import("instructions");
const Instruction = instructions.Instruction;

const lexer = @import("lexer.zig");
const Token = lexer.TypedToken;

const parser = @import("parser.zig");

const ast = @import("ast.zig");
