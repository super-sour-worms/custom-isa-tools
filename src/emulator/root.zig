const std = @import("std");
const builtin = std.builtin;
const instructions = @import("instructions");
const Instruction = instructions.Instruction;

pub const CpuCore = struct {
    global_regs: [8]u32 = [_]u32{0} ** 8,
    memory: [64]u8 align(4) = undefined,

    const RegIndex = enum(u5) {
        zr = 0, // Zero register
        pc = 1, // Program counter
        sp = 2, // Stack pointer
        bp = 3, // Base pointer
        _,
    };

    pub fn init() CpuCore {
        return .{};
    }

    pub fn run(self: *CpuCore) !void {
        while (true) {
            try self.executeNextInstruction();
        }
    }

    pub const CpuError = error{
        UnimplementedInstruction,
        InvalidMemoryAccess,
    };

    pub fn executeNextInstruction(self: *CpuCore) !void {
        const pc = self.getRegister(.pc);
        const raw_instr = std.mem.bytesToValue(u32, self.memory[pc .. pc + 4]);
        const instr: Instruction = @bitCast(raw_instr);
        // std.debug.print("Executing instruction: {any}\n", .{instr.type});
        switch (instr.type) {
            .alu => self.executeAluInstruction(instr.payload.s_format),
            else => return CpuError.UnimplementedInstruction,
        }
        self.setRegister(.pc, self.getRegister(.pc) +% 4);
    }

    fn executeAluInstruction(self: *CpuCore, instr: Instruction.SFormat) void {
        const op1 = self.getRegister(@enumFromInt(instr.primary_operand_reg));
        const op2 = if (instr.shift_source_is_register) blk: {
            const shift = instr.secondary_operand.reg;
            const value_to_shift = self.getRegister(@enumFromInt(shift.value_to_shift_reg));
            const shift_by = if (shift.is_shifted_by_reg)
                self.getRegister(@enumFromInt(shift.shift_by))
            else
                shift.shift_by;
            break :blk switch (shift.shift_type) {
                .lsl => std.math.shl(u32, value_to_shift, shift_by),
                .lsr => std.math.shr(u32, value_to_shift, shift_by),
                .asr => @as(u32, @intCast(std.math.shr(i32, @intCast(value_to_shift), shift_by))),
                .ror => std.math.rotr(u32, value_to_shift, shift_by),
            };
        } else blk: {
            const shift = instr.secondary_operand.imm;
            break :blk std.math.rotr(u32, shift.value_to_shift, shift.shift_by);
        };
        const result = switch (@as(instructions.AluOpcode, @enumFromInt(instr.opcode))) {
            .add => op1 +% op2,
            .sub => op1 -% op2,
            .xor => op1 ^ op2,
            .brs => op1 & ~op2,
            .@"and" => op1 & op2,
            .nand => ~(op1 & op2),
            .nor => ~(op1 | op2),
            .@"or" => op1 | op2,
        };
        self.setRegister(@enumFromInt(instr.target_reg), result);
    }

    fn getRegister(self: *CpuCore, reg: RegIndex) u32 {
        return if (reg == .zr) 0 else self.global_regs[@intFromEnum(reg)];
    }

    fn setRegister(self: *CpuCore, reg: RegIndex, val: u32) void {
        if (reg == .zr) return;
        self.global_regs[@intFromEnum(reg)] = val;
    }
};
