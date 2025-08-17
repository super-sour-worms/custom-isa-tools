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
        InvalidInstruction,
        InvalidMemoryAccess,
    };

    pub fn executeNextInstruction(self: *CpuCore) !void {
        const pc = self.getRegister(.pc);
        const raw_instr = std.mem.bytesToValue(u32, self.memory[pc .. pc + 4]);
        const instr: Instruction = @bitCast(raw_instr);
        // std.debug.print("instr: {x}\n", .{instr});
        switch (instr.type) {
            .alu => self.executeAluInstruction(instr.payload.s_format),
            .mem => try self.executeMemInstruction(instr.payload.s_format),
            else => return CpuError.UnimplementedInstruction,
        }
        self.setRegister(.pc, self.getRegister(.pc) +% 4);
    }

    fn executeAluInstruction(self: *CpuCore, instr: Instruction.SFormat) void {
        const op1, const op2 = self.getSFormatOps(instr);
        std.debug.print("Alu instruction: op1={}, op2={}, dest={}\n", .{ op1, op2, instr.target_reg });
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

    fn executeMemInstruction(self: *CpuCore, instr: Instruction.SFormat) !void {
        const op1, const op2 = self.getSFormatOps(instr);
        const addr = op1 + op2;
        const opcode: packed struct(u3) {
            is_store: bool,
            size: enum(u2) { byte = 0, half = 1, word = 2, _ },
        } = @bitCast(instr.opcode);
        if (opcode.is_store) {
            std.debug.print("Store {s} instruction: source={}, base={}, offset={}\n", .{ @tagName(opcode.size), instr.target_reg, op1, op2 });
            const val = self.getRegister(@enumFromInt(instr.target_reg));
            switch (opcode.size) {
                .byte => self.memory[addr] = @intCast(val),
                .half => std.mem.writeInt(u16, @ptrCast(&self.memory[addr]), @intCast(val), std.builtin.Endian.little),
                .word => std.mem.writeInt(u32, @ptrCast(&self.memory[addr]), val, std.builtin.Endian.little),
                else => return CpuError.InvalidInstruction,
            }
        } else {
            std.debug.print("Load {s} instruction: base={}, offset={}, dest={}\n", .{ @tagName(opcode.size), op1, op2, instr.target_reg });
            const val = switch (opcode.size) {
                .byte => self.memory[addr],
                .half => std.mem.readInt(u16, @ptrCast(&self.memory[addr]), std.builtin.Endian.little),
                .word => std.mem.readInt(u32, @ptrCast(&self.memory[addr]), std.builtin.Endian.little),
                else => return CpuError.InvalidInstruction,
            };
            self.setRegister(@enumFromInt(instr.target_reg), val);
        }
    }

    fn getSFormatOps(self: *CpuCore, instr: Instruction.SFormat) struct { u32, u32 } {
        const op1 = self.getRegister(@enumFromInt(instr.primary_operand_reg));
        if (instr.shift_source_is_register) {
            const shift = instr.secondary_operand.reg;
            const value_to_shift = self.getRegister(@enumFromInt(shift.value_to_shift_reg));
            const shift_by = if (shift.is_shifted_by_reg)
                self.getRegister(@enumFromInt(shift.shift_by))
            else
                shift.shift_by;
            return .{ op1, switch (shift.shift_type) {
                .lsl => std.math.shl(u32, value_to_shift, shift_by),
                .lsr => std.math.shr(u32, value_to_shift, shift_by),
                .asr => @as(u32, @intCast(std.math.shr(i32, @intCast(value_to_shift), shift_by))),
                .ror => std.math.rotr(u32, value_to_shift, shift_by),
            } };
        } else {
            const shift = instr.secondary_operand.imm;
            return .{ op1, std.math.rotr(u32, shift.value_to_shift, shift.shift_by) };
        }
    }

    fn getRegister(self: *CpuCore, reg: RegIndex) u32 {
        return if (reg == .zr) 0 else self.global_regs[@intFromEnum(reg)];
    }

    fn setRegister(self: *CpuCore, reg: RegIndex, val: u32) void {
        if (reg == .zr) return;
        self.global_regs[@intFromEnum(reg)] = val;
    }
};
