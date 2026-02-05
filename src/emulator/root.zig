pub const CpuCore = struct {
    regs: [32]u32 = [_]u32{0} ** 32,
    memory: [1024]u8 align(4) = undefined,
    pc_modified: bool = false,

    pub fn run(self: *CpuCore, alloc: Allocator) !void {
        while (true) {
            self.executeNextInstruction(alloc) catch |err| switch (err) {
                error.UnimplementedInstruction => return,
                else => return err,
            };
        }
    }

    pub const CpuError = error{
        InvalidInstruction,
        InvalidMemoryAccess,
    };

    pub fn executeNextInstruction(self: *CpuCore, alloc: Allocator) !void {
        const raw_instr = mem.bytesToValue(u32, self.memory[self.regs[1] .. self.regs[1] + 4]);
        self.pc_modified = false;
        const instr: Instruction = @bitCast(raw_instr);
        switch (instr.type) {
            .alu => try self.executeAluInstruction(alloc, instr.payload),
            .mem => try self.executeMemInstruction(instr.payload.s_or_m_format),
            .cjmp => try self.executeCJmpInstruction(instr.payload.cj_format),
            .ljmp => try self.executeLJmpInstruction(instr.payload.lj_format),
            else => return error.UnimplementedInstruction,
        }
        if (!self.pc_modified) {
            self.regs[1] +%= 4;
        }
    }

    fn executeAluInstruction(self: *CpuCore, alloc: Allocator, instr: Instruction.Payload) !void {
        const s: instructions.Instruction.SOrMFormat = @bitCast(instr);
        if (s.target == .zr) {
            const lm: instructions.Instruction.LMFormat = @bitCast(instr);
            const half: u32 = lm.imm_low | @as(u16, @intCast(lm.imm_high)) << 14;
            const new_value = if (lm.is_high_half)
                (self.getRegister(lm.target) & 0x0000_FFFF) | (half << 16)
            else
                (self.getRegister(lm.target) & 0xFFFF_0000) | half;
            self.setRegister(lm.target, new_value);
        } else {
            const op1 = self.getRegister(s.primary);
            const op2 = self.getSFormatSecondary(s);
            const result = switch (@as(instructions.AluOpcode, @enumFromInt(s.opcode))) {
                .add => op1 +% op2,
                .sub => op1 -% op2,
                .xor => op1 ^ op2,
                .brs => op1 & ~op2,
                .@"and" => op1 & op2,
                .nand => ~(op1 & op2),
                .nor => ~(op1 | op2),
                .@"or" => op1 | op2,
            };

            // TODO: make instruction info formating easier
            const dumpLineBegnning = try std.fmt.allocPrint(alloc, "{t} <{t}>({d}), ({d})<{t}>, ({d})", .{
                @as(instructions.AluOpcode, @enumFromInt(s.opcode)),
                s.target,
                result,
                op1,
                s.primary,
                op2,
            });
            defer alloc.free(dumpLineBegnning);
            if (s.secondary_is_register) {
                const reg = s.secondary.reg;
                if (reg.is_shifted_by_reg)
                    log.info("{s}<{t} {t} {t}>", .{
                        dumpLineBegnning,
                        reg.value_to_shift,
                        reg.shift_type,
                        reg.shift_by.reg,
                    })
                else
                    log.info("{s}<{t} {t} {d}>", .{
                        dumpLineBegnning,
                        reg.value_to_shift,
                        reg.shift_type,
                        reg.shift_by.imm,
                    });
            } else {
                const imm = s.secondary.s_format_imm;
                log.info("{s}<{d} rol {d}>", .{
                    dumpLineBegnning,
                    imm.value_to_shift,
                    imm.shift_by,
                });
            }

            self.setRegister(s.target, result);
        }
    }

    fn executeMemInstruction(self: *CpuCore, instr: Instruction.SOrMFormat) !void {
        const addr = self.getRegister(instr.primary) +% self.getMFormatSecondary(instr);
        const opcode: struct {
            is_store: bool,
            size: enum(u2) { @"1", @"2", @"4" },
        } = switch (@as(instructions.MemOpcode, @enumFromInt(instr.opcode))) {
            .str1 => .{ .is_store = true, .size = .@"1" },
            .lod1 => .{ .is_store = false, .size = .@"1" },
            .str2 => .{ .is_store = true, .size = .@"2" },
            .lod2 => .{ .is_store = false, .size = .@"2" },
            .str4 => .{ .is_store = true, .size = .@"4" },
            .lod4 => .{ .is_store = false, .size = .@"4" },
            else => return error.Unimplemented,
        };
        // if (opcode.is_store) {
        //     log.info("Store: addr={}, target={}", .{ addr, instr.target });
        // } else {
        //     log.info("Load: addr={}, target={}", .{ addr, instr.target });
        // }
        if (opcode.is_store) {
            const val = self.getRegister(instr.target);
            log.info("str{t}: ({d})<{t}>, [({x})]", .{ opcode.size, val, instr.target, addr });
            switch (opcode.size) {
                .@"1" => self.memory[addr] = @intCast(val),
                .@"2" => mem.writeInt(u16, @ptrCast(&self.memory[addr]), @intCast(val), .little),
                .@"4" => mem.writeInt(u32, @ptrCast(&self.memory[addr]), val, .little),
            }
        } else {
            const val = switch (opcode.size) {
                .@"1" => self.memory[addr],
                .@"2" => mem.readInt(u16, @ptrCast(&self.memory[addr]), .little),
                .@"4" => mem.readInt(u32, @ptrCast(&self.memory[addr]), .little),
            };
            log.info("lod{t}: <{t}>({d}), [({x})]", .{ opcode.size, instr.target, val, addr });
            self.setRegister(instr.target, val);
        }
    }

    fn executeCJmpInstruction(self: *CpuCore, instr: Instruction.CJFormat) !void {
        const op1 = self.getRegister(instr.comparand1);
        const op2 = self.getRegister(instr.comparand2);
        if (try compareTwoComparands(instr.condition, op1, op2)) {
            const addr: i16 = @as(i16, @intCast(instr.pc_offset)) << 2;
            self.regs[1] +%= @as(u32, @intCast(addr));
            self.pc_modified = true;
        }
    }

    fn executeLJmpInstruction(self: *CpuCore, instr: Instruction.LJFormat) !void {
        self.setRegister(instr.link_reg, self.getRegister(.pc) + 4);
        self.regs[1] = self.getRegister(instr.base_reg) +% @as(u32, @intCast(instr.offset));
        self.pc_modified = true;
    }

    fn compareTwoComparands(condition: instructions.Condition, a: u32, b: u32) !bool {
        return switch (condition) {
            .eq => a == b,
            .neq => a != b,
            .sles => @as(i32, @intCast(a)) < @as(i32, @intCast(b)),
            .sleq => @as(i32, @intCast(a)) <= @as(i32, @intCast(b)),
            .ules => a < b,
            .uleq => a <= b,
            else => return CpuError.InvalidInstruction,
        };
    }

    fn getSFormatSecondary(self: *CpuCore, instr: Instruction.SOrMFormat) u32 {
        if (instr.secondary_is_register) {
            const shift = instr.secondary.reg;
            const value_to_shift = self.getRegister(shift.value_to_shift);
            const shift_by = if (shift.is_shifted_by_reg)
                self.getRegister(shift.shift_by.reg)
            else
                shift.shift_by.imm;
            return switch (shift.shift_type) {
                .lsl => math.shl(u32, value_to_shift, shift_by),
                .lsr => math.shr(u32, value_to_shift, shift_by),
                .asr => @as(u32, @intCast(math.shr(i32, @intCast(value_to_shift), shift_by))),
                .rol => math.rotl(u32, value_to_shift, shift_by),
            };
        } else {
            const shift = instr.secondary.s_format_imm;
            return math.rotl(u32, shift.value_to_shift, shift.shift_by);
        }
    }

    fn getMFormatSecondary(self: *CpuCore, instr: Instruction.SOrMFormat) u32 {
        if (instr.secondary_is_register) {
            const shift = instr.secondary.reg;
            const value_to_shift = self.getRegister(shift.value_to_shift);
            const shift_by = if (shift.is_shifted_by_reg)
                self.getRegister(shift.shift_by.reg)
            else
                shift.shift_by.imm;
            return switch (shift.shift_type) {
                .lsl => math.shl(u32, value_to_shift, shift_by),
                .lsr => math.shr(u32, value_to_shift, shift_by),
                .asr => @as(u32, @intCast(math.shr(i32, @intCast(value_to_shift), shift_by))),
                .rol => math.rotl(u32, value_to_shift, shift_by),
            };
        } else return @bitCast(@as(i32, @intCast(instr.secondary.m_format_imm)));
    }

    fn getRegister(self: *CpuCore, reg: instructions.Register) u32 {
        return if (reg == .zr) 0 else self.regs[@intFromEnum(reg)];
    }

    fn setRegister(self: *CpuCore, reg: instructions.Register, val: u32) void {
        if (reg == .zr or reg == .pc) return;
        self.regs[@intFromEnum(reg)] = val;
    }
};

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = std.builtin;
const mem = std.mem;
const math = std.math;
const log = std.log;

const instructions = @import("instructions");
const Instruction = instructions.Instruction;
