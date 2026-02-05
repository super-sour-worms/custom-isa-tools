pub const AstRootNode = union(enum) {
    alu_instruction: AluInstruction,
    mem_instruction: MemInstruction,
    binary_directive: BinaryDirective,
    align_directive: AlignDirective,
    label_definition: []const u8,

    pub const AluInstruction = struct {
        mnemonic: Token.Kind.AluInstruction,
        s_format: SFormat,

        pub fn format(self: AluInstruction, writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("Mnem={t}, Targ={t}, Prim={t}, Sec={f}", .{
                self.mnemonic,
                self.s_format.target,
                self.s_format.primary,
                self.s_format.secondary,
            });
            if (self.s_format.shift) |shift| {
                try writer.print(", ShiftType={t}, ShiftBy={f}", .{ shift.shift_type, shift.shift_by });
            }
        }
    };

    pub const MemInstruction = struct {
        mnemonic: Token.Kind.MemInstruction,
        m_format: MFormat,

        pub fn format(self: MemInstruction, writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("Mnem={t}, Targ={t}, ", .{ self.mnemonic, self.m_format.target });
            switch (self.m_format.address) {
                .general_form => |addr| {
                    try writer.print("Base={t}, Offset={f}", .{ addr.base, addr.offset });
                    if (addr.offset_shift) |shift| {
                        try writer.print(", ShiftType={t}, ShiftBy={f}", .{ shift.shift_type, shift.shift_by });
                    }
                    try writer.print("", .{});
                },
                .label_form => |label| {
                    try writer.print("Addr=@'{s}'", .{label});
                },
            }
        }
    };

    pub const BinaryDirective = struct {
        data_type: Token.Kind.DataType,
        repeat_times: ?i64,
        values: union(Token.Kind.DataType) {
            u8: ArrayList(u8),
            i8: ArrayList(i8),
            u16: ArrayList(u16),
            i16: ArrayList(i16),
            u32: ArrayList(u32),
            i32: ArrayList(i32),
        },
    };

    pub const AlignDirective = struct {
        alignment: usize,
        data_type: Token.Kind.DataType,
        values: union(Token.Kind.DataType) {
            u8: u8,
            i8: i8,
            u16: u16,
            i16: i16,
            u32: u32,
            i32: i32,
        },
    };

    pub const SFormat = struct {
        target: Token.Kind.Register,
        primary: Token.Kind.Register,
        secondary: ComplexOrRegister,
        shift: ?Shift,
        pub const Shift = struct {
            shift_type: Token.Kind.ShiftOperator,
            shift_by: ComplexOrRegister,
        };

        pub fn toInstructionField(
            self: @This(),
            opcode: u3,
            context: ComplexesContext,
        ) !Instruction.SOrMFormat {
            const shift_by_value: ComplexOrRegister = if (self.shift) |s|
                s.shift_by
            else
                .{ .complex = .{ .number = 0 } };
            return .{
                .opcode = opcode,
                .target = self.target.toBinaryField(),
                .primary = self.primary.toBinaryField(),
                .secondary_is_register = self.secondary == .register,
                .secondary = if (self.secondary == .register)
                    .{ .reg = .{
                        .value_to_shift = self.secondary.register.toBinaryField(),
                        .shift_type = if (self.shift) |shift| shift.shift_type.toBinaryField() else .lsl,
                        .is_shifted_by_reg = shift_by_value == .register,
                        .shift_by = switch (shift_by_value) {
                            .register => |r| .{ .reg = r.toBinaryField() },
                            .complex => |c| .{ .imm = @intCast(try c.evaluate(context)) },
                        },
                    } }
                else
                    .{ .s_format_imm = .{
                        .value_to_shift = @intCast(try self.secondary.complex.evaluate(context)),
                        .shift_by = @intCast(shift_by_value.complex.number),
                    } },
            };
        }
    };

    pub const MFormat = struct {
        target: Token.Kind.Register,
        address: Address,
        pub const Address = union(enum) {
            general_form: GeneralForm,
            label_form: []const u8,
            pub const GeneralForm = struct {
                base: Token.Kind.Register,
                offset: ComplexOrRegister,
                offset_shift: ?SFormat.Shift,
            };
        };
        pub fn toInstructionField(
            self: @This(),
            opcode: u3,
            context: ComplexesContext,
        ) !Instruction.SOrMFormat {
            switch (self.address) {
                .general_form => |form| {
                    const shift_by_value: ComplexOrRegister = if (form.offset_shift) |s|
                        s.shift_by
                    else
                        .{ .complex = .{ .number = 0 } };
                    std.debug.assert(form.offset == .register or form.offset_shift == null);
                    return .{
                        .opcode = opcode,
                        .target = self.target.toBinaryField(),
                        .primary = form.base.toBinaryField(),
                        .secondary_is_register = form.offset == .register,
                        .secondary = switch (form.offset) {
                            .register => |r| .{ .reg = .{
                                .value_to_shift = r.toBinaryField(),
                                .shift_type = if (form.offset_shift) |shift| shift.shift_type.toBinaryField() else .lsl,
                                .is_shifted_by_reg = shift_by_value == .register,
                                .shift_by = switch (shift_by_value) {
                                    .register => |rs| .{ .reg = rs.toBinaryField() },
                                    .complex => |cs| .{ .imm = @intCast(try cs.evaluate(context)) },
                                },
                            } },
                            .complex => |c| .{ .m_format_imm = @intCast(try c.evaluate(context)) },
                        },
                    };
                },
                .label_form => |label| {
                    const label_address: i32 = @intCast(try Complex.evaluate(.{ .label = label }, context));
                    const current_address: i32 = @intCast(context.current_address.*);
                    return .{
                        .opcode = opcode,
                        .target = self.target.toBinaryField(),
                        .primary = .pc,
                        .secondary_is_register = false,
                        .secondary = .{ .m_format_imm = @intCast(label_address - current_address) },
                    };
                },
            }
        }
    };

    pub fn format(self: AstRootNode, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .alu_instruction => |instr| {
                try writer.print("ALU instruction: {f}", .{instr});
            },
            .mem_instruction => |instr| {
                try writer.print("Memory instruction: {f}", .{instr});
            },
            .binary_directive => |dir| {
                try writer.print("Binary directive: {t} {?} ", .{ dir.data_type, dir.repeat_times });
                switch (dir.values) {
                    .u8 => |v| try writer.print("{any}", .{v.items}),
                    .i8 => |v| try writer.print("{any}", .{v.items}),
                    .u16 => |v| try writer.print("{any}", .{v.items}),
                    .i16 => |v| try writer.print("{any}", .{v.items}),
                    .u32 => |v| try writer.print("{any}", .{v.items}),
                    .i32 => |v| try writer.print("{any}", .{v.items}),
                }
            },
            .align_directive => |dir| {
                try writer.print("Align directive: {} {t} ", .{ dir.alignment, dir.data_type });
                switch (dir.values) {
                    .u8 => |v| try writer.print("{}", .{v}),
                    .i8 => |v| try writer.print("{}", .{v}),
                    .u16 => |v| try writer.print("{}", .{v}),
                    .i16 => |v| try writer.print("{}", .{v}),
                    .u32 => |v| try writer.print("{}", .{v}),
                    .i32 => |v| try writer.print("{}", .{v}),
                }
            },
            .label_definition => |label| {
                try writer.print("Label definition: '{s}'", .{label});
            },
        }
    }
};

pub const Complex = union(enum) {
    number: i64,
    label: []const u8,
    operation: Operation,
    pub const Operation = struct {
        operand1_index: usize,
        operator: Operator,
        operand2_index: usize,

        pub const Operator = enum {
            multiply, // 5: * / %
            divide,
            mod,

            plus, // 4: + -
            minus,

            lsl, // 3: << >> -> <>
            lsr,
            asr,
            rol,

            @"and", // 2: &

            xor, // 1: ^

            @"or", // 0: |

            pub const Precedence = enum(u3) { @"0" = 0, @"1" = 1, @"2" = 2, @"3" = 3, @"4" = 4, @"5" = 5 };

            pub fn fromTokenKind(k: Token.Kind) !Operator {
                return switch (k) {
                    .symbol => |symbol| switch (symbol) {
                        .@"*" => .multiply,
                        .@"/" => .divide,
                        .@"%" => .mod,
                        .@"+" => .plus,
                        .@"-" => .minus,
                        .@"&" => .@"and",
                        .@"^" => .xor,
                        .@"|" => .@"or",
                        else => {
                            log.err("expected math operator, found {any}\n", .{symbol});
                            return ParserError.UnexpectedToken;
                        },
                    },
                    .shift_operator => |shift| switch (shift) {
                        .@"<<" => .lsl,
                        .@">>" => .lsr,
                        .@"->" => .asr,
                        .@"<>" => .rol,
                    },
                    else => {
                        log.err("expected math or shift operator, found {any}\n", .{k});
                        return ParserError.UnexpectedToken;
                    },
                };
            }

            pub fn getPrecedence(self: Operator) Precedence {
                return switch (self) {
                    .multiply, .divide, .mod => .@"5",
                    .plus, .minus => .@"4",
                    .lsl, .lsr, .asr, .rol => .@"3",
                    .@"and" => .@"2",
                    .xor => .@"1",
                    .@"or" => .@"0",
                };
            }
        };
    };

    pub fn format(self: Complex, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .number => |number| {
                try writer.print("({})", .{number});
            },
            .label => |label| {
                try writer.print("(@'{s}')", .{label});
            },
            .operation => |operation| {
                try writer.print("(*{} {t} *{})", .{
                    operation.operand1_index,
                    operation.operator,
                    operation.operand2_index,
                });
            },
        }
    }

    pub fn evaluate(self: Complex, context: ComplexesContext) !u32 {
        return switch (self) {
            .number => |number| @intCast(number),
            .label => |label| context.labels.get(label) orelse return ParserError.UndefinedLabel,
            .operation => |operation| blk: {
                const op1 = try context.complexes[operation.operand1_index].evaluate(context);
                const op2 = try context.complexes[operation.operand2_index].evaluate(context);
                break :blk switch (operation.operator) {
                    .multiply => op1 * op2,
                    .divide => op1 / op2,
                    .mod => op1 % op2,
                    .plus => op1 + op2,
                    .minus => op1 - op2,
                    .lsl => std.math.shl(u32, op1, op2),
                    .lsr => std.math.shr(u32, op1, op2),
                    .asr => @intCast(std.math.shr(i32, @intCast(op1), op2)),
                    .rol => std.math.rotl(u32, op1, op2),
                    .@"and" => op1 & op2,
                    .xor => op1 ^ op2,
                    .@"or" => op1 | op2,
                };
            },
        };
    }
};

pub const ComplexOrRegister = union(enum) {
    complex: Complex,
    register: Token.Kind.Register,

    pub fn format(self: ComplexOrRegister, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .complex => |complex| {
                try writer.print("{f}", .{complex});
            },
            .register => |register| {
                try writer.print("{t}", .{register});
            },
        }
    }
};

pub const ComplexesContext = struct {
    complexes: []const Complex,
    labels: *const std.StringHashMap(u32),
    current_address: *const u32,
};

const std = @import("std");
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const log = std.log;

const instructions = @import("instructions");
const Instruction = instructions.Instruction;

const lexer = @import("lexer.zig");
const Token = lexer.TypedToken;

const parser = @import("parser.zig");
const ParserError = parser.ParserError;
