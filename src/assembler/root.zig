const CodeGenerationError = error{
    UnexpectedToken,
    OutOfTokens,
};

const TokenizerError = error{
    InvalidToken,
};

const Token = struct {
    line: usize,
    column: usize,
    value: Value,

    const Mnemonic = union(enum) {
        alu: Alu,
        mem: Mem,
        const Alu = enum { add, sub, xor, brs, @"and", nand, nor, @"or", mov };
        const Mem = enum { blod, bstr, hlod, hstr, lod, str };
    };
    const ShiftType = enum { lsl, lsr, asr, ror };
    const RegisterAliases = enum(u5) {
        zr = 0,
        pc = 1,
    };
    const Directive = enum {
        byte,
    };

    const Value = union(enum) {
        mnemonic: Mnemonic,
        register: u5,
        immediate: u24,
        number: u32,
        shift_type: ShiftType,
        symbol: enum { comma },
        directive: Directive,

        fn fromString(str: []const u8) !Value {
            // std.debug.print("token: {s}\n", .{str});
            return parseMnemonic(str) orelse
                pasreShiftType(str) orelse
                parseRegister(str) orelse
                parseDirective(str) orelse
                TokenizerError.InvalidToken;
        }

        fn parseMnemonic(str: []const u8) ?Value {
            if (std.meta.stringToEnum(Token.Mnemonic.Alu, str)) |value|
                return .{ .mnemonic = .{ .alu = value } };
            if (std.meta.stringToEnum(Token.Mnemonic.Mem, str)) |value|
                return .{ .mnemonic = .{ .mem = value } };
            return null;
        }

        fn pasreShiftType(str: []const u8) ?Value {
            if (std.meta.stringToEnum(Token.ShiftType, str)) |value|
                return .{ .shift_type = value };
            return null;
        }

        fn parseRegister(str: []const u8) ?Value {
            if (std.meta.stringToEnum(Token.RegisterAliases, str)) |value|
                return .{ .register = @intFromEnum(value) };
            if (str.len != 2 and str.len != 3)
                return null;
            const reg_index = std.fmt.parseInt(u5, str[1..], 10) catch {
                return null;
            };
            return switch (str[0]) {
                'g' => .{ .register = reg_index },
                'l' => .{ .register = reg_index + 8 },
                else => return null,
            };
        }

        fn parseDirective(str: []const u8) ?Value {
            if (str.len < 1 or str[0] != '.') {
                return null;
            }
            if (std.meta.stringToEnum(Token.Directive, str[1..])) |value|
                return .{ .directive = value };
            return null;
        }

        fn isComma(self: Value) bool {
            return self == .symbol and self.symbol == .comma;
        }
    };
};

fn readAlphaNumericalString(str: []const u8) []const u8 {
    for (str, 0..) |char, i| {
        switch (char) {
            'A'...'Z', 'a'...'z', '0'...'9', '.', '_' => {},
            else => return str[0..i],
        }
    }
    return str;
}

fn readNumericalString(str: []const u8) !struct { u32, usize } {
    for (str, 0..) |char, i| {
        switch (std.ascii.toLower(char)) {
            '0'...'9', '_', 'a'...'f', 'o', 'x' => {},
            else => return .{ try std.fmt.parseInt(u32, str[0..i], 0), i },
        }
    }
    return .{ try std.fmt.parseInt(u32, str, 0), str.len };
}

fn tokenize(source: []const u8) ![]Token {
    var tokens = std.ArrayList(Token).init(allocator);
    var lines = std.mem.splitScalar(u8, source, '\n');
    var line_number: usize = 0;
    while (lines.next()) |line| {
        var column: usize = 0;
        while (column < line.len) {
            switch (line[column]) {
                'A'...'Z', 'a'...'z', '.', '_' => {
                    const token_string = readAlphaNumericalString(line[column..]);
                    const token: Token = .{
                        .line = line_number,
                        .column = column,
                        .value = try Token.Value.fromString(token_string),
                    };
                    column += @as(usize, token_string.len);
                    try tokens.append(token);
                },
                '0'...'9' => {
                    const number, const num_len = try readNumericalString(line[column..]);
                    try tokens.append(.{
                        .line = line_number,
                        .column = column,
                        .value = .{ .number = number },
                    });
                    column += num_len;
                },
                '#' => {
                    const number, const num_len = try readNumericalString(line[column + 1 ..]);
                    try tokens.append(.{
                        .line = line_number,
                        .column = column,
                        .value = .{ .immediate = @intCast(number) },
                    });
                    column += num_len + 1;
                },
                ',' => {
                    try tokens.append(.{
                        .line = line_number,
                        .column = column,
                        .value = .{ .symbol = .comma },
                    });
                    column += 1;
                },
                ' ', '\t' => column += 1,
                ';' => break,
                else => return TokenizerError.InvalidToken,
            }
        }
        line_number += 1;
    }
    return tokens.items;
}

const TokenIterator = struct {
    slice: []const Token,
    index: usize = 0,
    fn peek(self: *@This()) !*const Token {
        if (self.index >= self.slice.len) {
            return CodeGenerationError.OutOfTokens;
        }
        return &self.slice[self.index];
    }

    fn next(self: *@This()) !*const Token {
        defer self.index += 1;
        return try self.peek();
    }
};

fn generateAluInstruction(mnemonic: Token.Mnemonic.Alu, tokens: *TokenIterator) !instructions.Instruction {
    const opcode: instructions.AluOpcode = switch (mnemonic) {
        .add => .add,
        .sub => .sub,
        .xor => .xor,
        .brs => .brs,
        .@"and" => .@"and",
        .nand => .nand,
        .nor => .nor,
        .@"or" => .@"or",
        .mov => .add,
    };
    const prim_op_reg = if (mnemonic == .mov) blk: {
        break :blk 0;
    } else blk: {
        const reg = switch ((try tokens.next()).value) {
            .register => |register| register,
            else => return CodeGenerationError.UnexpectedToken,
        };
        if (!(try tokens.next()).value.isComma()) {
            return CodeGenerationError.UnexpectedToken;
        }
        break :blk reg;
    };
    const sec_op_value_to_shift: u8, const is_sec_op_reg = switch ((try tokens.next()).value) {
        .immediate => |immediate| .{ @intCast(immediate), false },
        .register => |register| .{ @intCast(register), true },
        else => return CodeGenerationError.UnexpectedToken,
    };
    const sec_op: instructions.Instruction.SFormat.SecondaryOperand = if (is_sec_op_reg) .{ .reg = .{
        .is_shifted_by_reg = false,
        .shift_type = .lsl,
        .value_to_shift_reg = @intCast(sec_op_value_to_shift),
        .shift_by = 0,
    } } else .{ .imm = .{
        .value_to_shift = sec_op_value_to_shift,
        .shift_by = 0,
    } };
    if (!(try tokens.next()).value.isComma()) {
        return CodeGenerationError.UnexpectedToken;
    }
    const targ_reg = switch ((try tokens.next()).value) {
        .register => |register| register,
        else => return CodeGenerationError.UnexpectedToken,
    };
    return instructions.Instruction{
        .type = .alu,
        .payload = .{ .s_format = .{
            .opcode = @intFromEnum(opcode),
            .target_reg = targ_reg,
            .primary_operand_reg = prim_op_reg,
            .shift_source_is_register = is_sec_op_reg,
            .secondary_operand = sec_op,
        } },
    };
}

fn generateBinaryFromTokens(tokens: []const Token) ![]u8 {
    var output = std.ArrayList(u8).init(allocator);
    var token_iter: TokenIterator = .{ .slice = tokens };
    while (true) {
        const token = token_iter.next() catch {
            break;
        };
        switch (token.value) {
            .mnemonic => |mnemonic| switch (mnemonic) {
                .alu => |alu_mnemonic| {
                    // try output.addManyAsArray(4);
                    const instr = try generateAluInstruction(alu_mnemonic, &token_iter);
                    std.mem.writeInt(u32, try output.addManyAsArray(4), @bitCast(instr), std.builtin.Endian.little);
                },
                else => {},
            },
            .directive => |directive| switch (directive) {
                .byte => {
                    while ((token_iter.peek() catch {
                        break;
                    }).value == .number) {
                        try output.append(@intCast((try token_iter.next()).value.number));
                        const next_token = (token_iter.peek() catch {
                            break;
                        }).value;
                        if (next_token == .symbol and next_token.symbol == .comma) {
                            _ = try token_iter.next();
                        } else {
                            break;
                        }
                    }
                },
            },
            else => return CodeGenerationError.UnexpectedToken,
        }
    }
    return @ptrCast(output.items);
}

pub fn assemble(source: []const u8) ![]u8 {
    const tokens = try tokenize(source);
    return try generateBinaryFromTokens(tokens);
}

const std = @import("std");
const instructions = @import("instructions");
var general_purpose_allocator: std.heap.GeneralPurposeAllocator(.{}) = .init;
const allocator = general_purpose_allocator.allocator();
