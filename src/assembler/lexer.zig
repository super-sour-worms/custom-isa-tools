// TODO: binary not (~) operator
/// file_path argument is used only in debug messages
pub fn tokenize(allocator: Allocator, src: []const u8, file_path: []const u8) !ArrayList(TypedToken) {
    var tokens: ArrayList(UntypedToken) = .empty;
    defer tokens.deinit(allocator);
    var queue: CharQueue = .{ .values = src, .file_path = file_path };
    while (queue.peek(0)) |char| {
        const line = queue.line;
        const column = queue.column;
        switch (char) {
            'A'...'Z', 'a'...'z', '@', '_', '.' => try tokens.append(allocator, .{
                .kind = .alpha_numerical,
                .text = try queue.readAlphaNumerical(),
                .line = line,
                .column = column,
            }),

            '0'...'9' => try tokens.append(allocator, .{
                .kind = .number,
                .text = try queue.readNumber(),
                .line = line,
                .column = column,
            }),

            '-' => try tokens.append(allocator, switch (queue.peek(1) orelse return queue.unexpSrcEndDiag()) {
                '>' => .{ .kind = .operator, .text = try queue.nextSlice(2), .line = line, .column = column },
                '0'...'9' => .{ .kind = .number, .text = try queue.readNumber(), .line = line, .column = column },
                else => .{ .kind = .operator, .text = try queue.nextSlice(1), .line = line, .column = column },
            }),

            '<' => switch ((queue.peek(1) orelse return queue.unexpSrcEndDiag())) {
                '<', '>' => try tokens.append(allocator, .{
                    .kind = .operator,
                    .text = try queue.nextSlice(2),
                    .line = line,
                    .column = column,
                }),
                else => return queue.invTokenDiag(),
            },

            '>' => switch ((queue.peek(1) orelse return queue.unexpSrcEndDiag())) {
                '>' => try tokens.append(allocator, .{
                    .kind = .operator,
                    .text = try queue.nextSlice(2),
                    .line = line,
                    .column = column,
                }),
                else => return queue.invTokenDiag(),
            },

            ',', '(', ')', '[', ']', '{', '}', '+', '*', '/', '%', '&', '^', '|', ':' => {
                try tokens.append(allocator, .{
                    .kind = .operator,
                    .text = try queue.nextSlice(1),
                    .line = line,
                    .column = column,
                });
            },

            ' ', '\t', '\n' => try queue.skip(1),

            // TODO: replace with '#' or "//"
            ';' => while ((queue.peek(0) orelse break) != '\n') try queue.skip(1),

            else => {
                log.err("Invalid token start symbol: \'{c}\'\n", .{queue.peek(0).?});
                return queue.invTokenDiag();
            },
        }
    }

    var typed_tokens: ArrayList(TypedToken) = try .initCapacity(allocator, tokens.items.len);
    for (tokens.items) |untyped_token| {
        try typed_tokens.append(allocator, try untyped_token.toTyped());
    }
    return typed_tokens;
}

const UntypedToken = struct {
    line: usize,
    column: usize,
    text: []const u8,
    kind: enum { alpha_numerical, number, operator },
    pub fn toTyped(untyped: UntypedToken) !TypedToken {
        return .{
            .line = untyped.line,
            .column = untyped.column,
            .text = untyped.text,
            .kind = switch (untyped.kind) {
                .alpha_numerical => blk: {
                    if (meta.stringToEnum(TypedToken.Kind.Register, untyped.text)) |register| {
                        break :blk .{ .register = register };
                    }
                    if (meta.stringToEnum(TypedToken.Kind.AluInstruction, untyped.text)) |alu_instruction| {
                        break :blk .{ .alu_instruction = alu_instruction };
                    }
                    if (meta.stringToEnum(TypedToken.Kind.MemInstruction, untyped.text)) |mem_instruction| {
                        break :blk .{ .mem_instruction = mem_instruction };
                    }
                    if (meta.stringToEnum(TypedToken.Kind.Directive, untyped.text)) |directive| {
                        break :blk .{ .directive = directive };
                    }
                    if (meta.stringToEnum(TypedToken.Kind.DataType, untyped.text)) |data_type| {
                        break :blk .{ .data_type = data_type };
                    }
                    if (untyped.text[0] == '@') {
                        break :blk .{ .label = untyped.text[1..] };
                    }
                    break :blk .{ .alpha_numerical = untyped.text };
                },
                .number => .{ .number = try std.fmt.parseInt(@FieldType(TypedToken.Kind, "number"), untyped.text, 0) },

                .operator => blk: {
                    if (meta.stringToEnum(TypedToken.Kind.Symbol, untyped.text)) |symbol| {
                        break :blk .{ .symbol = symbol };
                    }
                    if (meta.stringToEnum(TypedToken.Kind.Bracket, untyped.text)) |bracket| {
                        break :blk .{ .bracket = bracket };
                    }
                    break :blk .{ .shift_operator = meta.stringToEnum(TypedToken.Kind.ShiftOperator, untyped.text).? };
                },
            },
        };
    }
};

pub const TypedToken = struct {
    line: usize,
    column: usize,
    text: []const u8,
    kind: Kind,
    pub const Kind = union(enum) {
        alu_instruction: AluInstruction,
        mem_instruction: MemInstruction,
        directive: Directive,
        register: Register,
        data_type: DataType,
        alpha_numerical: []const u8,
        label: []const u8,
        number: i64,
        symbol: Symbol,
        bracket: Bracket,
        shift_operator: ShiftOperator,

        pub const AluInstruction = enum {
            add,
            sub,
            xor,
            brs,
            @"and",
            nand,
            nor,
            @"or",
            mov,
            pub fn toBinaryField(self: @This()) instructions.AluOpcode {
                return switch (self) {
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
            }
        };
        pub const MemInstruction = enum {
            lod1,
            str1,
            lod2,
            str2,
            lod4,
            str4,
            pub fn toBinaryField(self: @This()) instructions.MemOpcode {
                return switch (self) {
                    .lod1 => .lod1,
                    .str1 => .str1,
                    .lod2 => .lod2,
                    .str2 => .str2,
                    .lod4 => .lod4,
                    .str4 => .str4,
                };
            }
        };
        pub const Directive = enum { @".binary", @".align" };
        pub const Register = enum {
            zr,
            pc,
            r2,
            r3,
            r4,
            r5,
            r6,
            r7,
            r8,
            r9,
            r10,
            r11,
            r12,
            r13,
            r14,
            r15,
            pub fn toBinaryField(self: @This()) instructions.Register {
                return switch (self) {
                    .zr => .zr,
                    .pc => .pc,
                    .r2 => .r2,
                    .r3 => .r3,
                    .r4 => .r4,
                    .r5 => .r5,
                    .r6 => .r6,
                    .r7 => .r7,
                    .r8 => .r8,
                    .r9 => .r9,
                    .r10 => .r10,
                    .r11 => .r11,
                    .r12 => .r12,
                    .r13 => .r13,
                    .r14 => .r14,
                    .r15 => .r15,
                };
            }
        };
        pub const DataType = enum { u8, i8, u16, i16, u32, i32 };
        pub const Symbol = enum { @",", @"+", @"-", @"*", @"/", @"%", @"&", @"^", @"|", @":" };
        pub const Bracket = enum { @"(", @")", @"[", @"]", @"{", @"}" };
        pub const ShiftOperator = enum {
            @"<<",
            @">>",
            @"->",
            @"<>",
            pub fn toBinaryField(self: @This()) instructions.Instruction.SOrMFormat.ShiftType {
                return switch (self) {
                    .@"<<" => .lsl,
                    .@">>" => .lsr,
                    .@"->" => .asr,
                    .@"<>" => .rol,
                };
            }
        };
    };
};

const CharQueue = struct {
    values: []const u8,
    pos: usize = 0,
    line: usize = 1,
    column: usize = 1,
    /// only for diagnostics
    file_path: []const u8,

    fn peek(self: *const @This(), offset: usize) ?u8 {
        if (self.pos + offset >= self.values.len) {
            return null;
        }
        return self.values[self.pos + offset];
    }
    fn skip(self: *@This(), n: usize) !void {
        for (0..n) |_| {
            const char = self.peek(0);
            if (char == null) return self.unexpSrcEndDiag();
            if (char.? == '\n') {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.pos += 1;
        }
    }
    // fn next(self: *@This()) !u8 {
    //     const char = self.peek(0);
    //     try self.skip(1);
    //     return char;
    // }
    fn nextSlice(self: *@This(), n: usize) ![]const u8 {
        const slice = self.values[self.pos .. self.pos + n];
        try self.skip(n);
        return slice;
    }

    fn unexpSrcEndDiag(self: *const @This()) LexerError {
        log.err("{s}:{}:{}: Unexpected source code end", .{ self.file_path, self.line, self.column });
        return LexerError.UnexpectedSourceEnd;
    }

    fn invTokenDiag(self: *const @This()) LexerError {
        log.err("{s}:{}:{}: Invalid token", .{ self.file_path, self.line, self.column });
        return LexerError.InvalidToken;
    }

    /// Scans string from beggining to find an alpha numerical token end, returns token substring, moves queue position
    /// '@' is allowed only as the first character
    fn readAlphaNumerical(queue: *@This()) ![]const u8 {
        const start_pos = queue.pos;
        while (queue.peek(0)) |char| switch (char) {
            'a'...'z', 'A'...'Z', '0'...'9', '_', '.' => try queue.skip(1),
            '@' => if (queue.pos == start_pos) try queue.skip(1) else break,
            else => break,
        };
        return queue.values[start_pos..queue.pos];
    }

    /// Scans string from beggining to find a number end, returns number substring, moves queue position
    fn readNumber(queue: *CharQueue) ![]const u8 {
        const start_pos = queue.pos;
        while ((queue.peek(0) orelse return queue.unexpSrcEndDiag()) == '-') {
            try queue.skip(1);
        }
        // Read number base
        const BaseEnum = enum { B, O, D, X };
        var base = BaseEnum.D;
        if (queue.peek(0) != null and queue.peek(0) == '0') blk: {
            base = switch (queue.peek(1) orelse break :blk) {
                'B', 'b' => .B,
                'O', 'o' => .O,
                'X', 'x' => .X,
                '0'...'9' => return LexerError.LeadingZeros,
                else => return queue.values[start_pos..start_pos],
            };
            try queue.skip(2);
        }
        // Read numbers
        switch (base) {
            .B => {
                while (queue.peek(0)) |char| switch (char) {
                    '_', '0', '1' => try queue.skip(1),
                    else => break,
                };
            },
            .O => {
                while (queue.peek(0)) |char| switch (char) {
                    '_', '0'...'7' => try queue.skip(1),
                    else => break,
                };
            },
            .D => {
                while (queue.peek(0)) |char| switch (char) {
                    '_', '0'...'9' => try queue.skip(1),
                    else => break,
                };
            },
            .X => {
                while (queue.peek(0)) |char| switch (char) {
                    '_', '0'...'9', 'a'...'f', 'A'...'F' => try queue.skip(1),
                    else => break,
                };
            },
        }
        return queue.values[start_pos..queue.pos];
    }
};

// TODO: Leading Zeros diagnostics
const LexerError = error{ InvalidToken, LeadingZeros, UnexpectedSourceEnd };

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const log = std.log;
const meta = std.meta;
const instructions = @import("instructions");
