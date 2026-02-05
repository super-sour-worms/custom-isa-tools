// TODO: collect errors to list
pub fn createAst(
    allocator: Allocator,
    tokens: []const Token,
    file_path: []const u8,
) !struct { ArrayList(AstRootNode), ArrayList(Complex) } {
    var queue = TokenQueue{ .values = tokens, .file_path = file_path };
    var statements = try splitToStatements(allocator, &queue);
    defer statements.deinit(allocator);

    var ast_nodes: ArrayList(AstRootNode) = .empty;
    errdefer ast_nodes.deinit(allocator);
    var complexes: ArrayList(Complex) = .empty;
    errdefer complexes.deinit(allocator);
    for (statements.items) |statement| {
        var statement_queue = TokenQueue{ .values = statement, .file_path = file_path };
        try ast_nodes.append(allocator, try statement_queue.parseStatement(allocator, &complexes));
        if (statement_queue.values.len != statement_queue.pos) {
            const token = try statement_queue.next();
            // TODO: Proper error reporting
            log.err(
                "{s}:{}:{}: Unexpected token \'{s}\', expected expression or end of file",
                .{ statement_queue.file_path, token.line, token.column, token.text },
            );
            return ParserError.ExtraTokensInStatement;
        }
    }
    return .{ ast_nodes, complexes };
}

fn splitToStatements(allocator: Allocator, queue: *TokenQueue) !ArrayList([]const Token) {
    var statements: ArrayList([]const Token) = .empty; // TODO: size assumption
    errdefer statements.deinit(allocator);
    var statement_start_pos: usize = 0;
    while (queue.peekOrNull(1)) |t| switch (t.kind) {
        .register, .data_type, .alpha_numerical, .number, .symbol, .bracket, .shift_operator => {
            queue.skip(1);
        },
        .label => {
            queue.skip(1);
            if (queue.peekOrNull(1)) |t2| if (t2.kind == .symbol and t2.kind.symbol == .@":") {
                try statements.append(allocator, queue.values[statement_start_pos..queue.pos]);
                statement_start_pos = queue.pos;
            };
        },
        else => {
            queue.skip(1);
            try statements.append(allocator, queue.values[statement_start_pos..queue.pos]);
            statement_start_pos = queue.pos;
        },
    };
    try statements.append(allocator, queue.values[statement_start_pos .. queue.pos + 1]);
    return statements;
}

const TokenQueue = struct {
    values: []const Token,
    pos: usize = 0,
    /// only for diagnostics
    file_path: []const u8,

    queue_debug_logging: bool = false,

    /// Returns null if out of tokens
    fn peekOrNull(self: *const TokenQueue, offset: usize) ?*const Token {
        if (self.pos + offset >= self.values.len) {
            if (self.queue_debug_logging) {
                log.debug("{*}.peekOrNull({}) => 'null'", .{ self, offset });
            }
            return null;
        } else {
            if (self.queue_debug_logging) {
                log.debug("{*}.peekOrNull({}) => '{s}'", .{ self, offset, self.values[self.pos + offset].text });
            }
            return &self.values[self.pos + offset];
        }
    }
    /// Returns an error if out of tokens
    fn peek(self: *const TokenQueue, offset: usize) ParserError!*const Token {
        if (self.pos + offset >= self.values.len) return self.unexpEndOfTokensDiag();
        if (self.queue_debug_logging) {
            log.debug("{*}.peek({}) => '{s}'", .{ self, offset, self.values[self.pos + offset].text });
        }
        return &self.values[self.pos + offset];
    }
    /// Skips values. Does not check for end of tokens
    fn skip(self: *TokenQueue, n: usize) void {
        if (self.queue_debug_logging) {
            log.debug("{*}.skip({})", .{ self, n });
        }
        self.pos += n;
    }
    /// Returns next token and increases position by 1
    /// Returns an error if out of tokens
    fn next(self: *TokenQueue) ParserError!*const Token {
        if (self.pos >= self.values.len) return self.unexpEndOfTokensDiag();
        const token = &self.values[self.pos];
        if (self.queue_debug_logging) {
            log.debug("{*}.next() => '{s}'", .{ self, token.text });
        }
        self.pos += 1;
        return token;
    }
    /// Returns token.kind.{expected_kind} field of next token and increases position by 1
    /// Returns an error if out of tokens, or expected kind does not match next token kind
    fn nextKind(
        self: *TokenQueue,
        comptime expected: @typeInfo(Token.Kind).@"union".tag_type.?,
    ) ParserError!@FieldType(Token.Kind, @tagName(expected)) {
        if (self.pos >= self.values.len) return self.unexpEndOfTokensDiag();
        const token = &self.values[self.pos];
        if (self.queue_debug_logging) {
            log.debug("{*}.nextKind(.{t}) => .{t} '{s}'", .{ self, expected, token.kind, token.text });
        }
        if (token.kind != expected) {
            return self.unexpTokenDiag(@tagName(expected));
        }
        self.pos += 1;
        return @field(token.kind, @tagName(expected));
    }
    /// Increases position by 1
    /// Returns an error if expected token does not match next token, or out of tokens
    fn expectNext(self: *TokenQueue, comptime expected: Token.Kind) !void {
        if (self.pos >= self.values.len) return self.unexpEndOfTokensDiag();
        const token = &self.values[self.pos];
        if (self.queue_debug_logging) {
            log.debug("{*}.expectNext({any}) => .{t} '{s}'", .{ self, expected, token.kind, token.text });
        }
        if (!std.meta.eql(self.values[self.pos].kind, expected)) {
            return self.unexpTokenDiag(std.fmt.comptimePrint("'{any}'", .{expected}));
        }
        self.pos += 1;
    }

    fn unexpEndOfTokensDiag(self: *const @This()) ParserError {
        const last_token = self.values[self.values.len - 1];
        log.err("{s}:{}:{}: unexpected end of tokens", .{ self.file_path, last_token.line, last_token.column });
        return ParserError.UnexpectedEndOfTokens;
    }
    fn unexpTokenDiag(self: *const @This(), comptime expected: []const u8) ParserError {
        const current_token = try self.peek(0);
        log.err("{s}:{}:{}: unexpected token: expected {s}; found '{s}'", .{
            self.file_path,
            current_token.line,
            current_token.column,
            expected,
            current_token.text,
        });
        return ParserError.UnexpectedToken;
    }

    fn parseStatement(
        queue: *TokenQueue,
        allocator: Allocator,
        complexes: *ArrayList(Complex),
    ) !AstRootNode {
        const root_token = try queue.next();
        switch (root_token.kind) {
            .alu_instruction => |mnemonic| {
                // general form: <target: Reg> "," <op1: Reg> "," <op2: Shift>
                // mov form: <target: Reg> "," <op2: Shift>
                const target = try queue.nextKind(.register);
                try queue.expectNext(.{ .symbol = .@"," });
                const primary = if (mnemonic != .mov) try queue.nextKind(.register) else .zr;
                if (mnemonic != .mov) try queue.expectNext(.{ .symbol = .@"," });
                const secondary, const shift = try queue.parseSFormatSecondary(allocator, complexes);
                return .{ .alu_instruction = .{
                    .mnemonic = mnemonic,
                    .s_format = .{
                        .target = target,
                        .primary = primary,
                        .secondary = secondary,
                        .shift = shift,
                    },
                } };
            },
            .mem_instruction => |mnemonic| {
                // general form: <target: Reg> "," <base: Reg> "[" <offset: Shift> "]"
                // label form: <target: Reg> "," <label: Label>
                const target = try queue.nextKind(.register);
                try queue.expectNext(.{ .symbol = .@"," });

                if ((try queue.peek(0)).kind == .label) {
                    return .{ .mem_instruction = .{
                        .mnemonic = mnemonic,
                        .m_format = .{
                            .target = target,
                            .address = .{ .label_form = try queue.nextKind(.label) },
                        },
                    } };
                } else {
                    const base = try queue.nextKind(.register);
                    try queue.expectNext(.{ .bracket = .@"[" });
                    const offset, const shift = try queue.parseSFormatSecondary(allocator, complexes);
                    if (offset == .complex and shift != null) return ParserError.ImpossibleInstruction;
                    try queue.expectNext(.{ .bracket = .@"]" });
                    return .{ .mem_instruction = .{
                        .mnemonic = mnemonic,
                        .m_format = .{
                            .target = target,
                            .address = .{ .general_form = .{
                                .base = base,
                                .offset = offset,
                                .offset_shift = shift,
                            } },
                        },
                    } };
                }
            },
            .directive => |directive| switch (directive) {
                .@".binary" => return error.Unimplemented, // <datatype: DataType> <alignment: Int> "{" <value: Int> "}"
                .@".align" => return error.Unimplemented, // <datatype: DataType> <alignment: Int> "{" <value: Int> "}"
            },
            .label => |label| {
                // <label: Label> ":"
                try queue.expectNext(.{ .symbol = .@":" });
                return .{ .label_definition = label };
            },
            else => return queue.unexpTokenDiag("expression"),
        }
    }

    /// Returns slice with surrounding parenthesis, moves queue position
    fn findParenthesisExpressionBoundries(queue: *TokenQueue) ![]const Token {
        const start_pos = queue.pos;
        try queue.expectNext(.{ .bracket = .@"(" });
        var nestedness: usize = 1;
        while (true) {
            const token = (try queue.next()).kind;
            if (token == .bracket) {
                if (token.bracket == .@"(") nestedness += 1;
                if (token.bracket == .@")") nestedness -= 1;
            }
            if (nestedness == 0) return queue.values[start_pos..queue.pos];
        }
    }

    /// Usign labels as immediates is allowed only when they are in brackets
    /// In some instructions using labels without brackets is semanticly different from
    /// using address as immediate value directly.
    ///
    /// For example:
    /// mov r8, @label // not allowed
    /// mov r8, (@label + 4) // allowed
    /// mov r8, (@label) // allowed
    ///
    /// lod4 r8, @label
    /// // is roughly the same as
    /// lod4 r8, pc[(@label - @pc)]
    fn parseComplex(queue: *@This(), allocator: Allocator, complexes: *ArrayList(Complex)) !Complex {
        switch ((try queue.peek(0)).kind) {
            .number => return .{ .number = try queue.nextKind(.number) },
            .bracket => { // perhaps the most complex part of the whole fucking project
                const expression = try queue.findParenthesisExpressionBoundries();
                //for (expression) |e| log.debug("{any}  ", .{e.kind});
                var expr_queue = TokenQueue{
                    .values = expression[1 .. expression.len - 1],
                    .file_path = queue.file_path,
                };

                var foldable_expression: ArrayList(FoldableExpressionNode) = .empty;
                defer foldable_expression.deinit(allocator);
                var prev_is_oper = true; // only for validation
                while (expr_queue.peekOrNull(0)) |token| {
                    switch (token.kind) {
                        .label => {
                            if (prev_is_oper) prev_is_oper = false else return expr_queue.unexpTokenDiag("operator");
                            try foldable_expression.append(allocator, .{
                                .complex = .{ .label = try expr_queue.nextKind(.label) },
                            });
                        },
                        .number => {
                            if (prev_is_oper) prev_is_oper = false else return expr_queue.unexpTokenDiag("operator");
                            try foldable_expression.append(allocator, .{
                                .complex = .{ .number = try expr_queue.nextKind(.number) },
                            });
                        },
                        .bracket => {
                            if (prev_is_oper) prev_is_oper = false else return expr_queue.unexpTokenDiag("operator");
                            try foldable_expression.append(allocator, .{
                                .complex = try expr_queue.parseComplex(allocator, complexes),
                            });
                        },
                        .symbol, .shift_operator => {
                            if (!prev_is_oper) prev_is_oper = true else return expr_queue.unexpTokenDiag("label, number, or subexpression");
                            try foldable_expression.append(allocator, .{
                                .operator = try .fromTokenKind((try expr_queue.next()).kind),
                            });
                        },
                        else => return expr_queue.unexpTokenDiag("label, number, parenthesis, math or shift operator"),
                    }
                }
                return try FoldableExpressionNode.fold(foldable_expression.items, allocator, complexes);
            },
            else => return queue.unexpTokenDiag("number, or parenthesis"),
        }
    }
    fn parseComplexOrRegister(
        queue: *TokenQueue,
        allocator: Allocator,
        complexes: *ArrayList(Complex),
    ) !ComplexOrRegister {
        return switch ((try queue.peek(0)).kind) {
            .register => .{ .register = try queue.nextKind(.register) },
            .number, .bracket => .{ .complex = (try queue.parseComplex(allocator, complexes)) },
            else => return queue.unexpTokenDiag("register or complex"),
        };
    }

    // <base: ComplexOrRegister> [<shift_type: ShiftType> <shift_by: ImmediateOrRegister>]
    fn parseSFormatSecondary(
        queue: *TokenQueue,
        allocator: Allocator,
        complexes: *ArrayList(Complex),
    ) !struct { ComplexOrRegister, ?AstRootNode.SFormat.Shift } {
        return .{
            try queue.parseComplexOrRegister(allocator, complexes),
            if (queue.peekOrNull(0) != null and queue.peekOrNull(0).?.kind == .shift_operator) .{
                .shift_type = try queue.nextKind(.shift_operator),
                .shift_by = try queue.parseComplexOrRegister(allocator, complexes),
            } else null,
        };
    }
};

const FoldableExpressionNode = union(enum) {
    complex: Complex,
    operator: Complex.Operation.Operator,

    fn fold(
        foldable: []const FoldableExpressionNode,
        allocator: Allocator,
        complexes: *ArrayList(Complex),
    ) !Complex {
        //for (foldable) |f| log.debug("{any} ", .{f});
        var max_precedence: isize = -1;
        for (foldable) |linear_node| if (linear_node == .operator) {
            max_precedence = @max(max_precedence, @intFromEnum(linear_node.operator.getPrecedence()));
        };
        if (max_precedence == -1) {
            assert(foldable.len == 1);
            return foldable[0].complex;
        }

        // maybe parsing all operators of same precedence in single call frame by collecting foldable slices would be faster
        var iter = std.mem.reverseIterator(foldable);
        const max_precedence_enum: Complex.Operation.Operator.Precedence = @enumFromInt(max_precedence);
        while (iter.next()) |linear_node| if (linear_node == .operator) {
            if (linear_node.operator.getPrecedence() == max_precedence_enum) {
                try complexes.append(allocator, try fold(foldable[0..iter.index], allocator, complexes));
                try complexes.append(allocator, try fold(foldable[iter.index + 1 ..], allocator, complexes));
                return .{ .operation = .{
                    .operand1_index = complexes.items.len - 2,
                    .operator = foldable[iter.index].operator,
                    .operand2_index = complexes.items.len - 1,
                } };
            }
        };
        unreachable;
    }
};

pub const ParserError = error{
    UnexpectedEndOfTokens,
    UnexpectedToken,
    ImpossibleInstruction,
    ExtraTokensInStatement,
    UndefinedLabel,
};

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const log = std.log;
const assert = std.debug.assert;

const instructions = @import("instructions");
const Instruction = instructions.Instruction;

const lexer = @import("lexer.zig");
const Token = lexer.TypedToken;

const ast = @import("ast.zig");
const AstRootNode = ast.AstRootNode;
const Complex = ast.Complex;
const ComplexOrRegister = ast.ComplexOrRegister;
