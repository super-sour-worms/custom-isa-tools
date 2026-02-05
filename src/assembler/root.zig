/// file_path argument is used only in debug messages
pub fn assemble(allocator: Allocator, src: []const u8, file_path: []const u8) !ArrayList(u8) {
    var tokens = try lexer.tokenize(allocator, src, file_path);
    defer tokens.deinit(allocator);
    //dumpTokensToConsole(tokens.items);

    var ast_nodes, var complexes = try parser.createAst(allocator, tokens.items, file_path);
    defer ast_nodes.deinit(allocator);
    defer complexes.deinit(allocator);
    //dumpAstToConsole(ast_nodes.items);

    return try mcg.generate(allocator, ast_nodes.items, complexes.items);
}

pub fn dumpTokensToConsole(tokens: []const lexer.TypedToken) void {
    log.debug("Token dump:", .{});
    for (tokens) |token| {
        log.debug("    text: \"{s}\", kind: {}", .{ token.text, token.kind });
    }
}

pub fn dumpAstToConsole(nodes: []const ast.AstRootNode) void {
    log.debug("AST dump:", .{});
    for (nodes) |node| {
        log.debug("    {f}", .{node});
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const log = std.log;

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const mcg = @import("machine_code_generator.zig");
