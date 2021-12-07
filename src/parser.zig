const std = @import("std");
const Tokenizer = @import("tokenizer.zig");
const Token = Tokenizer.Token;

pub const Parser = @This();

const AstNodeKind = enum {
    NK_ADD, // +
    NK_SUB, // -
    NK_MUL, // *
    NK_DIV, // /
    NK_NUM, // Integer
};

const AstTree = struct {

    // AST node type
    pub const Node = struct {
        kind: AstNodeKind, // Node kind
        lhs: ?*const Node = null, // Left-hand side
        rhs: ?*const Node = null, // Right-hand side
        value: u64 = undefined, // Used if kind == NK_NUM
    };

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) AstTree {
        return .{ .allocator = allocator };
    }

    pub fn binaryExpression(self: *AstTree, kind: AstNodeKind, lhs: *const Node, rhs: *const Node) std.mem.Allocator.Error!*const Node {
        var binaray_node = try self.allocator.create(Node);
        binaray_node.rhs = rhs;
        binaray_node.lhs = lhs;
        binaray_node.kind = kind;
        binaray_node.value = undefined;
        return binaray_node;
    }

    pub fn number(self: *AstTree, value: u64) std.mem.Allocator.Error!*const Node {
        var num_terminal_node = try self.allocator.create(Node);
        num_terminal_node.value = value;
        num_terminal_node.kind = .NK_NUM;
        return num_terminal_node;
    }
};

pub const AstNode = AstTree.Node;

nodes: AstTree,
tokenizer: Tokenizer,
next_ast_node_index: usize,

pub fn init(allocator: std.mem.Allocator, input_token_stream: []const u8) Parser {
    return .{
        .nodes = AstTree.init(allocator),
        .tokenizer = Tokenizer.init(allocator, input_token_stream),
        .next_ast_node_index = 0,
    };
}
const Error = error{
    InvalidExpression,
    TerminalMismatch,
} || std.os.WriteError || std.mem.Allocator.Error;

pub fn tokenizeInput(self: *Parser) !*const Token {
    return try self.tokenizer.tokenize();
}

// expr = mul ("+" mul | "-" mul)*
pub fn parseExpression(self: *Parser, token: *const Token) Error!*const AstNode {
    const lhs_node = try self.mul(token);
    var next_lhs_node = lhs_node;
    while (true) {
        if (self.tokenizer.isCurrentTokenEqualTo("+")) {
            const rhs_node = try self.mul(self.tokenizer.nextToken());
            next_lhs_node = try self.nodes.binaryExpression(.NK_ADD, next_lhs_node, rhs_node);
            continue;
        }

        if (self.tokenizer.isCurrentTokenEqualTo("-")) {
            const rhs_node = try self.mul(self.tokenizer.nextToken());
            next_lhs_node = try self.nodes.binaryExpression(.NK_SUB, next_lhs_node, rhs_node);
            continue;
        }
        return next_lhs_node;
    }
}

//  mul = primary ("*" primary | "/" primary)*
fn mul(self: *Parser, token: *const Token) Error!*const AstNode {
    const lhs_node = try self.primary(token);
    var next_lhs_node = lhs_node;
    while (true) {
        if (self.tokenizer.isCurrentTokenEqualTo("*")) {
            const rhs_node = try self.primary(self.tokenizer.nextToken());
            next_lhs_node = try self.nodes.binaryExpression(.NK_MUL, next_lhs_node, rhs_node);
            continue;
        }

        if (self.tokenizer.isCurrentTokenEqualTo("/")) {
            const rhs_node = try self.primary(self.tokenizer.nextToken());
            next_lhs_node = try self.nodes.binaryExpression(.NK_DIV, next_lhs_node, rhs_node);
            continue;
        }

        return next_lhs_node;
    }
}

// primary = "(" expr ")" | num
fn primary(self: *Parser, token: *const Token) Error!*const AstNode {
    if (self.tokenizer.isCurrentTokenEqualTo("(")) {
        const expr_node = try self.parseExpression(self.tokenizer.nextToken());
        if (self.tokenizer.isCurrentTokenMatch(")")) {} else {
            self.tokenizer.reportError("expected token to be ) but found {s}", .{self.tokenizer.currentToken().lexeme});
            return error.TerminalMismatch;
        }
        return expr_node;
    }
    if (token.kind == .TK_NUM) {
        const num_node = try self.nodes.number(token.value);
        _ = self.tokenizer.nextToken();
        return num_node;
    }
    self.tokenizer.reportError("Expected an expression", .{});
    return error.InvalidExpression;
}
