const std = @import("std");
const Tokenizer = @import("tokenizer.zig");
const Token = Tokenizer.Token;

pub const Parser = @This();

const AstNodeKind = enum {
    NK_ADD, // +
    NK_SUB, // -
    NK_MUL, // *
    NK_DIV, // /
    NK_NEG, // Negative -Num
    NK_NUM, // Integer
    NK_EQ, // ==
    NK_NE, // !=
    NK_LT, // <
    NK_GT, // >
    NK_LE, // <=
    NK_GE, // >=
};

const AstTree = struct {

    // AST node type
    pub const Node = struct {
        kind: AstNodeKind, // Node kind
        lhs: ?*const Node = null, // Left-hand side
        rhs: ?*const Node = null, // Right-hand side
        value: u64 = undefined, // Used if kind == NK_NUM
    };
    const AstError = error{} || std.mem.Allocator.Error;

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) AstTree {
        return .{ .allocator = allocator };
    }

    fn createAstNode(self: *AstTree, kind: AstNodeKind) *Node {
        var new_ast_node = self.allocator.create(Node) catch |err| {
            std.log.err("{s} :allocator run out of memory", .{@errorName(err)});
            std.debug.panic("Out of Memory condition", .{});
        };
        new_ast_node.kind = kind;
        return new_ast_node;
    }

    pub fn binaryExpression(self: *AstTree, kind: AstNodeKind, lhs: *const Node, rhs: *const Node) *const Node {
        var binaray_node = self.createAstNode(kind);
        binaray_node.rhs = rhs;
        binaray_node.lhs = lhs;
        binaray_node.value = undefined;
        return binaray_node;
    }

    pub fn number(self: *AstTree, value: u64) *const Node {
        var num_terminal_node = self.createAstNode(.NK_NUM);
        num_terminal_node.value = value;
        return num_terminal_node;
    }

    pub fn unaryExpression(self: *AstTree, Kind: AstNodeKind, expr: *const Node) *const Node {
        var unary_node = self.createAstNode(Kind);
        unary_node.rhs = expr;
        unary_node.lhs = undefined;
        unary_node.value = undefined;
        return unary_node;
    }
};

pub const AstNode = AstTree.Node;

nodes: AstTree,
tokenizer: Tokenizer,

pub fn init(allocator: std.mem.Allocator, input_token_stream: []const u8) Parser {
    return .{
        .nodes = AstTree.init(allocator),
        .tokenizer = Tokenizer.init(allocator, input_token_stream),
    };
}
const Error = error{
    InvalidExpression,
    TerminalMismatch,
} || std.os.WriteError || std.mem.Allocator.Error;

pub fn tokenizeInput(self: *Parser) !*const Token {
    return try self.tokenizer.tokenize();
}

// expr = equality
pub fn parse(self: *Parser, token: *const Token) *const AstNode {
    return self.equality(token);
}

//equality = relational ("==" relational | "!=" relational)*
fn equality(self: *Parser, token: *const Token) *const AstNode {
    const equality_lhs_node = self.relational(token);
    var equality_tree_node = equality_lhs_node;

    while (true) {
        if (self.tokenizer.isCurrentTokenEqualTo("==")) {
            const equality_rhs_node = self.relational(self.tokenizer.nextToken());
            equality_tree_node = self.nodes.binaryExpression(.NK_EQ, equality_lhs_node, equality_rhs_node);
            continue;
        }
        if (self.tokenizer.isCurrentTokenEqualTo("!=")) {
            const equality_rhs_node = self.relational(self.tokenizer.nextToken());
            equality_tree_node = self.nodes.binaryExpression(.NK_NE, equality_lhs_node, equality_rhs_node);
            continue;
        }
        return equality_tree_node;
    }
}

//relational = add ("<" add | "<=" add | ">" add | ">=" add)*
fn relational(self: *Parser, token: *const Token) *const AstNode {
    const relational_lhs_node = self.add(token);
    var relational_tree_node = relational_lhs_node;

    while (true) {
        if (self.tokenizer.isCurrentTokenEqualTo("<")) {
            const relational_rhs_node = self.add(self.tokenizer.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_LT, relational_lhs_node, relational_rhs_node);
            continue;
        }
        if (self.tokenizer.isCurrentTokenEqualTo("<=")) {
            const relational_rhs_node = self.add(self.tokenizer.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_LE, relational_lhs_node, relational_rhs_node);
            continue;
        }
        if (self.tokenizer.isCurrentTokenEqualTo(">")) {
            const relational_rhs_node = self.add(self.tokenizer.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_GT, relational_lhs_node, relational_rhs_node);
            continue;
        }
        if (self.tokenizer.isCurrentTokenEqualTo(">=")) {
            const relational_rhs_node = self.add(self.tokenizer.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_GE, relational_lhs_node, relational_rhs_node);
            continue;
        }
        return relational_tree_node;
    }
}
// add = mul ("+" mul | "-" mul)
pub fn add(self: *Parser, token: *const Token) *const AstNode {
    const lhs_node = self.mul(token);
    var next_lhs_node = lhs_node;
    while (true) {
        if (self.tokenizer.isCurrentTokenEqualTo("+")) {
            const rhs_node = self.mul(self.tokenizer.nextToken());
            next_lhs_node = self.nodes.binaryExpression(.NK_ADD, next_lhs_node, rhs_node);
            continue;
        }

        if (self.tokenizer.isCurrentTokenEqualTo("-")) {
            const rhs_node = self.mul(self.tokenizer.nextToken());
            next_lhs_node = self.nodes.binaryExpression(.NK_SUB, next_lhs_node, rhs_node);
            continue;
        }
        return next_lhs_node;
    }
}

//  mul = unary ("*" unary | "/" unary)*
fn mul(self: *Parser, token: *const Token) *const AstNode {
    const lhs_node = self.unary(token);
    var next_lhs_node = lhs_node;
    while (true) {
        if (self.tokenizer.isCurrentTokenEqualTo("*")) {
            const rhs_node = self.unary(self.tokenizer.nextToken());
            next_lhs_node = self.nodes.binaryExpression(.NK_MUL, next_lhs_node, rhs_node);
            continue;
        }

        if (self.tokenizer.isCurrentTokenEqualTo("/")) {
            const rhs_node = self.unary(self.tokenizer.nextToken());
            next_lhs_node = self.nodes.binaryExpression(.NK_DIV, next_lhs_node, rhs_node);
            continue;
        }

        return next_lhs_node;
    }
}

// unary = ('+'|'-') unary | primary
fn unary(self: *Parser, token: *const Token) *const AstNode {
    if (self.tokenizer.isCurrentTokenEqualTo("+")) {
        return self.unary(self.tokenizer.nextToken());
    }

    if (self.tokenizer.isCurrentTokenEqualTo("-")) {
        return self.nodes.unaryExpression(.NK_NEG, self.unary(self.tokenizer.nextToken()));
    }
    _ = token;

    return self.primary(self.tokenizer.currentToken()) catch |err| switch (err) {
        error.InvalidExpression => {
            std.log.err("{s} :Invalid primary expression", .{@errorName(err)});
            std.process.exit(1);
        },
        error.TerminalMismatch => {
            std.log.err("{s} : Terminals ( must end with a corresponding )", .{@errorName(err)});
            std.process.exit(1);
        },
    };
}

// primary = "(" expr ")" | NUM
fn primary(self: *Parser, token: *const Token) !*const AstNode {
    if (self.tokenizer.isCurrentTokenEqualTo("(")) {
        const expr_node = self.parse(self.tokenizer.nextToken());
        if (self.tokenizer.isCurrentTokenMatch(")")) {} else {
            self.tokenizer.reportError("expected token to be ) but found {s}", .{self.tokenizer.currentToken().lexeme});
            return error.TerminalMismatch;
        }
        return expr_node;
    }
    if (token.kind == .TK_NUM) {
        const num_node = self.nodes.number(token.value);
        _ = self.tokenizer.nextToken();
        return num_node;
    }
    self.tokenizer.reportError("Expected an expression", .{});
    return error.InvalidExpression;
}
