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
    NK_EXPR_STMT, // Expression statement
};

const Error = error{
    InvalidExpression,
    TerminalMismatch,
} || std.os.WriteError || std.mem.Allocator.Error;

pub const AstNode = AstTree.Node;

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

    pub fn unaryExpression(self: *AstTree, Kind: AstNodeKind, unary_expr: *const Node) *const Node {
        var unary_node = self.createAstNode(Kind);
        unary_node.rhs = undefined;
        unary_node.lhs = unary_expr;
        unary_node.value = undefined;
        return unary_node;
    }
};

nodes: AstTree,
tokenizer: Tokenizer,
statements: std.ArrayList(*const AstNode),
items_position: usize = 0,

pub fn init(allocator: std.mem.Allocator, input_token_stream: []const u8) Parser {
    return .{
        .nodes = AstTree.init(allocator),
        .tokenizer = Tokenizer.init(allocator, input_token_stream),
        .statements = std.ArrayList(*const AstNode).init(allocator),
    };
}

pub fn tokenizeInput(self: *Parser) !*const Token {
    return try self.tokenizer.tokenize();
}
//program = stmt
pub fn parse(self: *Parser, token: *const Token) []*const AstNode {
    // NOTE: This can be replaced by a singly linked list of AstNode
    while (self.currentToken().kind != .TK_EOF) {
        self.statements.append(self.stmt(token)) catch |OOM| {
            std.log.err("Error {s}", .{@errorName(OOM)});
            std.debug.panic("Allocator Out Of Memory", .{});
            std.process.exit(3);
        };
    }
    return self.statements.items;
}

// stmt = expr_stmt
fn stmt(self: *Parser, token: *const Token) *const AstNode {
    return self.expr_stmt(token);
}

// expr_stmt = expr ";"
fn expr_stmt(self: *Parser, token: *const Token) *const AstNode {
    const expr_stmt_node = self.nodes.unaryExpression(.NK_EXPR_STMT, self.expr(token));
    if (self.expectCurrentTokenToMatch(";")) {} else {
        self.reportParserError("expected statement to end with ';' but found {s}", .{self.currentToken().lexeme});
    }
    return expr_stmt_node;
}

// expr = equality
fn expr(self: *Parser, token: *const Token) *const AstNode {
    return self.equality(token);
}

//equality = relational ("==" relational | "!=" relational)*
fn equality(self: *Parser, token: *const Token) *const AstNode {
    const equality_lhs_node = self.relational(token);
    var equality_tree_node = equality_lhs_node;

    while (true) {
        if (self.isCurrentTokenEqualTo("==")) {
            const equality_rhs_node = self.relational(self.nextToken());
            equality_tree_node = self.nodes.binaryExpression(.NK_EQ, equality_lhs_node, equality_rhs_node);
            continue;
        }
        if (self.isCurrentTokenEqualTo("!=")) {
            const equality_rhs_node = self.relational(self.nextToken());
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
        if (self.isCurrentTokenEqualTo("<")) {
            const relational_rhs_node = self.add(self.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_LT, relational_lhs_node, relational_rhs_node);
            continue;
        }
        if (self.isCurrentTokenEqualTo("<=")) {
            const relational_rhs_node = self.add(self.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_LE, relational_lhs_node, relational_rhs_node);
            continue;
        }
        if (self.isCurrentTokenEqualTo(">")) {
            const relational_rhs_node = self.add(self.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_GT, relational_lhs_node, relational_rhs_node);
            continue;
        }
        if (self.isCurrentTokenEqualTo(">=")) {
            const relational_rhs_node = self.add(self.nextToken());
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
        if (self.isCurrentTokenEqualTo("+")) {
            const rhs_node = self.mul(self.nextToken());
            next_lhs_node = self.nodes.binaryExpression(.NK_ADD, next_lhs_node, rhs_node);
            continue;
        }

        if (self.isCurrentTokenEqualTo("-")) {
            const rhs_node = self.mul(self.nextToken());
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
        if (self.isCurrentTokenEqualTo("*")) {
            const rhs_node = self.unary(self.nextToken());
            next_lhs_node = self.nodes.binaryExpression(.NK_MUL, next_lhs_node, rhs_node);
            continue;
        }

        if (self.isCurrentTokenEqualTo("/")) {
            const rhs_node = self.unary(self.nextToken());
            next_lhs_node = self.nodes.binaryExpression(.NK_DIV, next_lhs_node, rhs_node);
            continue;
        }

        return next_lhs_node;
    }
}

// unary = ('+'|'-') unary | primary
fn unary(self: *Parser, token: *const Token) *const AstNode {
    if (self.isCurrentTokenEqualTo("+")) {
        return self.unary(self.nextToken());
    }

    if (self.isCurrentTokenEqualTo("-")) {
        return self.nodes.unaryExpression(.NK_NEG, self.unary(self.nextToken()));
    }
    _ = token;

    return self.primary(self.currentToken()) catch |err| switch (err) {
        error.InvalidExpression => {
            self.reportParserError("{s} :Invalid primary expression", .{@errorName(err)});
        },
        error.TerminalMismatch => {
            self.reportParserError("{s} : Terminals ( must end with a corresponding )", .{@errorName(err)});
        },
    };
}

// primary = "(" expr ")" | NUM
fn primary(self: *Parser, token: *const Token) !*const AstNode {
    if (self.isCurrentTokenEqualTo("(")) {
        const expr_node = self.expr(self.nextToken());
        if (self.expectCurrentTokenToMatch(")")) {} else {
            self.reportParserError("expected token to be ) but found {s}", .{self.currentToken().lexeme});
            return error.TerminalMismatch;
        }
        return expr_node;
    }
    if (token.kind == .TK_NUM) {
        const num_node = self.nodes.number(token.value);
        _ = self.nextToken();
        return num_node;
    }
    self.reportParserError("Expected an expression , terminal  or number", .{});
    return error.InvalidExpression;
}

pub fn reportParserError(self: *const Parser, comptime msg: []const u8, args: anytype) noreturn {
    const token = self.currentToken();
    std.log.err("Invalid Parse Token '{s}' in '{s}' at {d}", .{
        token.lexeme,
        self.tokenizer.stream,
        token.location,
    });
    const location_offset = 34;
    const token_location = token.location + token.lexeme.len + location_offset;
    //add empty spaces till the character where the error starts
    std.debug.print("{[spaces]s:>[width]}", .{ .spaces = " ", .width = token_location });
    const format_msg = "^ " ++ msg ++ "\n";
    std.debug.print(format_msg, args);
    std.process.exit(3);
}
// Consumes the current token if it matches `operand`.
fn isCurrentTokenEqualTo(self: *const Parser, terminal: []const u8) bool {
    const token = self.currentToken();
    return std.mem.eql(u8, token.lexeme, terminal);
}

//look at current token
pub fn currentToken(self: *const Parser) *const Token {
    return &self.tokenizer.tokens.items[self.items_position];
}

//consume next token in the input stream
fn nextToken(self: *Parser) *const Token {
    self.items_position += 1;
    return &self.tokenizer.tokens.items[self.items_position];
}

//look at the previous token usually useful when dealing with errors
pub fn previousToken(self: *const Parser) *const Token {
    if (self.items_position - 1 < 0) {
        return &self.tokenizer.tokens.items[self.items_position - 1];
    }
    return &self.tokenizer.tokens.items[0];
}

///Ensure that the current terminal token matches the peek
///and move to the next token is it indeed matches
fn expectCurrentTokenToMatch(self: *Parser, terminal: []const u8) bool {
    if (self.isCurrentTokenEqualTo(terminal)) {
        _ = self.nextToken();
        return true;
    }
    return false;
}
