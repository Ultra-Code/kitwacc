const std = @import("std");
const Tokenizer = @import("tokenizer.zig");
const Token = Tokenizer.Token;
const OOMhandler = Tokenizer.OOMhandler;
const algods = @import("algods");

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
    NK_ASSIGN, // =
    NK_VAR, // Variable
    NK_RETURN, // return statement
    NK_BLOCK, // { block }
};

// Local variable
pub const Variable = struct {
    name: []const u8, // Variable name
    rbp_offset: usize, // Offset from RBP
};

//I use a SinglyCircularList because appending doesn't require node traversal
pub const ExprList = algods.linked_list.SinglyCircularList(*const AstTree.AstNode);
pub const VarList = algods.linked_list.SinglyCircularList(*const Variable);

pub const Function = struct {
    body: ExprList,
    local_variables: VarList,
    stack_size: usize,
};

pub const AstTree = struct {

    // AST node type
    pub const AstNode = struct {
        kind: AstNodeKind, // AstNode kind
        lhs: ?*const AstNode = null, // Left-hand side
        rhs: ?*const AstNode = null, // Right-hand side
        value: Value,
        pub const Value = union {
            number: u64, // value of integer if kind == NK_NUM
            identifier: *const Variable, // Used if node is an Identifier Token .ie kind == ND_VAR
            expression: ExprList,
        };
    };

    allocator: std.mem.Allocator,
    // All local variable instances created during parsing are accumulated to this list.
    local_variables: VarList,
    local_variables_rbp_offset: usize = 0,

    pub fn init(allocator: std.mem.Allocator) AstTree {
        return .{ .allocator = allocator, .local_variables = VarList.init(allocator) };
    }

    fn createAstNode(self: *AstTree, kind: AstNodeKind) *AstNode {
        var new_ast_node = self.allocator.create(AstNode) catch OOMhandler();
        new_ast_node.kind = kind;
        return new_ast_node;
    }

    pub fn binaryExpression(self: *AstTree, kind: AstNodeKind, lhs: *const AstNode, rhs: *const AstNode) *const AstNode {
        var binaray_node = self.createAstNode(kind);
        binaray_node.rhs = rhs;
        binaray_node.lhs = lhs;
        binaray_node.value = undefined;
        return binaray_node;
    }

    pub fn number(self: *AstTree, value: u64) *const AstNode {
        var num_terminal_node = self.createAstNode(.NK_NUM);
        num_terminal_node.value = AstNode.Value{ .number = value };
        return num_terminal_node;
    }

    pub fn unaryExpression(self: *AstTree, kind: AstNodeKind, unary_expr: *const AstNode) *const AstNode {
        var unary_node = self.createAstNode(kind);
        unary_node.rhs = unary_expr;
        unary_node.lhs = undefined;
        unary_node.value = undefined;
        return unary_node;
    }

    pub fn blockExpression(self: *AstTree, kind: AstNodeKind, list_of_expr: ExprList) *const AstNode {
        var compound_node = self.createAstNode(kind);
        compound_node.rhs = undefined;
        compound_node.lhs = undefined;
        compound_node.value = AstNode.Value{ .expression = list_of_expr };
        return compound_node;
    }

    // Assign offsets to local variables.
    fn nextlvarOffsets(self: *AstTree) usize {
        const byte_size = 8;
        const offset = new_offset: {
            self.local_variables_rbp_offset += byte_size;
            break :new_offset self.local_variables_rbp_offset;
        };
        return offset;
    }

    fn findVariable(self: *const AstTree, variable_name: []const u8) ?*const Variable {
        // Traverse forwards.
        var it = self.local_variables.iterator();
        while (it.next()) |variable| {
            if (std.mem.eql(u8, variable_name, variable.data.name)) {
                return variable.data;
            }
        }
        return null;
    }

    pub fn createVariable(self: *AstTree, name: []const u8) *Variable {
        var new_variable_identifier = self.allocator.create(Variable) catch OOMhandler();
        new_variable_identifier.name = name;
        new_variable_identifier.rbp_offset = offset: {
            if (self.findVariable(name)) |variable_exist| {
                break :offset variable_exist.rbp_offset;
            } else {
                break :offset self.nextlvarOffsets();
            }
        };
        return new_variable_identifier;
    }

    pub fn createListNode(self: *AstTree, comptime T: type, data: anytype) *T {
        var expr_list_node = self.allocator.create(T) catch OOMhandler();
        expr_list_node.data = data;
        return expr_list_node;
    }

    pub fn variableAssignment(self: *AstTree, variable: *const Variable) *const AstNode {
        var variable_node = self.createAstNode(.NK_VAR);
        variable_node.value = AstNode.Value{ .identifier = variable };
        variable_node.rhs = undefined;
        variable_node.lhs = undefined;
        return variable_node;
    }
};

nodes: AstTree,
tokenizer: Tokenizer,
statements: ExprList,
items_position: usize = 0,

pub fn init(allocator: std.mem.Allocator, input_token_stream: []const u8) Parser {
    return .{
        .nodes = AstTree.init(allocator),
        .tokenizer = Tokenizer.init(allocator, input_token_stream),
        .statements = ExprList.init(allocator),
    };
}

pub fn tokenizeInput(self: *Parser) !*const Token {
    return try self.tokenizer.tokenize();
}
// Round up `num` to the nearest multiple of `alignment`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
fn alignTo(num: usize, alignment: usize) usize {
    // return (num + alignment - 1) / alignment * alignment;
    return std.mem.alignForward(num, alignment);
}

//program = stmt
pub fn parse(self: *Parser, token: *const Token) Function {
    // NOTE: This can be replaced by a singly linked list of AstNode
    if (self.expectCurrentTokenToMatch("{")) {
        self.statements = self.compoundStmt(token);
    } else {
        self.reportParserError("expected program to start with {{ but found {s}", .{self.currentToken().value.ident_name});
    }
    //After parsing the stream of tokens EOF must be the last token
    std.debug.assert(self.currentToken().kind == .TK_EOF);

    return .{
        .body = self.statements,
        .local_variables = self.nodes.local_variables,
        .stack_size = alignTo(self.nodes.local_variables_rbp_offset, 16),
    };
}
//compound-stmt = stmt* "}"
fn compoundStmt(self: *Parser, token: *const Token) ExprList {
    var compound_stmt = ExprList.init(self.nodes.allocator);
    while (!self.isCurrentTokenEqualTo("}")) {
        const statement = self.stmt(token);
        compound_stmt.append(statement) catch OOMhandler();
    }
    if (!self.expectCurrentTokenToMatch("}")) {
        self.reportParserError("expected block to end with }} but found {s}", .{self.currentToken().value.ident_name});
    }
    return compound_stmt;
}

// stmt = "return" expr ";" | "{" compound-stmt | expr_stmt
fn stmt(self: *Parser, token: *const Token) *const AstTree.AstNode {
    if (self.isCurrentTokenEqualTo("return")) {
        const return_statement = self.nodes.unaryExpression(.NK_RETURN, self.expr(self.nextToken()));
        if (!self.expectCurrentTokenToMatch(";")) {
            self.reportParserError("expected statement to end with ';' but found {s}", .{self.currentToken().value.ident_name});
        }
        return return_statement;
    }
    if (self.isCurrentTokenEqualTo("{")) {
        const compound_stmt = self.nodes.blockExpression(.NK_BLOCK, self.compoundStmt(self.nextToken()));
        return compound_stmt;
    }
    return self.expr_stmt(token);
}

// expr_stmt = expr ";"
fn expr_stmt(self: *Parser, token: *const Token) *const AstTree.AstNode {
    const expr_stmt_node = self.nodes.unaryExpression(.NK_EXPR_STMT, self.expr(token));
    if (!self.expectCurrentTokenToMatch(";")) {
        self.reportParserError("expected statement to end with ';' but found {s}", .{self.currentToken().value.ident_name});
    }
    return expr_stmt_node;
}

// expr = assign
fn expr(self: *Parser, token: *const Token) *const AstTree.AstNode {
    return self.assign(token);
}

//assign = equality ('=' assign)?
fn assign(self: *Parser, token: *const Token) *const AstTree.AstNode {
    const equality_lhs_node = self.equality(token);
    var assignment_tree_node = equality_lhs_node;
    if (self.isCurrentTokenEqualTo("=")) {
        const assignment_rhs_node = self.assign(self.nextToken());
        assignment_tree_node = self.nodes.binaryExpression(.NK_ASSIGN, equality_lhs_node, assignment_rhs_node);
    }
    return assignment_tree_node;
}

//equality = relational ("==" relational | "!=" relational)*
fn equality(self: *Parser, token: *const Token) *const AstTree.AstNode {
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
fn relational(self: *Parser, token: *const Token) *const AstTree.AstNode {
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
pub fn add(self: *Parser, token: *const Token) *const AstTree.AstNode {
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
fn mul(self: *Parser, token: *const Token) *const AstTree.AstNode {
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
fn unary(self: *Parser, token: *const Token) *const AstTree.AstNode {
    if (self.isCurrentTokenEqualTo("+")) {
        return self.unary(self.nextToken());
    }

    if (self.isCurrentTokenEqualTo("-")) {
        return self.nodes.unaryExpression(.NK_NEG, self.unary(self.nextToken()));
    }
    _ = token;

    return self.primary(self.currentToken());
}

// primary = "(" expr ")" | IDENTIFIER | NUM
fn primary(self: *Parser, token: *const Token) *const AstTree.AstNode {
    if (self.isCurrentTokenEqualTo("(")) {
        const expr_node = self.expr(self.nextToken());
        if (self.expectCurrentTokenToMatch(")")) {} else {
            self.reportParserError("expected token to be ) but found {}", .{self.currentToken().value});
            self.reportParserError("Terminal ( must end with a corresponding )", .{});
        }
        return expr_node;
    }
    if (token.kind == .TK_IDENT) {
        const variable = self.nodes.findVariable(token.value.ident_name) orelse new_variable: {
            const new_identifier = self.nodes.createVariable(token.value.ident_name);
            self.nodes.local_variables.append(new_identifier) catch OOMhandler();
            break :new_variable new_identifier;
        };
        const identifier_node = self.nodes.variableAssignment(variable);
        _ = self.nextToken();
        return identifier_node;
    }
    if (token.kind == .TK_NUM) {
        const num_node = self.nodes.number(token.value.num_value);
        _ = self.nextToken();
        return num_node;
    }
    self.reportParserError("Expected an ( expression ) , variable assignment or a number", .{});
}

pub fn reportParserError(self: *const Parser, comptime msg: []const u8, args: anytype) noreturn {
    const token = self.currentToken();
    const error_msg = "\nInvalid Parse Token '{[token_name]s}' in '{[token_stream]s}' at {[token_location]d}";
    const identifier_name = token.value.ident_name;
    std.log.err(error_msg, .{
        .token_name = identifier_name,
        .token_stream = self.tokenizer.stream,
        .token_location = token.location,
    });
    const location_offset = 27;
    const token_location = location_offset + identifier_name.len + token.location;
    //add empty spaces till the character where the error starts
    std.debug.print("{[spaces]s:>[width]}", .{ .spaces = " ", .width = token_location });
    const format_msg = "^ " ++ msg ++ "\n";
    std.debug.print(format_msg, args);
    std.process.exit(3);
}
// Consumes the current token if it matches `operand`.
fn isCurrentTokenEqualTo(self: *const Parser, terminal: []const u8) bool {
    const token = self.currentToken();
    if (token.kind != .TK_NUM) {
        return std.mem.eql(u8, token.value.ident_name, terminal);
    }
    const digit = std.fmt.charToDigit(terminal[0], 10) catch undefined;
    if (digit == token.value.num_value) {
        return true;
    } else {
        return false;
    }
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
