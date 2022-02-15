//!An operator is left associative if it starts with a non-terminal on the left hand side
//!.ie add = mul ("+" mul | "-" mul) or  relational = add ("<" add | "<=" add | ">" add | ">=" add)*
//!And if it starts with a terminal on the right followed by a non terminal
//!or is right recursive then it is right associative eg. assign = equality ('=' assign)? or unary = ('+'|'-'|'*'|'&') unary | primary
//!The precedence of the operator is encoded by the depth of the recursive call to a non-terminal fn before getting to the
//!operator which is the terminal. Meaning since primary in very down the stack it has the highest precedence and this
//!reduces as you go up the stack to assign = equality ('=' assign)? and higher
const std = @import("std");
const Tokenizer = @import("tokenizer.zig");
pub const Token = Tokenizer.Token;
const TokenIterator = Tokenizer.TokenList.Iterator;
const OOMhandler = Tokenizer.OOMhandler;
const algods = @import("algods");
const compiler = @import("main.zig");
const types = @import("type.zig");
const Type = types.Type;

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
    NK_EXPR_STMT, // Expression statement .ie stmt separated by commas
    NK_ASSIGN, // =
    NK_VAR, // Variable
    NK_RETURN, // return statement
    NK_BLOCK, // { block }
    NK_IF, // if
    NK_LOOP, // for or while
    NK_ADDR, // unary & operator .ie address operator
    NK_DEREF, //unary * operator .ie pointer dereference
};

// Local variable
pub const Variable = struct {
    name: []const u8, // Variable name
    rbp_offset: usize, // Offset from RBP
};

//I use a SinglyCircularList because appending doesn't require node traversal
pub const ExprList = algods.linked_list.SinglyCircularList(*AstTree.AstNode);
pub const VarList = algods.linked_list.SinglyCircularList(Variable);

pub const Function = struct {
    body: ExprList,
    local_variables: VarList,
    stack_size: usize,
};

//if statement
const Conditonal = struct {
    if_expr: *AstTree.AstNode,
    then_branch: *AstTree.AstNode,
    else_branch: ?*AstTree.AstNode = null,
};

//for or while loop
const Loop = struct {
    init: ?*AstTree.AstNode = null,
    condition: ?*AstTree.AstNode = null,
    increment: ?*AstTree.AstNode = null,
    body: *AstTree.AstNode,
};

pub const AstTree = struct {

    // AST node type
    pub const AstNode = struct {
        kind: AstNodeKind, // AstNode kind
        lhs: ?*AstNode = null, // Left-hand side
        rhs: ?*AstNode = null, // Right-hand side
        token: Token, //A representative Token of the Node to improve error messages
        type: ?Type = null, //Type, e.g. int or pointer to int
        value: Value = .{ .no_value = true },
        const Value = union(enum) {
            number: u64, // value of integer if kind == NK_NUM
            identifier: Variable, // Used if node is an Identifier Token .ie kind == ND_VAR
            block: ExprList, // Block { ... }
            if_statement: Conditonal, //if statement
            loop: Loop, //for or while loop
            no_value: bool,
        };
    };

    allocator: std.mem.Allocator,
    // All local variable instances created during parsing are accumulated to this list.
    local_variables: VarList,
    local_variables_rbp_offset: usize = 0,

    pub fn init(allocator: std.mem.Allocator) AstTree {
        return .{ .allocator = allocator, .local_variables = VarList.init(allocator) };
    }

    fn createAstNode(self: *AstTree, kind: AstNodeKind, token: Token) *AstNode {
        var new_ast_node = self.allocator.create(AstNode) catch OOMhandler();
        //default initialize default fileds from undefined values
        new_ast_node.lhs = null;
        new_ast_node.rhs = null;
        new_ast_node.type = null;
        new_ast_node.value = AstNode.Value{ .no_value = true };
        new_ast_node.kind = kind;
        new_ast_node.token = token;
        return new_ast_node;
    }
    // In C, `+` operator is overloaded to perform the pointer arithmetic.
    // If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
    // so that p+n points to the location n elements (not bytes) ahead of p.
    // In other words, we need to scale an integer value before adding to a
    // pointer value. This function takes care of the scaling.
    fn addExpression(self: *AstTree, lhs: *AstNode, rhs: *AstNode, token: Token) *AstNode {
        types.addType(lhs);
        types.addType(rhs);
        //num + num
        if (lhs.type.?.isInt() and rhs.type.?.isInt()) {
            return self.binaryExpression(.NK_ADD, lhs, rhs, token);
        }
        // Canonicalize `num + ptr` to `ptr + num`.
        if (!lhs.type.?.isPtr() and rhs.type.?.isPtr()) {
            std.mem.swap(AstNode, lhs, rhs);
        }

        if (lhs.type.?.isPtr() and rhs.type.?.isInt()) {
            //ptr + num
            //number is scaled by 8 .ie assuming 64bit
            const new_rhs = self.binaryExpression(.NK_MUL, rhs, self.number(8, token), token);
            //.NK_SUB because we need to subtract the scaled number for the stack pointer
            return self.binaryExpression(.NK_SUB, lhs, new_rhs, token);
        }

        //&num + &num is not allowed
        // if (lhs.type.?.isPtr() and rhs.type.?.isPtr())
        reportParserError(token, "invalid operands. expected &lhs + num but found &lhs + &num", .{});
    }

    // Like `+`, `-` is overloaded for the pointer type.
    fn subExpression(self: *AstTree, lhs: *AstNode, rhs: *AstNode, token: Token) *AstNode {
        types.addType(lhs);
        types.addType(rhs);

        //num - num
        if (lhs.type.?.isInt() and rhs.type.?.isInt()) {
            return self.binaryExpression(.NK_SUB, lhs, rhs, token);
        }

        //ptr - num
        if (lhs.type.?.isPtr() and rhs.type.?.isInt()) {
            var scaled_rhs = self.binaryExpression(.NK_MUL, rhs, self.number(8, token), token);
            types.addType(scaled_rhs);
            var current_lhs = self.binaryExpression(.NK_ADD, lhs, scaled_rhs, token);
            current_lhs.type = lhs.type;
            return current_lhs;
        }

        // ptr - ptr, which returns how many elements are between the two.
        if (lhs.type.?.isPtr() and rhs.type.?.isPtr()) {
            //sub rhs from lhs because rhs is usually at a higher address than lhs
            //so we don't get negative values
            var bytes_btw_lhs_rhs = self.binaryExpression(.NK_SUB, rhs, lhs, token);
            bytes_btw_lhs_rhs.type = types.int_ty;
            return self.binaryExpression(.NK_DIV, bytes_btw_lhs_rhs, self.number(8, token), token);
        }

        reportParserError(token, "invalid subtraction  operands", .{});
    }

    fn binaryExpression(self: *AstTree, kind: AstNodeKind, lhs: *AstNode, rhs: *AstNode, token: Token) *AstNode {
        var binaray_node = self.createAstNode(kind, token);
        binaray_node.rhs = rhs;
        binaray_node.lhs = lhs;
        return binaray_node;
    }

    fn number(self: *AstTree, value: u64, token: Token) *AstNode {
        var num_terminal_node = self.createAstNode(.NK_NUM, token);
        num_terminal_node.value = AstNode.Value{ .number = value };
        return num_terminal_node;
    }

    fn unaryExpression(self: *AstTree, kind: AstNodeKind, unary_expr: *AstNode, token: Token) *AstNode {
        var unary_node = self.createAstNode(kind, token);
        unary_node.rhs = unary_expr;
        return unary_node;
    }

    fn blockExpression(self: *AstTree, kind: AstNodeKind, expression_list: ExprList, token: Token) *AstNode {
        var compound_node = self.createAstNode(kind, token);
        compound_node.value = AstNode.Value{ .block = expression_list };
        return compound_node;
    }

    fn conditionExpression(self: *AstTree, kind: AstNodeKind, conditional_expression: Conditonal, token: Token) *AstNode {
        var if_statement = self.createAstNode(kind, token);
        if_statement.value = AstNode.Value{ .if_statement = conditional_expression };
        return if_statement;
    }

    fn loopExpression(self: *AstTree, kind: AstNodeKind, loop: Loop, token: Token) *AstNode {
        var loop_statment = self.createAstNode(kind, token);
        loop_statment.value = AstNode.Value{ .loop = loop };
        return loop_statment;
    }

    fn nullBlock(self: *AstTree, token: Token) *AstNode {
        var null_block = self.createAstNode(.NK_BLOCK, token);
        null_block.value = AstNode.Value{ .block = ExprList.init(self.allocator) };
        return null_block;
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

    fn findVariable(self: AstTree, variable_name: []const u8) ?Variable {
        // Traverse forwards.
        var it = self.local_variables.iterator();
        while (it.next()) |variable| {
            if (std.mem.eql(u8, variable_name, variable.name)) {
                return variable;
            }
        }
        return null;
    }

    fn createVariable(self: *AstTree, name: []const u8) Variable {
        return Variable{ .name = name, .rbp_offset = offset: {
            if (self.findVariable(name)) |variable_exist| {
                break :offset variable_exist.rbp_offset;
            } else {
                break :offset self.nextlvarOffsets();
            }
        } };
    }

    fn variableAssignment(self: *AstTree, variable: Variable, token: Token) *AstNode {
        var variable_node = self.createAstNode(.NK_VAR, token);
        variable_node.value = AstNode.Value{ .identifier = variable };
        return variable_node;
    }
};

nodes: AstTree,
tokenizer: TokenIterator,
statements: ExprList,

pub fn init(allocator: std.mem.Allocator, tokens: TokenIterator) Parser {
    return .{
        .nodes = AstTree.init(allocator),
        .tokenizer = tokens,
        .statements = ExprList.init(allocator),
    };
}

// Round up `num` to the nearest multiple of `alignment`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
fn alignTo(num: usize, alignment: usize) usize {
    // return (num + alignment - 1) / alignment * alignment;
    return std.mem.alignForward(num, alignment);
}

//program = stmt
pub fn parse(self: *Parser) Function {
    //we call nextToken to move from end to first token
    if (self.expectToken(self.nextToken(), "{")) {
        self.statements = self.compoundStmt(self.currentToken());
    } else {
        reportParserError(self.currentToken(), "expected program to start with {{ but found {s}", .{self.currentToken().value.ident_name});
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
fn compoundStmt(self: *Parser, token: Token) ExprList {
    var compound_stmt = ExprList.init(self.nodes.allocator);
    _ = token;
    while (!isEqual(self.currentToken(), "}")) {
        const statement = self.stmt(self.currentToken());
        compound_stmt.append(statement) catch OOMhandler();
    }
    if (!self.expectToken(self.currentToken(), "}")) {
        reportParserError(self.currentToken(), "expected block to end with }} but found {s}", .{self.currentToken().value.ident_name});
    }
    return compound_stmt;
}

/// stmt = "return" expr ";" | "{" compound-stmt |
///       | "if" "(" expr ")"  stmt  ("else" stmt )?
///       | "for" "(" expr_stmt expr? ";" expr ")" stmt
///       | "while" "(" expr ")" stmt
///       | expr_stmt
fn stmt(self: *Parser, token: Token) *AstTree.AstNode {
    const start = token;
    if (isEqual(token, "return")) {
        const return_statement = self.nodes.unaryExpression(.NK_RETURN, self.expr(self.nextToken()), start);
        if (!self.expectToken(self.currentToken(), ";")) {
            reportParserError(self.currentToken(), "expected statement to end with ';' but found {s}", .{self.currentToken().value.ident_name});
        }
        return return_statement;
    }
    if (isEqual(token, "{")) {
        const compound_stmt = self.nodes.blockExpression(.NK_BLOCK, self.compoundStmt(self.nextToken()), start);
        return compound_stmt;
    }
    if (isEqual(token, "if")) {
        if (self.expectToken(self.nextToken(), "(")) {
            const if_expr = self.expr(self.currentToken());
            if (self.expectToken(self.currentToken(), ")")) {
                const then_branch = self.stmt(self.currentToken());
                if (isEqual(self.currentToken(), "else")) {
                    const else_branch = self.stmt(self.nextToken());
                    const if_statement = self.nodes.conditionExpression(.NK_IF, .{ .if_expr = if_expr, .then_branch = then_branch, .else_branch = else_branch }, start);
                    return if_statement;
                } else {
                    const if_statement = self.nodes.conditionExpression(.NK_IF, .{ .if_expr = if_expr, .then_branch = then_branch }, start);
                    return if_statement;
                }
            } else {
                reportParserError(self.currentToken(), "expected ) after  if ( expr  but found {s}", .{self.currentToken().value.ident_name});
            }
        } else {
            reportParserError(self.currentToken(), "expected ( after if keyword but found {s}", .{self.currentToken().value.ident_name});
        }
    }

    if (isEqual(token, "for")) {
        var for_loop: Loop = Loop{ .init = undefined, .body = undefined };

        if (self.expectToken(self.nextToken(), "(")) {
            const init_statment = self.exprStmt(self.currentToken());
            for_loop.init = init_statment;
        } else {
            reportParserError(self.currentToken(), "expected '(' after 'for'  like 'for ('  but found 'for {s}'", .{self.currentToken().value.ident_name});
        }

        //if not for(init;;) .ie for (init;expr;)
        if (!isEqual(self.currentToken(), ";")) {
            const conditon_expr = self.expr(self.currentToken());
            for_loop.condition = conditon_expr;
        }
        if (!self.expectToken(self.currentToken(), ";")) {
            reportParserError(self.currentToken(), "expected ';' after 'condition' like 'for (init; condition ;'  but found 'for (init; condition {s}'", .{self.currentToken().value.ident_name});
        }

        if (!isEqual(self.currentToken(), ")")) {
            const increment_expr = self.expr(self.currentToken());
            for_loop.increment = increment_expr;
        }
        if (!self.expectToken(self.currentToken(), ")")) {
            reportParserError(self.currentToken(), "expected ')' after 'inc' like 'for (init; condition ; inc )'  but found 'for (init; condition ; inc {s}'", .{self.currentToken().value.ident_name});
        }

        const loop_body = self.stmt(self.currentToken());
        for_loop.body = loop_body;

        return self.nodes.loopExpression(.NK_LOOP, for_loop, start);
    }

    if (isEqual(token, "while")) {
        var while_loop: Loop = Loop{ .condition = undefined, .body = undefined };

        if (self.expectToken(self.nextToken(), "(")) {
            const conditon_expr = self.expr(self.currentToken());
            while_loop.condition = conditon_expr;
        } else {
            reportParserError(self.currentToken(), "expected '(' after 'while' like 'while (' but found 'while {s}'", .{self.currentToken().value.ident_name});
        }

        if (!self.expectToken(self.currentToken(), ")")) {
            reportParserError(self.currentToken(), "expected ')' after 'condition' like 'while ( condition )' but found 'while (condition {s}'", .{self.currentToken().value.ident_name});
        }

        const while_body = self.stmt(self.currentToken());
        while_loop.body = while_body;

        return self.nodes.loopExpression(.NK_LOOP, while_loop, start);
    }

    return self.exprStmt(token);
}

// expr_stmt = expr? ";"
fn exprStmt(self: *Parser, token: Token) *AstTree.AstNode {
    const start = token;
    //.ie there is no expr
    //null block are used in for (;;){stmt}
    if (isEqual(token, ";")) {
        //skip the current Token
        _ = self.nextToken();
        return self.nodes.nullBlock(start);
    }
    const expr_stmt_node = self.nodes.unaryExpression(.NK_EXPR_STMT, self.expr(token), start);
    if (!self.expectToken(self.currentToken(), ";")) {
        reportParserError(self.currentToken(), "expected statement to end with ';' but found {s}", .{self.currentToken().value.ident_name});
    }
    return expr_stmt_node;
}

// expr = assign
fn expr(self: *Parser, token: Token) *AstTree.AstNode {
    return self.assign(token);
}

//assign = equality ('=' assign)?
fn assign(self: *Parser, token: Token) *AstTree.AstNode {
    const start = token;
    const equality_lhs_node = self.equality(token);
    var assignment_tree_node = equality_lhs_node;
    if (isEqual(self.currentToken(), "=")) {
        const assignment_rhs_node = self.assign(self.nextToken());
        assignment_tree_node = self.nodes.binaryExpression(.NK_ASSIGN, equality_lhs_node, assignment_rhs_node, start);
    }
    return assignment_tree_node;
}

//equality = relational ("==" relational | "!=" relational)*
fn equality(self: *Parser, token: Token) *AstTree.AstNode {
    const start = token;
    const equality_lhs_node = self.relational(token);
    var equality_tree_node = equality_lhs_node;

    while (true) {
        if (isEqual(self.currentToken(), "==")) {
            const equality_rhs_node = self.relational(self.nextToken());
            equality_tree_node = self.nodes.binaryExpression(.NK_EQ, equality_lhs_node, equality_rhs_node, start);
            continue;
        }
        if (isEqual(self.currentToken(), "!=")) {
            const equality_rhs_node = self.relational(self.nextToken());
            equality_tree_node = self.nodes.binaryExpression(.NK_NE, equality_lhs_node, equality_rhs_node, start);
            continue;
        }
        return equality_tree_node;
    }
}

//relational = add ("<" add | "<=" add | ">" add | ">=" add)*
fn relational(self: *Parser, token: Token) *AstTree.AstNode {
    const start = token;
    const relational_lhs_node = self.add(token);
    var relational_tree_node = relational_lhs_node;

    while (true) {
        if (isEqual(self.currentToken(), "<")) {
            const relational_rhs_node = self.add(self.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_LT, relational_lhs_node, relational_rhs_node, start);
            continue;
        }
        if (isEqual(self.currentToken(), "<=")) {
            const relational_rhs_node = self.add(self.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_LE, relational_lhs_node, relational_rhs_node, start);
            continue;
        }
        if (isEqual(self.currentToken(), ">")) {
            const relational_rhs_node = self.add(self.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_GT, relational_lhs_node, relational_rhs_node, start);
            continue;
        }
        if (isEqual(self.currentToken(), ">=")) {
            const relational_rhs_node = self.add(self.nextToken());
            relational_tree_node = self.nodes.binaryExpression(.NK_GE, relational_lhs_node, relational_rhs_node, start);
            continue;
        }
        return relational_tree_node;
    }
}

/// add = mul ("+" mul | "-" mul)
fn add(self: *Parser, token: Token) *AstTree.AstNode {
    const start = token;
    const lhs_node = self.mul(token);
    var next_lhs_node = lhs_node;
    while (true) {
        if (isEqual(self.currentToken(), "+")) {
            const rhs_node = self.mul(self.nextToken());
            next_lhs_node = self.nodes.addExpression(next_lhs_node, rhs_node, start);
            continue;
        }

        if (isEqual(self.currentToken(), "-")) {
            const rhs_node = self.mul(self.nextToken());
            next_lhs_node = self.nodes.subExpression(next_lhs_node, rhs_node, start);
            continue;
        }
        return next_lhs_node;
    }
}

///  mul = unary ("*" unary | "/" unary)*
fn mul(self: *Parser, token: Token) *AstTree.AstNode {
    const start = token;
    const lhs_node = self.unary(token);
    var next_lhs_node = lhs_node;
    while (true) {
        if (isEqual(self.currentToken(), "*")) {
            const rhs_node = self.unary(self.nextToken());
            next_lhs_node = self.nodes.binaryExpression(.NK_MUL, next_lhs_node, rhs_node, start);
            continue;
        }

        if (isEqual(self.currentToken(), "/")) {
            const rhs_node = self.unary(self.nextToken());
            next_lhs_node = self.nodes.binaryExpression(.NK_DIV, next_lhs_node, rhs_node, start);
            continue;
        }

        return next_lhs_node;
    }
}

/// unary = ('+'|'-'|'*'|'&') unary | primary
fn unary(self: *Parser, token: Token) *AstTree.AstNode {
    const start = token;
    if (isEqual(token, "+")) {
        return self.unary(self.nextToken());
    }

    if (isEqual(token, "-")) {
        return self.nodes.unaryExpression(.NK_NEG, self.unary(self.nextToken()), start);
    }

    if (isEqual(token, "*")) {
        return self.nodes.unaryExpression(.NK_DEREF, self.unary(self.nextToken()), start);
    }

    if (isEqual(token, "&")) {
        return self.nodes.unaryExpression(.NK_ADDR, self.unary(self.nextToken()), start);
    }

    return self.primary(token);
}

// primary = "(" expr ")" | IDENTIFIER | NUM
fn primary(self: *Parser, token: Token) *AstTree.AstNode {
    const start = token;
    if (isEqual(token, "(")) {
        const expr_node = self.expr(self.nextToken());
        if (!self.expectToken(self.currentToken(), ")")) {
            reportParserError(self.currentToken(), "expected ) after 'expr' like '( expr )' but found  '( expr {}'", .{self.currentToken().value});
        }
        return expr_node;
    }
    if (token.kind == .TK_IDENT) {
        const variable = self.nodes.findVariable(token.value.ident_name) orelse new_variable: {
            const new_identifier = self.nodes.createVariable(token.value.ident_name);
            self.nodes.local_variables.append(new_identifier) catch OOMhandler();
            break :new_variable new_identifier;
        };
        const identifier_node = self.nodes.variableAssignment(variable, start);
        _ = self.nextToken();
        return identifier_node;
    }
    if (token.kind == .TK_NUM) {
        const num_node = self.nodes.number(token.value.num_value, start);
        _ = self.nextToken();
        return num_node;
    }
    reportParserError(token, "Expected an ( expression ) , variable assignment or a number", .{});
}

fn reportParserError(token: Token, comptime msg: []const u8, args: anytype) noreturn {
    const error_msg = "\nInvalid Parse Token '{[token_name]s}' in '{[token_stream]s}' at {[token_location]d}";
    const identifier_name = token.value.ident_name;
    std.log.err(error_msg, .{
        .token_name = identifier_name,
        .token_stream = compiler.TOKEN_STREAM,
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

///check if current token matches terminal
fn isEqual(token: Token, terminal: []const u8) bool {
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

///consumes token if it matches terminal.
fn expectToken(self: *Parser, token: Token, terminal: []const u8) bool {
    if (token.kind != .TK_NUM) {
        const match = std.mem.eql(u8, token.value.ident_name, terminal);
        if (match) {
            _ = self.nextToken();
            return true;
        } else {
            return false;
        }
    }
    const digit = std.fmt.charToDigit(terminal[0], 10) catch undefined;
    if (digit == token.value.num_value) {
        _ = self.nextToken();
        return true;
    } else {
        return false;
    }
}

//look at current token
pub fn currentToken(self: *Parser) Token {
    return self.tokenizer.current();
}

//consume next token in the input stream
fn nextToken(self: *Parser) Token {
    return self.tokenizer.next().?;
}
