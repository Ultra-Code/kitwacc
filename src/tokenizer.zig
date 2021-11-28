const std = @import("std");

const TokenKind = enum {
    TK_PUNCT, // Punctuators
    TK_NUM, // Numeric literals
    TK_EOF, // End-of-file markers
};

const Token = struct {
    kind: TokenKind, // Token kind
    value: i64, // If kind is TK_NUM, its value
    lexeme: []const u8, // Token lexeme
    lenght: usize, // Token length
};

pub const Tokenizer = struct {
    tokens: std.ArrayList(Token), // Next token
    //The -1 sentinal represent the end or that no element is requested
    items_position: usize,

    const Error = error{
        TerminalMismatch,
        TokenNotANumber,
        InvalidToken,
    };

    pub fn init(allocator: *std.mem.Allocator) Tokenizer {
        return Tokenizer{ .tokens = std.ArrayList(Token).init(allocator), .items_position = 0 };
    }

    // Create a new token.
    fn addToken(self: *Tokenizer, token: *const Token) !void {
        try self.tokens.append(.{ .kind = token.kind, .value = token.value, .lexeme = token.lexeme, .lenght = token.lexeme.len });
    }

    // Tokenize `peek` and returns new tokens.
    pub fn tokenize(self: *Tokenizer, peek: []const u8) !*const Token {
        //copy peek so that we can modify since parameters are const by default in zig
        var current_peek = peek;
        var index: usize = 0;
        // indicator that the slice has been resized and index needs to point
        // to the begining of the slice
        var reset_index: bool = false;
        //condition to prevent out of bound index but allow procession of last
        //token
        while (index < current_peek.len) : (index = if (reset_index) reset_index: {
            index = 0;
            reset_index = false;
            break :reset_index index;
        } else increment_index: {
            index += 1;
            break :increment_index index;
        }) {
            var value = current_peek[index];
            // Numeric literal
            if (std.ascii.isDigit(value)) {
                const ascii_offset = '0';

                //since value is a digit we need to look pass the current index
                //for addition digits in the stream
                var digit_index = index + 1;
                // convert ascii char to number
                var digits: u32 = value - ascii_offset;

                //look from digit_index for additional digits
                for (current_peek[digit_index..]) |digit| {
                    if (std.ascii.isDigit(digit)) {
                        //convert multiple number characters into a single
                        //numeric value
                        digits = digits * 10 + (digit - ascii_offset);
                        digit_index += 1;

                        //if at end of stream
                        if (current_peek.len == 1) {
                            current_peek = "";
                            std.log.info("peek is '_{s}_'", .{current_peek});
                            break;
                        }
                        //current_peek should contain slice we haven't
                        //processed yet
                        current_peek = current_peek[digit_index..];
                        reset_index = true;
                        continue;
                    }
                    break;
                }
                try self.addToken(&Token{ .kind = .TK_NUM, .value = digits, .lexeme = &[_]u8{value}, .lenght = 1 });
                continue;
            }

            // Skip whitespace characters.
            if (std.ascii.isSpace(value)) {
                continue;
            }

            // Operator
            if (value == '+') {
                try self.addToken(&Token{ .kind = .TK_PUNCT, .value = value, .lexeme = "+", .lenght = 1 });
                continue;
            }

            if (value == '-') {
                try self.addToken(&Token{ .kind = .TK_PUNCT, .value = value, .lexeme = "-", .lenght = 1 });
                continue;
            }

            if (std.mem.eql(u8, current_peek, "")) {
                continue;
            }

            std.log.err("Invalid Token '{c}' in peek", .{value});
            return error.InvalidToken;
        }

        try self.addToken(&Token{ .kind = .TK_EOF, .value = 0, .lexeme = "EOF", .lenght = 0 });
        return currentToken(self);
    }

    // Consumes the current token if it matches `operand`.
    fn equal(token: *const Token, terminal: []const u8) bool {
        return std.mem.eql(u8, token.lexeme, terminal);
    }

    //look at current token
    fn currentToken(self: *const Tokenizer) *const Token {
        return &self.tokens.items[self.items_position];
    }

    //consume next token in the input stream
    pub fn nextToken(self: *Tokenizer) *const Token {
        self.items_position += 1;
        return &self.tokens.items[self.items_position];
    }

    // Ensure that the current terminal token matches the peek
    pub fn match(token: *const Token, terminal: []const u8) bool {
        return equal(token, terminal);
    }

    pub fn getNumber(self: *const Tokenizer) Error!i32 {
        if (currentToken(self).kind == .TK_NUM) {
            return currentToken(self).value;
        } else {
            return error.TokenNotANumber;
        }
    }
};
