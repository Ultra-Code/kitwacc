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
    location: usize, // location of Token in input stream
};

pub fn reportError(peek: []const u8, comptime msg: []const u8, token: *const Token) void {
    std.log.err("Invalid Token '{c}' in '{s}' at {d}", .{
        @intCast(u8, token.value),
        peek,
        token.location,
    });
    const location_offset = 29;
    const token_location = token.location + location_offset;
    const stream_width = token_location + msg.len;
    //add empty spaces till the character where the error starts
    std.debug.print("{[msg]s:>[width]}\n", .{ .msg = msg, .width = stream_width });
    std.process.exit(1);
}

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
        try self.tokens.append(.{
            .kind = token.kind,
            .value = token.value,
            .lexeme = token.lexeme,
            .lenght = token.lenght,
            .location = token.location,
        });
    }

    // Tokenize `peek` and returns new tokens.
    pub fn tokenize(self: *Tokenizer, peek: []const u8) !*const Token {
        var index: usize = 0;
        //condition to prevent out of bound index but allow procession of last token
        while (index < peek.len) : (index += 1) {
            const value = peek[index];
            // Numeric literal
            if (std.ascii.isDigit(value)) {
                const ascii_offset = '0';
                const current_index = index;
                //since value is a digit we need to look pass the current index for addition digits in the stream
                var digit_index = current_index + 1;
                // convert ascii char to number
                var digits: u32 = value - ascii_offset;

                //look from digit_index for additional digits
                for (peek[digit_index..]) |digit| {
                    if (std.ascii.isDigit(digit)) {
                        //convert multiple number characters into a single numeric value
                        digits = digits * 10 + (digit - ascii_offset);
                        digit_index += 1;
                        continue;
                    }
                    // digit is not a digit
                    break;
                }
                //-1 because our anticipated next digit token wasn't a digit
                const next_token_index = digit_index - 1;
                index = next_token_index;
                try self.addToken(&Token{
                    .kind = .TK_NUM,
                    .value = digits,
                    .lexeme = &[_]u8{value},
                    .lenght = (digit_index - current_index),
                    .location = current_index,
                });
                continue;
            }

            // Skip whitespace characters.
            if (std.ascii.isSpace(value)) {
                continue;
            }

            // Operator
            if (value == '+') {
                try self.addToken(&Token{
                    .kind = .TK_PUNCT,
                    .value = value,
                    .lexeme = "+",
                    .lenght = 1,
                    .location = index,
                });
                continue;
            }

            if (value == '-') {
                try self.addToken(&Token{
                    .kind = .TK_PUNCT,
                    .value = value,
                    .lexeme = "-",
                    .lenght = 1,
                    .location = index,
                });
                continue;
            }

            try self.addToken(&Token{
                .kind = .TK_EOF,
                .value = value,
                .lexeme = "INVALID TOKEN",
                .lenght = 0,
                .location = index,
            });
            const error_token = self.tokens.items[self.tokens.items.len - 1];
            //reportError(peek, "Invalid Token {c} in {s} at {d}", .{ error_token.value, peek, error_token.location });
            reportError(peek, "^ expected number", &error_token);
            return error.InvalidToken;
        }

        try self.addToken(&Token{
            .kind = .TK_EOF,
            .value = 0,
            .lexeme = "EOF",
            .lenght = 0,
            .location = index,
        });
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
