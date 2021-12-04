const std = @import("std");

const TokenKind = enum {
    TK_PUNCT, // Punctuators
    TK_NUM, // Numeric literals
    TK_EOF, // End-of-file markers
};

const Token = struct {
    kind: TokenKind, // Token kind
    value: u64, // If kind is TK_NUM, its value
    lexeme: []const u8, // Token lexeme
    lenght: usize, // Token length
    location: usize, // location of Token in input stream
};

pub const Tokenizer = struct {
    tokens: std.ArrayList(Token), // Next token
    //The -1 sentinal represent the end or that no element is requested
    items_position: usize,
    stream: []const u8,

    const Error = error{
        TerminalMismatch,
        TokenNotANumber,
        InvalidToken,
    };

    pub fn init(allocator: *std.mem.Allocator, input_stream: []const u8) Tokenizer {
        return Tokenizer{
            .tokens = std.ArrayList(Token).init(allocator),
            .items_position = 0,
            .stream = input_stream,
        };
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

    pub fn reportError(self: *const Tokenizer, comptime msg: []const u8, args: anytype) void {
        const token = self.currentToken();
        std.log.err("Invalid Token '{c}' in '{s}' at {d}", .{
            @intCast(u8, token.value),
            self.stream,
            token.location,
        });
        const location_offset = 29;
        const token_location = token.location + location_offset;
        //add empty spaces till the character where the error starts
        std.debug.print("{[spaces]s:>[width]}", .{ .spaces = " ", .width = token_location });
        const format_msg = "^ " ++ msg ++ "\n";
        std.debug.print(format_msg, args);
        std.process.exit(1);
    }

    // Tokenize `peek` and returns new tokens.
    pub fn tokenize(self: *Tokenizer) !*const Token {
        const peek = self.stream;
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
            //set current items_position to the last
            self.items_position = self.tokens.items.len - 1;
            self.reportError("expected number or + or -", .{});
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
        if (token.kind == .TK_NUM) {
            const digits = convertU8slicetoNum(terminal) catch |err| undefined_value: {
                std.log.err("{s}:cannot convert {s} to decimal digit", .{ @errorName(err), terminal });
                break :undefined_value undefined;
            };
            return if (token.value == digits) true else false;
        }
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
    pub fn match(self: *Tokenizer, token: *const Token, terminal: []const u8) ?*const Token {
        if (equal(token, terminal)) {
            return self.nextToken();
        }
        return null;
    }

    pub fn getNumber(self: *const Tokenizer) Error!u64 {
        if (currentToken(self).kind == .TK_NUM) {
            return currentToken(self).value;
        } else {
            self.reportError("expected a number", .{});
            return error.TokenNotANumber;
        }
    }
};

fn convertU8slicetoNum(str: []const u8) !u64 {
    var digits: u64 = 0;
    const value = str[0];
    if (std.ascii.isDigit(value)) {
        const ascii_offset = '0';
        const digit_index = 1;
        // convert ascii char to number
        digits = value - ascii_offset;
        //look from digit_index for additional digits
        for (str[digit_index..]) |digit| {
            if (std.ascii.isDigit(digit)) {
                //convert multiple number characters into a single numeric value
                digits = digits * 10 + (digit - ascii_offset);
                continue;
            }
            // digit is not a digit
            return error.u8ToNumError;
        }
    } else {
        return error.u8ToNumError;
    }
    return digits;
}

test "Tokenizer.convertU8toNum" {
    try std.testing.expect(3679 == try convertU8slicetoNum("3679"));
}
