const std = @import("std");
const fmt = std.fmt;

const TokenKind = enum {
    TK_PUNCT, // Punctuators
    TK_NUM, // Numeric literals
    TK_EOF, // End-of-file markers
};

pub const Token = struct {
    kind: TokenKind, // Token kind
    value: u64, // If kind is TK_NUM, its value
    lexeme: []const u8, // Token lexeme
    lenght: usize, // Token length
    location: usize, // location of Token in input stream
};

const Tokenizer = @This();

const Error = error{
    TerminalMismatch,
    TokenNotANumber,
    InvalidToken,
};

tokens: std.ArrayList(Token), // Next token
stream: []const u8,
position_in_stream: usize = 0,

pub fn init(allocator: std.mem.Allocator, input_stream: []const u8) Tokenizer {
    return Tokenizer{
        .tokens = std.ArrayList(Token).init(allocator),
        .stream = input_stream,
    };
}

// Create a new token.
fn addToken(self: *Tokenizer, token: Token) !void {
    try self.tokens.append(token);
}

pub fn reportTokenizerError(self: *const Tokenizer, error_slice: []const u8, comptime msg: []const u8, args: anytype) noreturn {
    const token_index = std.mem.indexOf(u8, self.stream, error_slice).?;
    const error_fmt = "Error: Invalid Token '{s}' in '{s} at {d}\n";
    const position_of_stream_in_error_fmt = 28;
    const error_token_start_location = error_slice.len + position_of_stream_in_error_fmt;
    const actual_location = error_token_start_location + token_index;
    std.debug.print(error_fmt, .{ error_slice, self.stream, token_index });
    std.debug.print("{[spaces]s:>[width]}", .{ .spaces = " ", .width = actual_location });
    const format_msg = "^ " ++ msg ++ "\n";
    std.debug.print(format_msg, args);
    std.process.exit(2);
}

fn isPunct(self: *Tokenizer, punct: []const u8) bool {
    if (std.mem.eql(u8, punct, "==") or std.mem.eql(u8, punct, "<=") or std.mem.eql(u8, punct, ">=") or
        std.mem.eql(u8, punct, "!="))
    {
        return true;
    }
    self.reportTokenizerError(punct, "expected == or <= or >= or != but found {s}", .{punct});
    return false;
}

// Tokenize `peek` and returns new tokens.
pub fn tokenize(self: *Tokenizer) !*const Token {
    const peek = self.stream;
    var index: usize = 0;
    //condition to prevent out of bound index but allow procession of last token
    while (index < peek.len) : ({
        index += 1;
        self.position_in_stream = index;
    }) {
        const value = peek[index];
        // Numeric literal
        if (std.ascii.isDigit(value)) {
            const current_index = index;
            //since value is a digit we need to look pass the current index for addition digits in the stream
            var digit_index = current_index + 1;
            // convert ascii char to number
            var digits = try fmt.charToDigit(value, 10);
            //look from digit_index for additional digits
            for (peek[digit_index..]) |digit| {
                if (std.ascii.isDigit(digit)) {
                    //convert multiple number characters into a single numeric value
                    digits = digits * 10 + try fmt.charToDigit(digit, 10);
                    digit_index += 1;
                    continue;
                }
                // digit is not a digit
                break;
            }
            //-1 because our anticipated next digit token wasn't a digit
            const next_token_index = digit_index - 1;
            index = next_token_index;
            try self.addToken(Token{
                .kind = .TK_NUM,
                .value = digits,
                .lexeme = try fmt.allocPrint(self.tokens.allocator, "{d}", .{digits}),
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
        if (std.ascii.isPunct(value)) {
            const current_index = index;
            const current_punct = value;
            //check if == != <= or >=
            if (current_index + 1 < peek.len) {
                if (std.ascii.isPunct(peek[current_index + 1])) {
                    const next_punct = peek[current_index + 1];
                    var buf: [2]u8 = undefined;
                    const operator = try std.fmt.bufPrint(&buf, "{c}{c}", .{ current_punct, next_punct });
                    if (self.isPunct(operator)) {
                        index = current_index + 1;
                        try self.addToken(Token{
                            .kind = .TK_PUNCT,
                            .value = value,
                            .lexeme = try fmt.allocPrint(self.tokens.allocator, "{s}", .{operator}),
                            .lenght = operator.len,
                            .location = current_index,
                        });
                        continue;
                    }
                }
            }

            try self.addToken(Token{
                .kind = .TK_PUNCT,
                .value = value,
                .lexeme = try fmt.allocPrint(self.tokens.allocator, "{c}", .{current_punct}),
                .lenght = 1,
                .location = index,
            });

            continue;
        }

        const bad_token = &[_]u8{value};
        self.reportTokenizerError(bad_token, "expected number or punctuation but found {s}", .{bad_token});
        return error.InvalidToken;
    }

    try self.addToken(Token{
        .kind = .TK_EOF,
        .value = 0,
        .lexeme = "EOF",
        .lenght = 0,
        .location = index,
    });
    return &self.tokens.items[0];
}
