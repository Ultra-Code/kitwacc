const std = @import("std");
const fmt = std.fmt;

const TokenKind = enum {
    TK_IDENT, // Identifiers
    TK_PUNCT, // Punctuators
    TK_NUM, // Numeric literals
    TK_EOF, // End-of-file markers
    TK_KEYWORD, //Keywords
};

pub const Token = struct {
    kind: TokenKind, // Token kind
    value: Value,
    location: usize, // location of Token in input stream
    pub const Value = union {
        num_value: u64, // If kind is TK_NUM, its value
        ident_name: []const u8, // if kind is TK_IDENT, it name
    };
};

const Tokenizer = @This();

const Error = error{
    TerminalMismatch,
    TokenNotANumber,
    InvalidToken,
};

pub fn OOMhandler() noreturn {
    std.log.err("allocator has run out of memory", .{});
    std.debug.panic("Out of Memory condition", .{});
    std.process.exit(4);
}

tokens: std.ArrayList(Token), // List of tokens
stream: []const u8,
position_in_stream: usize = 0,

pub fn init(allocator: std.mem.Allocator, input_stream: []const u8) Tokenizer {
    return Tokenizer{
        .tokens = std.ArrayList(Token).init(allocator),
        .stream = input_stream,
    };
}

// Create a new token.
fn addToken(self: *Tokenizer, token: Token) void {
    self.tokens.append(token) catch OOMhandler();
}

fn reportTokenizerError(self: *const Tokenizer, error_slice: []const u8, comptime msg: []const u8, args: anytype) noreturn {
    const token_index = std.mem.indexOf(u8, self.stream, error_slice).?;
    const error_fmt = "\nError: Invalid Token '{s}' in '{s} at {d}\n";
    const position_of_stream_in_error_fmt = 28;
    const error_token_start_location = error_slice.len + position_of_stream_in_error_fmt;
    const actual_location = error_token_start_location + token_index;
    std.debug.print(error_fmt, .{ error_slice, self.stream, token_index });
    std.debug.print("{[spaces]s:>[width]}", .{ .spaces = " ", .width = actual_location });
    const format_msg = "^ " ++ msg ++ "\n";
    std.debug.print(format_msg, args);
    std.process.exit(2);
}

const PUNCTUATOR = [_][]const u8{
    "==",
    "<=",
    ">=",
    "!=",
};

fn isPunct(punct: []const u8) bool {
    for (PUNCTUATOR) |punctuation| {
        if (std.mem.eql(u8, punctuation, punct)) return true;
    }
    return false;
}

// Returns true if char is valid as the first character of an identifier.
fn isValidIdentifier1stChar(char: u8) bool {
    if (std.ascii.isAlpha(char) or char == '_') {
        return true;
    }
    return false;
}

// Returns true if char is valid as a character in an identifier.
fn isValidIdentifierChar(char: u8) bool {
    if (isValidIdentifier1stChar(char) or std.ascii.isDigit(char)) {
        return true;
    }
    return false;
}

const KEYWORDS = [_][]const u8{
    "return",
    "if",
    "else",
    "for",
    "while",
};

fn isKeyword(identifier: []const u8) bool {
    for (KEYWORDS) |keyword| {
        if (std.mem.eql(u8, keyword, identifier)) {
            return true;
        }
    }
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
            const RADIX = 10;
            var digits = fmt.charToDigit(value, RADIX) catch unreachable;
            //look from digit_index for additional digits
            for (peek[digit_index..]) |digit| {
                if (std.ascii.isDigit(digit)) {
                    //convert multiple number characters into a single numeric value
                    digits = (digits * 10) + (fmt.charToDigit(digit, RADIX) catch unreachable);
                    digit_index += 1;
                    continue;
                }
                // digit is not a digit
                break;
            }
            //-1 because our anticipated next digit token wasn't a digit
            const next_token_index = digit_index - 1;
            index = next_token_index;
            self.addToken(Token{
                .kind = .TK_NUM,
                .value = Token.Value{ .num_value = digits },
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
                    const max_punct_char = 2;
                    var buf: [max_punct_char]u8 = undefined;
                    const operator = try std.fmt.bufPrint(&buf, "{c}{c}", .{ current_punct, next_punct });
                    if (isPunct(operator)) {
                        index = current_index + 1;
                        self.addToken(Token{
                            .kind = .TK_PUNCT,
                            .value = Token.Value{ .ident_name = try fmt.allocPrint(self.tokens.allocator, "{s}", .{operator}) },
                            .location = current_index,
                        });
                        continue;
                    }
                }
            }

            self.addToken(Token{
                .kind = .TK_PUNCT,
                .value = Token.Value{ .ident_name = try fmt.allocPrint(self.tokens.allocator, "{c}", .{current_punct}) },
                .location = index,
            });
            continue;
        }

        //Identifier or Keywords
        if (isValidIdentifier1stChar(value)) {
            const current_index = index;
            const next_index = index + 1;
            const max_identifier_length = 124;
            var identifier_buf: [max_identifier_length]u8 = undefined;
            identifier_buf[0] = value;
            const next_ident_offset = 1;
            var identifier_len: usize = 1;
            for (peek[next_index..]) |identifier_char, ident_index| {
                if (isValidIdentifierChar(identifier_char)) {
                    identifier_len += next_ident_offset;
                    //move index to end of identifier
                    index += next_ident_offset;
                    identifier_buf[ident_index + next_ident_offset] = identifier_char;
                } else {
                    //break if char isn't a valid identifier character
                    break;
                }
            }
            const identifier = identifier_buf[0..identifier_len];

            if (isKeyword(identifier)) {
                self.addToken(Token{
                    .kind = .TK_KEYWORD,
                    .value = Token.Value{ .ident_name = try fmt.allocPrint(self.tokens.allocator, "{s}", .{identifier}) },
                    .location = current_index,
                });
            } else {
                self.addToken(Token{
                    .kind = .TK_IDENT,
                    .value = Token.Value{ .ident_name = try fmt.allocPrint(self.tokens.allocator, "{s}", .{identifier}) },
                    .location = current_index,
                });
            }

            continue;
        }

        const bad_token = &[_]u8{value};
        self.reportTokenizerError(bad_token, "expected number or punctuation or identifier but found {s}", .{bad_token});
        return error.InvalidToken;
    }

    self.addToken(Token{
        .kind = .TK_EOF,
        .value = Token.Value{ .ident_name = "EOF" },
        .location = index,
    });
    return &self.tokens.items[0];
}
