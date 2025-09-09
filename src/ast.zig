const std = @import("std");
const interpreter = @import("interpreter.zig");
const intrinsics = @import("intrinsics.zig");
const util = @import("util.zig");
const gc = @import("boehm.zig");

var interned_intrinsics: std.StringArrayHashMapUnmanaged(*Expr) = .{};
var interned_syms: std.StringArrayHashMapUnmanaged(void) = .{};
var interned_nums: std.AutoHashMapUnmanaged(i16, *Expr) = .{};
const EnumField = std.builtin.Type.EnumField;

/// Generate an enum for built-ins where each field corresponds
/// to a public decl in intrinsics.zig
pub const Intrinsic = v: {
    const decls = std.meta.declarations(intrinsics);
    var fields: []const EnumField = &[_]EnumField{};
    for (decls, 0..) |decl, i| {
        fields = fields ++ &[_]EnumField{.{
            .name = decl.name,
            .value = i,
        }};
    }
    break :v @Type(.{ .Enum = .{
        .fields = fields,
        .tag_type = std.math.IntFittingRange(0, fields.len),
        .decls = &[_]std.builtin.Type.Declaration{},
        .is_exhaustive = false,
    } });
};

/// Generate a map from the Intrinsic enum fields to a built-in expression.
/// Use the `getIntrinsic` function to perform lookups into this map.
const intrinsic_map = v: {
    const intrinsic_fields = std.meta.fields(Intrinsic);
    var arr: [intrinsic_fields.len]*const Expr = undefined;

    for (intrinsic_fields) |f| {
        const tag = @field(Intrinsic, f.name);
        if (@hasDecl(intrinsics, @tagName(tag))) {
            const field = @field(intrinsics, f.name);
            arr[@intFromEnum(tag)] = switch (@typeInfo(@TypeOf(field))) {
                .Fn => &.{ .val = ExprValue{ .fun = @field(intrinsics, f.name) } },
                .Float => &.{ .val = ExprValue{ .num = @field(intrinsics, f.name) } },
                .Void => &.{ .val = ExprValue{ .sym = f.name } },
                else => @compileError("Unsupported type for intrinsic named " ++ f.name),
            };
        } else @compileError("All public decls should have a generated Intrinsic entry");
    }

    break :v arr;
};

/// Given an Intrinsic tag, return the corresponding built-in expression.
pub fn getIntrinsic(tag: Intrinsic) *Expr {
    return @constCast(intrinsic_map[@intFromEnum(tag)]);
}

/// Bio source code contents. If the source is loaded from a file,
/// then `path` is set to the resolved absolute path. This is used when
/// generating error messages.
pub const File = struct {
    /// If null, this file represents a REPL expression
    path: ?[]const u8 = null,
    /// Bio search code
    content: []const u8,
};

/// Source information used when printing error messages. This is computed
/// as necessary.
pub const SourceInfo = struct {
    /// 0-based line number
    line: usize,
    /// 0-based column
    column: usize,
    /// A slice into the original source
    source_line: []const u8,
    line_start: usize,
    line_end: usize,

    pub fn compute(tok: Token) @This() {
        const source = tok.file.content;
        var line: usize = 0;
        var column: usize = 0;
        var line_start: usize = 0;
        var i: usize = 0;
        while (i < tok.start and i < source.len) : (i += 1) {
            switch (source[i]) {
                '\n' => {
                    line += 1;
                    column = 0;
                    line_start = i + 1;
                },
                else => {
                    column += 1;
                },
            }
        }
        while (i < source.len and source[i] != '\n') {
            i += 1;
        }
        return .{
            .line = line,
            .column = column,
            .source_line = source[line_start..i],
            .line_start = line_start,
            .line_end = i,
        };
    }
};

/// The tokenizer produces a stream of Token instances. The token itself does not
/// store copies of string literals, identifiers, and so on. Rather, the start/end
/// indices into the original source is used whenever the content is actually needed,
/// such as when displaying error messages.
pub const Token = struct {
    start: u32,
    end: u32,
    file: *File,
    kind: Kind,

    pub const Kind = union(enum) {
        invalid,
        // eof,
        lparen,
        rparen,
        /// A symbol. If the input 'xyz, then the tokenization
        /// will be `.quote` followed by `.identifier`. Unquoted
        /// identifiers will be tokenized as just `.identifier`.
        identifier,
        /// A double quoted string. The quotes are excluded
        /// from the string.
        string,
        /// A number literal.
        number: f64,
        /// '
        quote,
        /// `
        quasi_quote,
        /// ,
        quasi_unquote,
        /// ,@
        quasi_unquote_splicing,
    };
};

/// The tokenizer turns a Bio source buffer into a stream of Tokens, each token
/// containing indices into the source and a Token.Kind determination.
pub const Tokenizer = struct {
    /// The Bio source code being tokenized. This can be safely replaced by
    /// larger slice while tokenizing, which is utilized by the REPL.
    file: *File,
    /// Current offset into `source`
    index: usize = 0,
    token_len: usize = 0,
    in_string: bool = false,
    in_comment: bool = false,
    prev_kind: Token.Kind = .invalid,

    /// Current parenthesis balance. This can be checked by tokenizer caller,
    /// using this logic: if 0, a fully balanced expression has been tokenized,
    /// if positive, the expression isn't done yet (or missing right parens),
    /// and if negative, too many right parenthesis has been encountered, which
    /// is an unrecoverable situation.
    ///
    /// = 0 => balanced parenthesis
    /// > 0 => too few right parenthesis
    /// < 0 => too many right parenthesis
    parens_balance: isize = 0,

    pub fn init(file: *File) @This() {
        return .{
            .file = file,
        };
    }

    pub fn updateSource(self: *Tokenizer, new_source: []const u8) void {
        self.file.content = new_source;
    }

    /// Extract the lexeme from the source code for the given token's start/end offsets.
    pub fn getLexeme(self: *Tokenizer, tok: *Token) []const u8 {
        return self.file.content[tok.start..tok.end];
    }

    /// Returns the next token, or null if we're done.
    pub fn next(self: *Tokenizer) !?Token {
        const source = self.file.content;
        const len = source.len;

        while (self.index < len) {
            const char = source[self.index];
            const maybe_next = if (self.index + 1 < len) source[self.index + 1] else null;

            if (self.in_comment) {
                if (char == '\n') self.in_comment = false;
                self.index += 1;
                continue;
            }

            if (char == ';' and !self.in_string) {
                self.in_comment = true;
                self.index += 1;
                continue;
            }

            if (char == '"' and !self.in_string) {
                self.in_string = true;
            } else if (char == '"' and self.in_string) {
                self.in_string = false;
                const tok = Token{
                    .start = @intCast(self.index - self.token_len),
                    .end = @intCast(self.index),
                    .file = self.file,
                    .kind = .string,
                };
                self.prev_kind = tok.kind;
                self.token_len = 0;
                self.index += 1;
                return tok;
            } else if (self.in_string) {
                self.token_len += 1;
            } else if (char == '\'') {
                const tok = Token{
                    .start = @intCast(self.index),
                    .end = @intCast(self.index + 1),
                    .file = self.file,
                    .kind = .quote,
                };
                self.prev_kind = tok.kind;
                self.index += 1;
                self.token_len = 0;
                return tok;
            } else if (char == '(' or char == ')' or char == '`') {
                if (self.token_len > 0) {
                    var tok = Token{
                        .start = @intCast(self.index - self.token_len),
                        .end = @intCast(self.index),
                        .file = self.file,
                        .kind = .identifier,
                    };
                    self.reclassifyIdentifier(&tok);
                    self.prev_kind = tok.kind;
                    self.token_len = 0;
                    return tok;
                } else {
                    self.parens_balance += switch (char) {
                        '(' => 1,
                        ')' => -1,
                        else => 0,
                    };
                    const tok = Token{
                        .start = @intCast(self.index),
                        .end = @intCast(self.index + 1),
                        .file = self.file,
                        .kind = switch (char) {
                            '(' => .lparen,
                            ')' => .rparen,
                            '`' => .quasi_quote,
                            else => unreachable,
                        },
                    };
                    self.prev_kind = tok.kind;
                    self.index += 1;
                    return tok;
                }
            } else if (char == ',') {
                if (self.token_len > 0) {
                    var tok = Token{
                        .start = @intCast(self.index - self.token_len),
                        .end = @intCast(self.index),
                        .file = self.file,
                        .kind = .identifier,
                    };
                    self.reclassifyIdentifier(&tok);
                    self.prev_kind = tok.kind;
                    self.token_len = 0;
                    // Note that we don't increase index as we want the ',' branch to
                    // be taken again with token_len == 0
                    return tok;
                }

                // Check for splice
                if (maybe_next != null and maybe_next.? == '@') {
                    const tok = Token{
                        .start = @intCast(self.index),
                        .end = @intCast(self.index + 2),
                        .file = self.file,
                        .kind = .quasi_unquote_splicing,
                    };
                    self.index += 2;
                    self.prev_kind = tok.kind;
                    return tok;
                }

                self.index += 1;
                return Token{
                    .start = @intCast(self.index - self.token_len - 1),
                    .end = @intCast(self.index),
                    .file = self.file,
                    .kind = .quasi_unquote,
                };
            } else if (std.mem.indexOfAny(u8, &.{char}, " \t\n\r") != null) {
                if (self.token_len > 0) {
                    var tok = Token{
                        .start = @intCast(self.index - self.token_len),
                        .end = @intCast(self.index),
                        .file = self.file,
                        .kind = .identifier,
                    };
                    self.reclassifyIdentifier(&tok);
                    self.token_len = 0;
                    self.index += 1;
                    return tok;
                }
            } else {
                self.token_len += 1;
            }

            self.index += 1;
        }

        if (self.in_string) return error.UnterminatedStringLiteral;

        if (self.token_len > 0) {
            var tok = Token{
                .start = @intCast(self.index - self.token_len),
                .end = @intCast(self.index),
                .file = self.file,
                .kind = .identifier,
            };
            self.reclassifyIdentifier(&tok);
            self.token_len = 0;
            return tok;
        }

        // Eof of token stream
        return null;
    }

    // Determine if an `.identifier` token can be further classified
    // as a number.
    fn reclassifyIdentifier(self: *Tokenizer, tok: *Token) void {
        std.debug.assert(tok.kind == .identifier);
        std.debug.assert(tok.end > tok.start);

        const ident = self.getLexeme(tok);
        std.debug.assert(ident.len > 0);

        // For numbers, we store the result to avoid having to parse the lexeme twice
        const num: f64 = std.fmt.parseFloat(f64, ident) catch {
            return;
        };

        tok.kind = .{ .number = num };
    }
};

test "tokenize an expression" {
    var source =
        \\ (begin (print "str" 5 `(,a ,@b)))
    ;

    var file = File{ .content = source };
    var tokenizer = Tokenizer.init(&file);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .lparen);
    const begin_tok = (try tokenizer.next()).?;
    try std.testing.expectEqual(begin_tok.kind, .identifier);
    try std.testing.expectEqualSlices(u8, source[begin_tok.start..begin_tok.end], "begin");
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .lparen);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .identifier);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .string);
    const num_tok = (try tokenizer.next()).?;
    try std.testing.expectEqualSlices(u8, source[num_tok.start..num_tok.end], "5");
    try std.testing.expectEqual(num_tok.kind.number, 5);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .quasi_quote);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .lparen);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .quasi_unquote);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .identifier);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .quasi_unquote_splicing);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .identifier);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .rparen);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .rparen);
    try std.testing.expectEqual((try tokenizer.next()).?.kind, .rparen);
    try std.testing.expectEqual(tokenizer.parens_balance, 0);
}

test "tokenize source with too many right parenthesis" {
    const source =
        \\ (* 2 3))
    ;
    var file = File{ .content = source };
    var tokenizer = Tokenizer.init(&file);
    while (try tokenizer.next()) |_| {}
    try std.testing.expectEqual(tokenizer.parens_balance, -1);
}

test "tokenize source with too few right parenthesis" {
    const source =
        \\ (/ (* 2 3) 1.5
    ;
    var file = File{ .content = source };
    var tokenizer = Tokenizer.init(&file);
    while (try tokenizer.next()) |_| {}
    try std.testing.expectEqual(tokenizer.parens_balance, 1);
}

/// The parser runs through tokens given by a tokenizer and constructs a list
/// of s-expressions. For a REPL, this may be a single expression, while it's
/// usually a long sequence of expressions when parsing a whole file. The parser
/// does not evaluate anything, it just produces s-expressions, which are by
/// definition atoms and lists of atoms and other lists.
/// The parser is resumable, which is useful for a REPL.
pub const Parser = struct {
    file: *File,
    tokenizer: Tokenizer,
    /// A list expression containing parsed expressions
    cur: *Expr = undefined,
    list_stack: std.ArrayList(*Expr),
    prev_token: ?Token = null,

    pub fn init(file: *File) @This() {
        return .{
            .tokenizer = Tokenizer.init(file),
            .file = file,
            // Create a top-level list containing all parsed expressions
            .cur = makeListExpr(null) catch unreachable,
            .list_stack = std.ArrayList(*Expr).init(gc.allocator()),
        };
    }

    /// Returns a list expression where each entry can be evaluated.
    /// The `file_path`, if given, will be used in error messages.
    pub fn parseMultipleExpressionsFromReader(reader: anytype, file_path: ?[]const u8) !*Expr {
        var source = std.ArrayList(u8).init(gc.allocator());
        const writer = source.writer();
        try util.copyBytes(reader, writer);
        return parseMultipleExpressions(try source.toOwnedSlice(), file_path);
    }

    /// Returns a list expression where each entry can be evaluated.
    /// The `file_path`, if given, will be used in error messages.
    pub fn parseMultipleExpressions(source: []const u8, file_path: ?[]const u8) !*Expr {
        const file = try gc.allocator().create(File);
        file.* = .{ .path = file_path, .content = source };
        var parser = Parser.init(file);
        try parser.parse();
        return parser.cur;
    }

    /// Returns an expression that can be evaluated. The input is expected to have just one expression,
    /// and should thus never be used on user input.
    pub fn parseSingleExpression(source: []const u8) !*Expr {
        const list_expr = try parseMultipleExpressions(source, null);
        std.debug.assert(list_expr.val == .lst and list_expr.val.lst.items.len == 1);
        return list_expr.val.lst.items[0];
    }

    /// Parsing is iterative and resumable after adding more source. We avoid
    /// recursion since Bio can be used as a dataformat with arbitrary nesting depth.
    pub fn parse(self: *Parser) !void {
        while (try self.tokenizer.next()) |tok| {
            switch (tok.kind) {
                .lparen => {
                    try self.list_stack.append(self.cur);
                    self.cur = try makeListExpr(null);
                    self.cur.tok = tok;
                },
                .rparen => {
                    // TODO: deal with too many rparens?
                    if (self.list_stack.items.len == 0) {
                        // std.debug.print("Imbalance!\n", .{});
                        return error.TooManyRightParens;
                    }
                    const completed_list = self.cur;
                    self.cur = self.list_stack.pop();
                    std.debug.assert(completed_list != self.cur);
                    std.debug.assert(self.cur.val == .lst);
                    try self.cur.val.lst.append(completed_list);
                },
                .string => {
                    // Turn into a (quote ...) list expression. As this is known to be a string quotation, we
                    // end the list immidiately, unlike the .quote which continues the loop.
                    var quote_expr = try Expr.fromIntrinsic(@constCast(getIntrinsic(.quote)));
                    quote_expr.tok = tok;
                    var atom = try makeSymbol(self.file.content[tok.start..tok.end], false);
                    atom.tok = tok;
                    var quote_list = try makeListExpr(&.{ quote_expr, atom });
                    quote_list.tok = tok;
                    try self.cur.val.lst.append(quote_list);
                    continue;
                },
                .quote => {
                    // Turn into a (quote ...) list expression
                    try self.list_stack.append(self.cur);
                    var quote_expr = try Expr.fromIntrinsic(@constCast(getIntrinsic(.quote)));
                    quote_expr.tok = tok;
                    self.cur = try makeListExpr(&.{quote_expr});
                    self.cur.tok = tok;
                    continue;
                },
                .quasi_quote => {
                    // Turn into a (quasiquote ...) list expression
                    try self.list_stack.append(self.cur);
                    var quote_expr = try Expr.fromIntrinsic(@constCast(getIntrinsic(.quasiquote)));
                    quote_expr.tok = tok;
                    self.cur = try makeListExpr(&.{quote_expr});
                    self.cur.tok = tok;
                    continue;
                },
                .quasi_unquote => {
                    // Turn into a (unquote ...) list expression
                    try self.list_stack.append(self.cur);
                    var quote_expr = try Expr.fromIntrinsic(@constCast(getIntrinsic(.unquote)));
                    quote_expr.tok = tok;
                    self.cur = try makeListExpr(&.{quote_expr});
                    self.cur.tok = tok;
                    continue;
                },
                .quasi_unquote_splicing => {
                    // Turn into a (unquote-splicing ...) list expression
                    try self.list_stack.append(self.cur);
                    var quote_expr = try Expr.fromIntrinsic(@constCast(getIntrinsic(.@"unquote-splicing")));
                    quote_expr.tok = tok;
                    self.cur = try makeListExpr(&.{quote_expr});
                    self.cur.tok = tok;
                    continue;
                },
                .identifier => {
                    var atom = try makeAtomByDuplicating(self.file.content[tok.start..tok.end]);
                    if (std.meta.stringToEnum(Intrinsic, self.file.content[tok.start..tok.end]) == null) {
                        atom.tok = tok;
                    }
                    try self.cur.val.lst.append(atom);
                },
                .number => |n| {
                    var atom = try makeNumExpr(n);
                    atom.tok = tok;
                    try self.cur.val.lst.append(atom);
                },
                .invalid => @panic("Invalid token state"),
            }

            // Is the current list a (quote ...) list, or similarily quoting-related? Then close it.
            if (self.cur.val.lst.items.len > 0 and self.cur.val.lst.items[0].val == .fun and
                (self.cur.val.lst.items[0].isIntrinsic(.quote) or
                self.cur.val.lst.items[0].isIntrinsic(.unquote) or
                self.cur.val.lst.items[0].isIntrinsic(.quasiquote) or
                self.cur.val.lst.items[0].isIntrinsic(.@"unquote-splicing")))
            {
                const completed_list = self.cur;
                self.cur = self.list_stack.pop();
                try self.cur.val.lst.append(completed_list);
            }
            self.prev_token = tok;
        }
    }

    /// Updates the slice which the parser operates on, while keeping the
    /// state intact. This allows a gradually larger slice to be parsed,
    /// such as in a REPL situation.
    pub fn updateSource(self: *Parser, source: []const u8) void {
        // This is safe because the tokenizer uses offsets into the source, not pointers.
        // The tokenizer will happily continue where it left off.
        self.file.content = source;
        self.tokenizer.updateSource(source);
    }

    /// Checks if one or more )'s are missing, indicating that more source code
    /// is needed, which can be added by calling `appendSource`
    pub fn missingRightParen(self: *Parser) bool {
        return self.tokenizer.parens_balance > 0;
    }
};

test "resumable parser" {
    // Simulate a REPL session with partial input on the first line
    var file = File{ .content = "" };
    var p = Parser.init(&file);
    p.updateSource("(* 2\n");
    while (true) {
        try p.parse();
        if (p.missingRightParen()) {
            // next REPL line input
            p.updateSource("(* 2\n 2) (if #t 8 9) (+ 3 4 'a) (make `(x ,y z ,@lst))");
            continue;
        }
        // We're done
        break;
    }
    try std.testing.expectEqual(p.tokenizer.parens_balance, 0);
}

/// All built-in Bio functions have this signature
pub const IntrinsicFn = *const fn (evaluator: *interpreter.Interpreter, env: *Env, []const *Expr) anyerror!*Expr;

/// A Bio error encounted during parsing or evaluation
pub const ExprErrors = error{
    AlreadyReported,
    MissingRightParen,
    TooManyRightParens,
    UnexpectedRightParen,
    ExpectedNumber,
    ExpectedBool,
    InvalidArgumentType,
    InvalidArgumentCount,
    SyntaxError,
    Eof,
    BindingNotFound,
    NotInQuasiQuote,
    AssertionFailed,
};

/// Given an error, return the corresponding error message fragment.
pub fn errString(err: anyerror) []const u8 {
    return switch (err) {
        ExprErrors.AlreadyReported => "***",
        ExprErrors.InvalidArgumentCount => "Invalid argument count",
        ExprErrors.InvalidArgumentType => "Invalid argument type",
        ExprErrors.ExpectedNumber => "Expected a number",
        ExprErrors.ExpectedBool => "Expected a boolean expression",
        ExprErrors.UnexpectedRightParen => "Unexpected )",
        ExprErrors.MissingRightParen => "Missing )",
        ExprErrors.TooManyRightParens => "Too many )'s'",
        ExprErrors.SyntaxError => "Syntax error while parsing",
        ExprErrors.Eof => "End of file",
        ExprErrors.BindingNotFound => "Binding not found",
        ExprErrors.NotInQuasiQuote => "Not in a quasiquote context",
        ExprErrors.AssertionFailed => "Assertion failed",
        else => "Unknown error",
    };
}

/// Expression types
pub const ExprType = enum(u8) { sym, num, lst, map, lam, mac, fun, env, err, any };

/// Given an expression type, return the corresponding string representation. Used in error messages.
pub fn typeString(etype: ExprType) []const u8 {
    return switch (etype) {
        .sym => "symbol",
        .num => "number",
        .lst => "list",
        .map => "map",
        .lam => "lambda",
        .mac => "macro",
        .fun => "intrinsic",
        .env => "environment",
        .err => "error",
        .any => "any",
    };
}

/// Expression runtime value
pub const ExprValue = union(ExprType) {
    sym: []const u8,
    num: f64,
    lst: std.ArrayList(*Expr),
    map: std.ArrayHashMap(*Expr, *Expr, Expr.HashUtil, true),
    lam: std.ArrayList(*Expr),
    mac: std.ArrayList(*Expr),
    fun: IntrinsicFn,
    env: *Env,
    err: *Expr,
    /// Type-erased value, such as a file descriptor or a pointer to a struct
    any: usize,
};

/// A Bio expression with a value and an optional environment (lambda expressions
/// must know in which environment they were defined)
pub const Expr = struct {
    /// Hashmap support for Expr
    pub const HashUtil = struct {
        pub fn hash(_: HashUtil, key: *Expr) u32 {
            if (key.val == ExprType.sym) {
                return @as(u32, @truncate(std.hash.Wyhash.hash(0, key.val.sym)));
            } else if (key.val == ExprType.num) {
                return @as(u32, @truncate(@as(u64, @intFromFloat(key.val.num))));
            }
            @panic("Invalid hash key type");
        }
        pub fn eql(_: HashUtil, first: *Expr, second: *Expr, b_index: usize) bool {
            _ = b_index;
            if (first.val == ExprType.sym) {
                return std.mem.eql(u8, first.val.sym, second.val.sym);
            } else if (first.val == ExprType.num) {
                return first.val.num == second.val.num;
            }
            @panic("Invalid hash key type");
        }
    };

    val: ExprValue,
    env: ?*Env = null,
    tok: ?Token = null,

    /// Create a new expression with an undefined value
    pub fn create(_: bool) !*@This() {
        var allocator = gc.allocator();
        const self = try allocator.create(Expr);
        self.* = Expr{ .val = undefined };
        return self;
    }

    /// Creates a new Expr, copying the value from the intrinsic. By creating a new Expr instance,
    /// we can later add Token information (intrinsic expressions are singletons)
    pub fn fromIntrinsic(intrinsic_expr: *Expr) !*@This() {
        var allocator = gc.allocator();
        const expr = try allocator.create(Expr);
        expr.* = Expr{ .val = intrinsic_expr.val };
        // var file = File{ .content = "abcd" };
        // expr.tok = Token{ .start = 0, .end = 0, .file = file };
        return expr;
    }

    pub fn isIntrinsic(self: *@This(), tag: Intrinsic) bool {
        const intrinsic_expr = getIntrinsic(tag);
        if (self == intrinsic_expr) return true;

        return switch (self.val) {
            .fun => |f| intrinsic_expr.val == .fun and intrinsic_expr.val.fun == f,
            .sym => |s| intrinsic_expr.val == .sym and intrinsic_expr.val.sym.ptr == s.ptr,
            .num => |n| intrinsic_expr.val == .num and intrinsic_expr.val.num == n,
            else => false,
        };
    }

    pub fn isNil(self: *@This()) bool {
        return self.isIntrinsic(.nil);
    }

    /// Called by the GC to clean up expression resources. Note that symbols
    /// are deallocated by sweeping the internalized string map.
    pub fn deinit(self: *@This()) void {
        if (self.val == ExprType.lst) {
            self.val.lst.deinit();
        } else if (self.val == ExprType.map) {
            self.val.map.deinit();
        } else if (self.val == ExprType.lam) {
            self.val.lam.deinit();
        } else if (self.val == ExprType.mac) {
            self.val.mac.deinit();
        }
    }

    /// Returns an owned string representation of this expression
    pub fn toStringAlloc(self: *@This()) anyerror![]u8 {
        const allocator = gc.allocator();
        var ident: []const u8 = if (self.tok) |tok| if (tok.file.content.len >= tok.end) tok.file.content[tok.start..tok.end] else "?" else "??";
        // if (self.val != .lst)
        //     std.debug.print("indent {*}:{s}\n", .{ self, ident });
        switch (self.val) {
            // Print symbol along with an indication if it's an intrinsic symbol
            ExprValue.sym => return try std.fmt.allocPrint(allocator, "{s}", .{self.val.sym}),
            ExprValue.num => return try std.fmt.allocPrint(allocator, "{d}", .{self.val.num}),
            ExprValue.lam => return try std.fmt.allocPrint(allocator, "<lambda>", .{}),
            ExprValue.mac => return try std.fmt.allocPrint(allocator, "<macro>", .{}),
            ExprValue.fun => {
                // The source may use quote-related abbreviations, but the parser has created a list
                // which we replace with the corresponding intrinsic function name.
                if (std.mem.eql(u8, ident, "'")) ident = "quote";
                if (std.mem.eql(u8, ident, "`")) ident = "quasiquote";
                if (std.mem.eql(u8, ident, ",")) ident = "unquote";
                if (std.mem.eql(u8, ident, ",@")) ident = "unquote-splicing";
                return try std.fmt.allocPrint(allocator, "{s}", .{ident});
            },
            ExprValue.env => return try std.fmt.allocPrint(allocator, "<env>", .{}),
            ExprValue.any => return try std.fmt.allocPrint(allocator, "<any>", .{}),
            ExprValue.err => |err_expr| {
                const err_str = try err_expr.toStringAlloc();
                return try std.fmt.allocPrint(allocator, "{s}", .{err_str});
            },
            ExprValue.lst => |lst| {
                var buf = std.ArrayList(u8).init(allocator);
                defer buf.deinit();
                var bufWriter = buf.writer();

                try bufWriter.writeAll("(");
                for (lst.items, 0..) |item, index| {
                    const itemBuf = try item.toStringAlloc();
                    try bufWriter.writeAll(itemBuf);
                    if (index + 1 < lst.items.len) {
                        try bufWriter.writeAll(" ");
                    }
                }
                try bufWriter.writeAll(")");
                return buf.toOwnedSlice();
            },
            ExprValue.map => |map| {
                var buf = std.ArrayList(u8).init(allocator);
                defer buf.deinit();
                var bufWriter = buf.writer();

                // Output is ((key val)(key val)(key val))
                try bufWriter.writeAll("(");
                var it = map.iterator();
                while (it.next()) |entry| {
                    const key = try entry.key_ptr.*.toStringAlloc();
                    const val = try entry.value_ptr.*.toStringAlloc();

                    try bufWriter.writeAll("(");
                    try bufWriter.writeAll(key);
                    try bufWriter.writeAll(" ");
                    try bufWriter.writeAll(val);
                    try bufWriter.writeAll(")");
                }
                try bufWriter.writeAll(")");
                return try buf.toOwnedSlice();
            },
        }
    }

    /// Prints the expression to stderr
    pub fn print(self: *@This()) anyerror!void {
        const str = try self.toStringAlloc();
        std.debug.print("{s}", .{str});
    }
};

/// Environment for variable bindings. Instances are named to get friendly debugging output.
pub const Env = struct {
    // This is the same as Expr.HashUtil, but is duplicated here to dodge a `get_slice_type` compiler bug
    pub const HashUtil = struct {
        pub fn hash(_: HashUtil, key: *Expr) u32 {
            if (key.val == ExprType.sym) {
                return @as(u32, @truncate(std.hash.Wyhash.hash(0, key.val.sym)));
            } else if (key.val == ExprType.num) {
                return @as(u32, @truncate(@as(u64, @intFromFloat(key.val.num))));
            }
            @panic("Invalid hash key type");
        }
        pub fn eql(_: HashUtil, first: *Expr, second: *Expr, b_index: usize) bool {
            _ = b_index;
            if (first.val == ExprType.sym) {
                return std.mem.eql(u8, first.val.sym, second.val.sym);
            } else if (first.val == ExprType.num) {
                return first.val.num == second.val.num;
            }
            @panic("Invalid hash key type");
        }
    };

    map: std.ArrayHashMap(*Expr, *Expr, Env.HashUtil, true),
    parent: ?*@This() = null,
    name: []const u8,

    pub fn populateWithIntrinsics(env: *Env) !void {
        @setEvalBranchQuota(100_000);
        inline for (std.meta.fields(Intrinsic)) |f| {
            const key = @field(Intrinsic, f.name);
            try env.putIntrinsic(key, getIntrinsic(key));
        }
    }

    pub fn putIntrinsic(self: *@This(), key: Intrinsic, val: *const Expr) !void {
        const binding_expr = try makeAtomByDuplicating(@tagName(key));
        try self.putWithSymbol(binding_expr, @constCast(val));
    }

    pub fn deinit(self: *@This()) void {
        self.map.deinit();
    }

    /// Put symbol/value, duplicating the key, replacing any existing value
    pub fn put(self: *@This(), key: []const u8, val: *Expr) !void {
        const binding_expr = try makeAtomByDuplicating(key);
        try self.putWithSymbol(binding_expr, val);
    }

    // TODO: remove
    pub fn putFunction(self: *@This(), key: Intrinsic, val: *Expr) !void {
        const binding_expr = try makeAtomByDuplicating(@tagName(key));
        try self.putWithSymbol(binding_expr, val);
    }

    /// Put symbol/value, replacing any existing value
    pub fn putWithSymbol(self: *@This(), variable_name: *Expr, val: *Expr) anyerror!void {
        try self.map.put(variable_name, val);
    }

    /// Look up an expression in this or a parent environment
    pub fn lookup(self: *@This(), sym: []const u8, recursive: bool) ?*Expr {
        var lookupSym = Expr{ .val = ExprValue{ .sym = sym } };
        if (self.map.get(&lookupSym)) |val| {
            return val;
        } else if (self.parent) |parent| {
            return if (recursive) parent.lookup(sym, recursive) else null;
        } else {
            return null;
        }
    }

    /// Recursively search for the binding, replace it if found.
    /// If the new value is null, the binding is removed instead.
    /// If the binding is found, then then `val` is returned. This makes
    /// it possible to implement macros such as =+
    /// If the binding is not found, an error is returned.
    pub fn replace(self: *Env, var_name: *Expr, val: ?*Expr) !*Expr {
        if (self.map.get(var_name)) |_| {
            if (val) |value| {
                try self.putWithSymbol(var_name, value);
                return value;
            } else {
                _ = self.map.swapRemove(var_name);
                return getIntrinsic(.nil);
            }
        } else if (self.parent) |parent| {
            return try parent.replace(var_name, val);
        }

        return ExprErrors.BindingNotFound;
    }
};

/// Make an environment expression
pub fn makeEnv(parent: ?*Env, name: []const u8) !*Env {
    var allocator = gc.allocator();
    var env = try allocator.create(Env);
    env.parent = parent;
    env.map = @TypeOf(env.map).init(allocator);
    env.name = name;
    return env;
}

/// Duplicates the input and return an atom
/// If the input is an intrinsic symbol, then the corresponding expression
/// is returned instead of duplicating.
pub fn makeAtomByDuplicating(buf: []const u8) anyerror!*Expr {
    return try makeInternedExpr(buf, false);
}

/// Takes ownership of the input and returns an atom
/// ONLY call this for freshly allocated data (such as reading a line from a file)
/// where it's obviously no other references to the data.
pub fn makeAtomAndTakeOwnership(buf: []const u8) anyerror!*Expr {
    return try makeInternedExpr(buf, true);
}

/// Make and return a potentially interned atom (symbol or number)
fn makeInternedExpr(buf: []const u8, take_ownership: bool) anyerror!*Expr {
    @setEvalBranchQuota(100_000);
    if (std.meta.stringToEnum(Intrinsic, buf)) |tag| {
        const expr = getIntrinsic(tag);
        if (expr.val == .sym) {
            return expr;
        }
    }

    const allocator = gc.allocator();

    // Zig's parseFloat is too lenient and accepts input like "." and "--"
    // For Bio, we require at least one digit.
    if (std.mem.indexOfAny(u8, buf, "0123456789")) |_| {
        if (std.fmt.parseFloat(f64, buf)) |num| {
            const internalizable = @floor(num) == num and !std.math.isInf(num) and num > -1024 and num < 1024;
            if (internalizable) {
                if (interned_nums.get(@as(i16, @intFromFloat(num)))) |expr| {
                    return expr;
                }
            }

            var expr = try Expr.create(false);
            expr.val = ExprValue{ .num = num };
            if (internalizable) {
                try interned_nums.put(allocator, @as(i16, @intFromFloat(num)), expr);
            }
            return expr;
        } else |_| {}
    }

    // std.debug.print("Not a number, nor an intrinsic symbol: '{s}'\n", .{literal});
    return try makeSymbol(buf, take_ownership);
}

/// Make an interned symbol, where a symbol is a non-numeric atom
/// such as 'abc and "my string".
/// Note that "5" is considered a symbol atom, while '5 is a numeric atom.
/// If `take_ownership` is false, the input `buf` is copied.
pub fn makeSymbol(buf: []const u8, take_ownership: bool) anyerror!*Expr {
    var allocator = gc.allocator();
    const sym = sym_blk: {
        const maybe_entry = interned_syms.getEntry(buf);
        if (maybe_entry) |entry| {
            break :sym_blk entry.key_ptr.*;
        } else {
            const res = if (take_ownership) buf else try allocator.dupe(u8, buf);
            try interned_syms.put(allocator, res, {});
            break :sym_blk res;
        }
    };

    var expr = try Expr.create(true);
    expr.val = ExprValue{ .sym = sym };
    return expr;
}

/// Make a list expression.
/// If `initial_expressions` is not null, each item is added *without evaluation*
pub fn makeListExpr(initial_expressions: ?[]const *Expr) !*Expr {
    const allocator = gc.allocator();
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .lst = std.ArrayList(*Expr).init(allocator) };
    if (initial_expressions) |expressions| {
        for (expressions) |e| {
            try expr.val.lst.append(e);
        }
    }
    return expr;
}

/// Make a hashmap expression
/// If `initial_expressions` is not null, each item as added *without evaluation*
pub fn makeHashmapExpr(initial_expressions: ?[]const *Expr) !*Expr {
    const allocator = gc.allocator();
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .map = std.ArrayHashMap(*Expr, *Expr, Expr.HashUtil, true).init(allocator) };

    if (initial_expressions) |expressions| {
        for (expressions) |e| {
            try expr.val.map.put(e.val.lst.items[0], e.val.lst.items[1]);
        }
    }
    return expr;
}

/// Make a lambda expression
pub fn makeLambdaExpr(env: *Env) !*Expr {
    const allocator = gc.allocator();
    var expr = try Expr.create(true);

    // This is a crucial detail: we're recording the environment that existed at the
    // time of lambda definition. This will be the parent environment whenever we
    // are invoking the lambda in Interpreter#eval
    expr.env = env;
    expr.val = ExprValue{ .lam = std.ArrayList(*Expr).init(allocator) };
    return expr;
}

/// Make a macro expression
pub fn makeMacroExpr() !*Expr {
    const allocator = gc.allocator();
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .mac = std.ArrayList(*Expr).init(allocator) };
    return expr;
}

/// Make a numeric expression
pub fn makeNumExpr(num: f64) !*Expr {
    var expr = try Expr.create(true);
    expr.val = ExprValue{ .num = num };
    return expr;
}

/// Make an error expression
pub fn makeError(expr: *Expr) !*Expr {
    var error_expr = try Expr.create(true);
    error_expr.val = ExprValue{ .err = expr };
    return error_expr;
}
