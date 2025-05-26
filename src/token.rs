#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
    Comment,
    Eof,
    Identifier,
    Unknown,

    // Errors
    InvalidChar,
    InvalidNumber,
    UnterminatedChar,
    UnterminatedString,

    // Primitives
    Binary,
    Char,
    Float,
    Hex,
    Integer,
    Octal,

    // String
    StringStart,        // Represents the opening quote "
    StringLiteral,      // A literal string part, e.g., "Hello, "
    InterpolationStart, // Represents #{
    InterpolationEnd,   // Represents }
    StringEnd,          // Represents the closing quote "

    // Keywords
    Async,
    Break,
    Continue,
    Defer,
    Enum,
    Extend,
    False,
    Fn,
    For,
    In,
    Let,
    Loop,
    Match,
    Mut,
    Proto,
    Pub,
    Struct,
    True,
    Type,
    Use,
    While,
    With,
    Yield,

    // Delimiters
    LBrace,
    LBracket,
    LParen,
    RBrace,
    RBracket,
    RParen,

    // Punctuation
    And,
    AndAnd,
    AndEq,
    At,
    Caret,
    CaretEq,
    Colon,
    Comma,
    Dot,
    DotDot,
    DotDotEq,
    Eq,
    EqEq,
    Ge,
    Gt,
    Le,
    Lt,
    Minus,
    MinusEq,
    Ne,
    Not,
    Or,
    OrEq,
    OrOR,
    PathSep,
    Percent,
    PercentEq,
    Plus,
    PlusEq,
    Question,
    RArrow,
    Semi,
    Shl,
    ShlEq,
    Shr,
    ShrEq,
    Slash,
    SlashEq,
    Star,
    StarEq,
    Tilde,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub kind: Kind,
    pub lexeme: &'a str,
    pub start: usize,
    pub end: usize,
}
