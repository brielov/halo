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

impl Kind {
    /// Returns true if the token kind is a prefix operator.
    pub fn as_prefix_operator(&self) -> bool {
        matches!(self, Kind::Not | Kind::Minus | Kind::Plus | Kind::Tilde)
    }

    /// Returns true if the token kind is a postfix operator.
    pub fn is_postfix_operator(&self) -> bool {
        matches!(self, Kind::Question)
    }

    /// Returns the precedence level if the token kind is an infix operator, otherwise None.
    pub fn infix_precedence(&self) -> Option<u8> {
        match self {
            Kind::OrOR => Some(1),                                // Logical OR
            Kind::AndAnd => Some(2),                              // Logical AND
            Kind::Or => Some(3),                                  // Bitwise OR
            Kind::Caret => Some(4),                               // Bitwise XOR
            Kind::And => Some(5),                                 // Bitwise AND
            Kind::EqEq | Kind::Ne => Some(6),                     // Equality
            Kind::Lt | Kind::Le | Kind::Gt | Kind::Ge => Some(7), // Relational
            Kind::Shl | Kind::Shr => Some(8),                     // Shift
            Kind::Plus | Kind::Minus => Some(9),                  // Additive
            Kind::Star | Kind::Slash | Kind::Percent => Some(10), // Multiplicative
            Kind::Question => Some(11),                           // Postfix question operator
            Kind::Dot => Some(12), // Infix dot operator for member access
            _ => None,
        }
    }
}

impl<'a> Into<Kind> for &Token<'a> {
    fn into(self) -> Kind {
        self.kind
    }
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Kind::Comment => "Comment",
            Kind::Eof => "End of File",
            Kind::Identifier => "Identifier",
            Kind::Unknown => "Unknown",

            // Errors
            Kind::InvalidChar => "Invalid Character",
            Kind::InvalidNumber => "Invalid Number",
            Kind::UnterminatedChar => "Unterminated Character",
            Kind::UnterminatedString => "Unterminated String",

            // Primitives
            Kind::Binary => "Binary Literal",
            Kind::Char => "Char Literal",
            Kind::Float => "Float Literal",
            Kind::Hex => "Hex Literal",
            Kind::Integer => "Integer Literal",
            Kind::Octal => "Octal Literal",

            // String
            Kind::StringStart => "String Start",
            Kind::StringLiteral => "String Literal",
            Kind::InterpolationStart => "Interpolation Start",
            Kind::InterpolationEnd => "Interpolation End",
            Kind::StringEnd => "String End",

            // Keywords
            Kind::Async => "async",
            Kind::Break => "break",
            Kind::Continue => "continue",
            Kind::Defer => "defer",
            Kind::Enum => "enum",
            Kind::Extend => "extend",
            Kind::False => "false",
            Kind::Fn => "fn",
            Kind::For => "for",
            Kind::In => "in",
            Kind::Let => "let",
            Kind::Loop => "loop",
            Kind::Match => "match",
            Kind::Mut => "mut",
            Kind::Proto => "proto",
            Kind::Pub => "pub",
            Kind::Struct => "struct",
            Kind::True => "true",
            Kind::Type => "type",
            Kind::Use => "use",
            Kind::While => "while",
            Kind::With => "with",
            Kind::Yield => "yield",

            // Delimiters
            Kind::LBrace => "{",
            Kind::LBracket => "[",
            Kind::LParen => "(",
            Kind::RBrace => "}",
            Kind::RBracket => "]",
            Kind::RParen => ")",

            // Punctuation
            Kind::And => "&",
            Kind::AndAnd => "&&",
            Kind::AndEq => "&=",
            Kind::At => "@",
            Kind::Caret => "^",
            Kind::CaretEq => "^=",
            Kind::Colon => ":",
            Kind::Comma => ",",
            Kind::Dot => ".",
            Kind::DotDot => "..",
            Kind::DotDotEq => "..=",
            Kind::Eq => "=",
            Kind::EqEq => "==",
            Kind::Ge => ">=",
            Kind::Gt => ">",
            Kind::Le => "<=",
            Kind::Lt => "<",
            Kind::Minus => "-",
            Kind::MinusEq => "-=",
            Kind::Ne => "!=",
            Kind::Not => "!",
            Kind::Or => "|",
            Kind::OrEq => "|=",
            Kind::OrOR => "||",
            Kind::PathSep => "::",
            Kind::Percent => "%",
            Kind::PercentEq => "%=",
            Kind::Plus => "+",
            Kind::PlusEq => "+=",
            Kind::Question => "?",
            Kind::RArrow => "->",
            Kind::Semi => ";",
            Kind::Shl => "<<",
            Kind::ShlEq => "<<=",
            Kind::Shr => ">>",
            Kind::ShrEq => ">>=",
            Kind::Slash => "/",
            Kind::SlashEq => "/=",
            Kind::Star => "*",
            Kind::StarEq => "*=",
            Kind::Tilde => "~",
        };

        write!(f, "{s}")
    }
}
