use std::str::Chars;

use crate::token::{Kind, Token};

pub struct Lexer<'a> {
    chars: Chars<'a>,
    source: &'a str,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer` instance from the input source string.
    ///
    /// # Arguments
    /// * `source` - The input string to tokenize.
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars(),
            source,
        }
    }

    /// Returns the current offset in the source string.
    fn offset(&self) -> usize {
        self.source.len() - self.chars.as_str().len()
    }

    /// Peeks at the next character without consuming it.
    fn peek(&self) -> Option<char> {
        self.chars.as_str().chars().next()
    }

    /// Advances the lexer and returns the next character.
    fn advance(&mut self) -> Option<char> {
        self.chars.next()
    }

    /// Consumes the next character if it satisfies the given predicate.
    ///
    /// # Arguments
    /// * `f` - A function that takes a character and returns a boolean.
    fn consume_if<F>(&mut self, predicate: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        if let Some(c) = self.peek() {
            if predicate(c) {
                self.advance();
                return true;
            }
        }
        false
    }

    /// Consumes characters while the predicate is true and returns the consumed slice.
    ///
    /// # Arguments
    /// * `f` - A function that takes a character and returns a boolean.
    fn consume_while<F>(&mut self, predicate: F) -> &'a str
    where
        F: Fn(char) -> bool,
    {
        let start = self.offset();
        while self.consume_if(&predicate) {}
        &self.source[start..self.offset()]
    }

    /// Determines the kind of the next token based on the current character.
    fn read_next_kind(&mut self) -> Kind {
        if let Some(c) = self.advance() {
            return match c {
                '@' => Kind::At,
                ',' => Kind::Comma,
                '{' => Kind::LBrace,
                '[' => Kind::LBracket,
                '(' => Kind::LParen,
                '?' => Kind::Question,
                '}' => Kind::RBrace,
                ']' => Kind::RBracket,
                ')' => Kind::RParen,
                ';' => Kind::Semi,
                '~' => Kind::Tilde,
                '#' => {
                    self.consume_while(|x| x != '\n');
                    Kind::Comment
                }
                '&' => {
                    if self.consume_if(|x| x == '&') {
                        Kind::AndAnd
                    } else if self.consume_if(|x| x == '=') {
                        Kind::AndEq
                    } else {
                        Kind::And
                    }
                }
                '^' => {
                    if self.consume_if(|x| x == '=') {
                        Kind::CaretEq
                    } else {
                        Kind::Caret
                    }
                }
                ':' => {
                    if self.consume_if(|x| x == ':') {
                        Kind::PathSep
                    } else {
                        Kind::Colon
                    }
                }
                '.' => {
                    if self.consume_if(|x| x == '.') {
                        if self.consume_if(|x| x == '=') {
                            Kind::DotDotEq
                        } else {
                            Kind::DotDot
                        }
                    } else {
                        Kind::Dot
                    }
                }
                '=' => {
                    if self.consume_if(|x| x == '=') {
                        Kind::EqEq
                    } else {
                        Kind::Eq
                    }
                }
                '>' => {
                    if self.consume_if(|x| x == '>') {
                        if self.consume_if(|x| x == '=') {
                            Kind::ShrEq
                        } else {
                            Kind::Shr
                        }
                    } else if self.consume_if(|x| x == '=') {
                        Kind::Ge
                    } else {
                        Kind::Gt
                    }
                }
                '<' => {
                    if self.consume_if(|x| x == '<') {
                        if self.consume_if(|x| x == '=') {
                            Kind::ShlEq
                        } else {
                            Kind::Shl
                        }
                    } else if self.consume_if(|x| x == '=') {
                        Kind::Le
                    } else {
                        Kind::Lt
                    }
                }
                '-' => {
                    if self.consume_if(|x| x == '=') {
                        Kind::MinusEq
                    } else if self.consume_if(|x| x == '>') {
                        Kind::RArrow
                    } else {
                        Kind::Minus
                    }
                }
                '!' => {
                    if self.consume_if(|x| x == '=') {
                        Kind::Ne
                    } else {
                        Kind::Not
                    }
                }
                '|' => {
                    if self.consume_if(|x| x == '|') {
                        Kind::OrOR
                    } else if self.consume_if(|x| x == '=') {
                        Kind::OrEq
                    } else {
                        Kind::Or
                    }
                }
                '%' => {
                    if self.consume_if(|x| x == '=') {
                        Kind::PercentEq
                    } else {
                        Kind::Percent
                    }
                }
                '+' => {
                    if self.consume_if(|x| x == '=') {
                        Kind::PlusEq
                    } else {
                        Kind::Plus
                    }
                }
                '/' => {
                    if self.consume_if(|x| x == '=') {
                        Kind::SlashEq
                    } else {
                        Kind::Slash
                    }
                }
                '*' => {
                    if self.consume_if(|x| x == '/') {
                        Kind::StarEq
                    } else {
                        Kind::Star
                    }
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let start = self.offset() - c.len_utf8();
                    self.consume_while(|x| x.is_alphanumeric() || x == '_');

                    // Detect keywords, fallback to identifier
                    match &self.source[start..self.offset()] {
                        "async" => Kind::Async,
                        "break" => Kind::Break,
                        "continue" => Kind::Continue,
                        "defer" => Kind::Defer,
                        "enum" => Kind::Enum,
                        "extend" => Kind::Extend,
                        "false" => Kind::False,
                        "fn" => Kind::Fn,
                        "for" => Kind::For,
                        "in" => Kind::In,
                        "let" => Kind::Let,
                        "loop" => Kind::Loop,
                        "match" => Kind::Match,
                        "mut" => Kind::Mut,
                        "proto" => Kind::Proto,
                        "pub" => Kind::Pub,
                        "struct" => Kind::Struct,
                        "true" => Kind::True,
                        "type" => Kind::Type,
                        "use" => Kind::Use,
                        "while" => Kind::While,
                        "with" => Kind::With,
                        "yield" => Kind::Yield,
                        _ => Kind::Identifier,
                    }
                }
                '0'..='9' => {
                    if c == '0' {
                        self.numeric_literal_starting_with_zero()
                    } else {
                        self.consume_while(|c| c.is_ascii_digit() || c == '_');
                        if self.consume_if(|c| c == '.') {
                            self.consume_while(|c| c.is_ascii_digit() || c == '_');
                            if self.consume_if(|c| c == 'e' || c == 'E') {
                                self.consume_if(|c| c == '+' || c == '-');
                                self.consume_while(|c| c.is_ascii_digit() || c == '_');
                            }
                            Kind::Float
                        } else if self.consume_if(|c| c == 'e' || c == 'E') {
                            self.consume_if(|c| c == '+' || c == '-');
                            self.consume_while(|c| c.is_ascii_digit() || c == '_');
                            Kind::Float
                        } else {
                            Kind::Integer
                        }
                    }
                }
                '"' => {
                    let mut escaped = false;
                    loop {
                        match self.advance() {
                            Some('\\') if !escaped => {
                                escaped = true;
                                continue;
                            }
                            Some('"') if !escaped => return Kind::String,
                            Some(_) => {}
                            None => return Kind::UnterminatedString,
                        }
                        escaped = false;
                    }
                }
                '\'' => {
                    let mut escaped = false;
                    let mut char_count = 0;

                    loop {
                        match self.advance() {
                            Some('\\') if !escaped => {
                                escaped = true;
                                continue;
                            }
                            Some('\'') if !escaped => {
                                if char_count == 1 {
                                    return Kind::Char;
                                } else {
                                    return Kind::InvalidChar;
                                }
                            }
                            Some(_) => {
                                char_count += 1;
                                if char_count > 1 {
                                    // Allow reading more but already invalid
                                    escaped = false;
                                } else {
                                    escaped = false;
                                }
                            }
                            None => return Kind::UnterminatedChar,
                        }
                    }
                }
                _ => Kind::Unknown,
            };
        }
        Kind::Eof
    }

    /// Reads and returns the next token, skipping whitespace.
    fn read_next_token(&mut self) -> Token<'a> {
        self.consume_while(|x| x.is_whitespace());
        let start = self.offset();
        let kind = self.read_next_kind();
        let end = self.offset();
        let lexeme = &self.source[start..end];

        Token {
            kind,
            lexeme,
            start,
            end,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token<'a>> {
        let mut tokens = Vec::new();
        loop {
            let token = self.read_next_token();
            if token.kind == Kind::Eof {
                break;
            }
            tokens.push(token);
        }
        tokens
    }

    /// Handles numeric literals starting with '0' (e.g., binary, octal, hex, float).
    fn numeric_literal_starting_with_zero(&mut self) -> Kind {
        match self.peek() {
            Some('b' | 'B') => {
                self.advance(); // Consume 'b' or 'B'
                let digits = self.consume_while(|c| c == '0' || c == '1' || c == '_');
                if digits.is_empty() {
                    // Consume any invalid chars until whitespace or valid token boundary
                    self.consume_while(|c| {
                        !c.is_whitespace()
                            && c != ';'
                            && c != ','
                            && c != '}'
                            && c != ')'
                            && c != ']'
                    });
                    Kind::InvalidNumber
                } else {
                    Kind::Binary
                }
            }
            Some('o' | 'O') => {
                self.advance(); // Consume 'o' or 'O'
                let digits = self.consume_while(|c| c >= '0' && c <= '7' || c == '_');
                if digits.is_empty() {
                    self.consume_while(|c| {
                        !c.is_whitespace()
                            && c != ';'
                            && c != ','
                            && c != '}'
                            && c != ')'
                            && c != ']'
                    });
                    Kind::InvalidNumber
                } else {
                    Kind::Octal
                }
            }
            Some('x' | 'X') => {
                self.advance(); // Consume 'x' or 'X'
                let digits = self.consume_while(|c| c.is_ascii_hexdigit() || c == '_');
                if digits.is_empty() {
                    self.consume_while(|c| {
                        !c.is_whitespace()
                            && c != ';'
                            && c != ','
                            && c != '}'
                            && c != ')'
                            && c != ']'
                    });
                    Kind::InvalidNumber
                } else {
                    Kind::Hex
                }
            }
            Some('0'..='9') => {
                self.consume_while(|c| c.is_ascii_digit() || c == '_');
                if self.consume_if(|c| c == '.') {
                    self.consume_while(|c| c.is_ascii_digit() || c == '_');
                    if self.consume_if(|c| c == 'e' || c == 'E') {
                        self.consume_if(|c| c == '+' || c == '-');
                        self.consume_while(|c| c.is_ascii_digit() || c == '_');
                    }
                    Kind::Float
                } else if self.consume_if(|c| c == 'e' || c == 'E') {
                    self.consume_if(|c| c == '+' || c == '-');
                    self.consume_while(|c| c.is_ascii_digit() || c == '_');
                    Kind::Float
                } else {
                    Kind::Octal
                }
            }
            Some('.') => {
                self.advance();
                self.consume_while(|c| c.is_ascii_digit() || c == '_');
                if self.consume_if(|c| c == 'e' || c == 'E') {
                    self.consume_if(|c| c == '+' || c == '-');
                    self.consume_while(|c| c.is_ascii_digit() || c == '_');
                }
                Kind::Float
            }
            _ => Kind::Integer,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Kind;

    fn kinds_from_source(source: &str) -> Vec<Kind> {
        Lexer::new(source)
            .tokenize()
            .into_iter()
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn test_comments() {
        let source = "# This is a comment";
        let expected = vec![Kind::Comment];
        assert_eq!(kinds_from_source(source), expected);
    }

    #[test]
    fn test_single_char_tokens() {
        let source = "@,{}[]();~";
        let expected = vec![
            Kind::At,
            Kind::Comma,
            Kind::LBrace,
            Kind::RBrace,
            Kind::LBracket,
            Kind::RBracket,
            Kind::LParen,
            Kind::RParen,
            Kind::Semi,
            Kind::Tilde,
        ];
        assert_eq!(kinds_from_source(source), expected);
    }

    #[test]
    fn test_operators() {
        let source = "& && &= ^ ^= :: : . .. ..= = == > >= >> >>= < <= << <<= - -= -> ! != | || |= % %= + += / /= *";
        let expected = vec![
            Kind::And,
            Kind::AndAnd,
            Kind::AndEq,
            Kind::Caret,
            Kind::CaretEq,
            Kind::PathSep,
            Kind::Colon,
            Kind::Dot,
            Kind::DotDot,
            Kind::DotDotEq,
            Kind::Eq,
            Kind::EqEq,
            Kind::Gt,
            Kind::Ge,
            Kind::Shr,
            Kind::ShrEq,
            Kind::Lt,
            Kind::Le,
            Kind::Shl,
            Kind::ShlEq,
            Kind::Minus,
            Kind::MinusEq,
            Kind::RArrow,
            Kind::Not,
            Kind::Ne,
            Kind::Or,
            Kind::OrOR,
            Kind::OrEq,
            Kind::Percent,
            Kind::PercentEq,
            Kind::Plus,
            Kind::PlusEq,
            Kind::Slash,
            Kind::SlashEq,
            Kind::Star,
        ];
        assert_eq!(kinds_from_source(source), expected);
    }

    #[test]
    fn test_keywords_and_identifiers() {
        let source = "async break fn struct true false let while type myVar another_var";
        let expected = vec![
            Kind::Async,
            Kind::Break,
            Kind::Fn,
            Kind::Struct,
            Kind::True,
            Kind::False,
            Kind::Let,
            Kind::While,
            Kind::Type,
            Kind::Identifier,
            Kind::Identifier,
        ];
        assert_eq!(kinds_from_source(source), expected);
    }

    #[test]
    fn test_numeric_literals() {
        let source = "0 123 0b1010 0o77 0x1F 12.34 1.0e10 1e+5 2e-2";
        let expected = vec![
            Kind::Integer, // 0
            Kind::Integer, // 123
            Kind::Binary,  // 0b1010
            Kind::Octal,   // 0o77 (assumed you return Integer for valid octals and hex)
            Kind::Hex,     // 0x1F
            Kind::Float,   // 12.34
            Kind::Float,   // 1.0e10
            Kind::Float,   // 1e+5
            Kind::Float,   // 2e-2
        ];
        assert_eq!(kinds_from_source(source), expected);
    }

    #[test]
    fn test_strings_and_chars() {
        let source = r#""hello" 'a' '\'' '"' 'ab' "unterminated"#;
        let expected = vec![
            Kind::String,
            Kind::Char,
            Kind::Char,
            Kind::Char,
            Kind::InvalidChar,
            Kind::UnterminatedString,
        ];
        assert_eq!(kinds_from_source(source), expected);
    }

    #[test]
    fn test_unknown_and_whitespace() {
        let source = " $";
        let expected = vec![Kind::Unknown];
        assert_eq!(kinds_from_source(source), expected);
    }
}
