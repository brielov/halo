use crate::token::{Kind, Token};
use std::collections::VecDeque;
use std::str::Chars;

enum LexerState {
    Code,
    StringLiteral,
}

pub struct Lexer<'a> {
    chars: Chars<'a>,
    source: &'a str,
    state_stack: VecDeque<LexerState>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars(),
            source,
            state_stack: VecDeque::from([LexerState::Code]),
        }
    }

    fn offset(&self) -> usize {
        self.source.len() - self.chars.as_str().len()
    }

    fn peek(&self) -> Option<char> {
        self.chars.as_str().chars().next()
    }

    fn peek_next(&self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next()
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next()
    }

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

    fn consume_while<F>(&mut self, predicate: F) -> &'a str
    where
        F: Fn(char) -> bool,
    {
        let start = self.offset();
        while self.consume_if(&predicate) {}
        &self.source[start..self.offset()]
    }

    fn read_code_kind(&mut self) -> Kind {
        if let Some(c) = self.advance() {
            match c {
                '"' => Kind::StringStart,
                '}' => {
                    if self.state_stack.len() >= 2
                        && matches!(
                            self.state_stack.get(self.state_stack.len() - 2),
                            Some(LexerState::StringLiteral)
                        )
                    {
                        Kind::InterpolationEnd
                    } else {
                        Kind::RBrace
                    }
                }
                '@' => Kind::At,
                ',' => Kind::Comma,
                '{' => Kind::LBrace,
                '[' => Kind::LBracket,
                '(' => Kind::LParen,
                '?' => Kind::Question,
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
                    if self.consume_if(|x| x == '=') {
                        Kind::StarEq
                    } else {
                        Kind::Star
                    }
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let start = self.offset() - c.len_utf8();
                    self.consume_while(|x| x.is_alphanumeric() || x == '_');
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
                                return if char_count == 1 {
                                    Kind::Char
                                } else {
                                    Kind::InvalidChar
                                };
                            }
                            Some(_) => {
                                char_count += 1;
                                escaped = false;
                            }
                            None => return Kind::UnterminatedChar,
                        }
                    }
                }
                _ => Kind::Unknown,
            }
        } else {
            Kind::Eof
        }
    }

    fn read_string_literal_part(&mut self) -> Option<&'a str> {
        let start = self.offset();
        let mut escaped = false;
        while let Some(c) = self.peek() {
            if escaped {
                self.advance();
                escaped = false;
            } else if c == '\\' {
                self.advance();
                escaped = true;
            } else if c == '"' || (c == '#' && self.peek_next() == Some('{')) {
                break;
            } else {
                self.advance();
            }
        }
        let end = self.offset();
        // Only return Some if we have content and didn’t hit EOF
        if end > start && self.peek().is_some() {
            Some(&self.source[start..end])
        } else {
            None
        }
    }

    fn read_next_token(&mut self) -> Token<'a> {
        self.consume_while(|x| x.is_whitespace());
        let start = self.offset();

        if self.peek().is_none() {
            if matches!(self.state_stack.back(), Some(LexerState::StringLiteral)) {
                self.state_stack.pop_back();
                let end = self.offset();
                return Token {
                    kind: Kind::UnterminatedString,
                    lexeme: &self.source[start..end],
                    start,
                    end,
                };
            }
            self.state_stack.clear();
            return Token {
                kind: Kind::Eof,
                lexeme: "",
                start,
                end: start,
            };
        }

        match self.state_stack.back() {
            Some(LexerState::Code) => {
                let kind = self.read_code_kind();
                let end = self.offset();
                let lexeme = &self.source[start..end];
                let token = Token {
                    kind,
                    lexeme,
                    start,
                    end,
                };
                match token.kind {
                    Kind::StringStart => self.state_stack.push_back(LexerState::StringLiteral),
                    Kind::InterpolationEnd => {
                        self.state_stack.pop_back();
                    }
                    _ => {}
                }
                token
            }
            Some(LexerState::StringLiteral) => {
                if let Some(literal) = self.read_string_literal_part() {
                    let end = self.offset();
                    Token {
                        kind: Kind::StringLiteral,
                        lexeme: literal,
                        start,
                        end,
                    }
                } else if self.peek() == Some('"') {
                    self.advance();
                    let end = self.offset();
                    self.state_stack.pop_back();
                    Token {
                        kind: Kind::StringEnd,
                        lexeme: &self.source[start..end],
                        start,
                        end,
                    }
                } else if self.peek() == Some('#') && self.peek_next() == Some('{') {
                    self.advance();
                    self.advance();
                    let end = self.offset();
                    self.state_stack.push_back(LexerState::Code);
                    Token {
                        kind: Kind::InterpolationStart,
                        lexeme: &self.source[start..end],
                        start,
                        end,
                    }
                } else {
                    // Reached EOF or no valid terminator
                    let end = self.offset();
                    self.state_stack.pop_back();
                    Token {
                        kind: Kind::UnterminatedString,
                        lexeme: &self.source[start..end],
                        start,
                        end,
                    }
                }
            }
            None => Token {
                kind: Kind::Eof,
                lexeme: "",
                start: self.offset(),
                end: self.offset(),
            },
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token<'a>> {
        let mut tokens = Vec::new();
        loop {
            let token = self.read_next_token();
            tokens.push(token.clone());
            if token.kind == Kind::Eof {
                break;
            }
        }
        tokens
    }

    fn numeric_literal_starting_with_zero(&mut self) -> Kind {
        match self.peek() {
            Some('b' | 'B') => {
                self.advance();
                let digits = self.consume_while(|c| c == '0' || c == '1' || c == '_');
                if digits.is_empty() {
                    Kind::InvalidNumber
                } else {
                    Kind::Binary
                }
            }
            Some('o' | 'O') => {
                self.advance();
                let digits = self.consume_while(|c| c >= '0' && c <= '7' || c == '_');
                if digits.is_empty() {
                    Kind::InvalidNumber
                } else {
                    Kind::Octal
                }
            }
            Some('x' | 'X') => {
                self.advance();
                let digits = self.consume_while(|c| c.is_ascii_hexdigit() || c == '_');
                if digits.is_empty() {
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
        let expected = vec![Kind::Comment, Kind::Eof];
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
            Kind::Eof,
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
            Kind::Eof,
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
            Kind::Eof,
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
            Kind::Eof,
        ];
        assert_eq!(kinds_from_source(source), expected);
    }

    #[test]
    fn test_strings_and_interpolation() {
        let source =
            r#""hello" "world #{x + 1}!" "\"escaped\"" "nested #{ "inner #{y}" }" "unterminated"#;
        let expected = vec![
            Kind::StringStart,
            Kind::StringLiteral, // "hello"
            Kind::StringEnd,
            Kind::StringStart,
            Kind::StringLiteral,      // "world "
            Kind::InterpolationStart, // "#{"
            Kind::Identifier,         // "x"
            Kind::Plus,               // "+"
            Kind::Integer,            // "1"
            Kind::InterpolationEnd,   // "}"
            Kind::StringLiteral,      // "!"
            Kind::StringEnd,
            Kind::StringStart,
            Kind::StringLiteral, // "\"escaped\""
            Kind::StringEnd,
            Kind::StringStart,
            Kind::StringLiteral,      // "nested "
            Kind::InterpolationStart, // "#{"
            Kind::StringStart,
            Kind::StringLiteral,      // "inner "
            Kind::InterpolationStart, // "#{"
            Kind::Identifier,         // "y"
            Kind::InterpolationEnd,   // "}"
            Kind::StringEnd,
            Kind::InterpolationEnd, // "}"
            Kind::StringEnd,
            Kind::StringStart,
            Kind::UnterminatedString,
            Kind::Eof,
        ];
        assert_eq!(kinds_from_source(source), expected);
    }

    #[test]
    fn test_unknown_and_whitespace() {
        let source = " $";
        let expected = vec![Kind::Unknown, Kind::Eof];
        assert_eq!(kinds_from_source(source), expected);
    }
}
