//! The lexer takes some source string and generates a stream
//! of `TokenKind`s. `TokenKind`s are used to represent the most basic meaningful
//! piece of information in some Lite source code, a token. Tokens are sort
//! of like the "word"s in Lite. They're meaningful in the sense that things
//! akin to whitespace and comments are filtered out, as they would end up
//! being unnecessary junk for the parser.

#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::must_use_candidate)]

pub mod token;

use std::iter::Peekable;
use std::str::Chars;

use span::{Span, Spanned};
use unicode_xid::UnicodeXID;

use crate::token::TokenKind;

fn get_keyword(name: &str) -> TokenKind {
    match name {
        "do" => TokenKind::Do,
        "else" => TokenKind::Else,
        "end" => TokenKind::End,
        "false" => TokenKind::False,
        "for" => TokenKind::For,
        "fun" => TokenKind::Fun,
        "if" => TokenKind::If,
        "in" => TokenKind::In,
        "import" => TokenKind::Import,
        "let" => TokenKind::Let,
        "match" => TokenKind::Match,
        "mut" => TokenKind::Mut,
        "pub" => TokenKind::Pub,
        "return" => TokenKind::Return,
        "trait" => TokenKind::Trait,
        "true" => TokenKind::True,
        "type" => TokenKind::Type,
        "while" => TokenKind::While,
        _ => TokenKind::Ident(name.to_string()),
    }
}

/// Generates a stream of `TokenKind`s from some UTF-8 encoded
/// string.
///
/// The job of the `Lexer` is to generate tokens on-demand
/// from the given source string, filtering out unnecessary
/// items, such as whitespace or comments.
pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    line: u32,
    column: u32,
    pub position: usize,
}

impl<'a> Lexer<'a> {
    /// Constructs a new `Lexer` from the given source string.
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.chars().peekable(),
            line: 1,
            column: 0,
            position: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.position += 1;
        self.source.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.source.peek()
    }

    fn consume(&mut self, expected: char) -> bool {
        if let Some(c) = self.peek() {
            if c == &expected {
                self.advance();
                return true;
            }
        }

        false
    }

    pub fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn create_token(&mut self, kind: TokenKind, len: usize) -> Spanned<TokenKind> {
        (kind, Span::from(self.position - len..self.position))
    }

    fn lex_string(&mut self) -> Spanned<TokenKind> {
        let mut value = String::new();

        // Safe to unwrap since && will short circuit
        while !self.at_end() && self.peek().unwrap() != &'"' {
            value.push(self.advance().unwrap()); // Safe to unwrap since not the end of input
        }

        let len = value.len();

        // If at the end of the source string and closing quote not found, return an error token
        if self.at_end() {
            return self.create_token(
                TokenKind::Error("Unterminated string literal. Expected closing quote, instead found EoF (End of File)".to_string()), 
                len
            );
        }

        self.advance(); // Consume closing quote
        self.create_token(TokenKind::String(value), len)
    }

    fn lex_char(&mut self) -> Spanned<TokenKind> {
        if self.at_end() {
            return self.create_token(
                TokenKind::Error("Unterminated character literal. Expected closing quote `'`, instead found EoF (End of File)".to_string()), 
                1
            );
        }

        let value = self.advance().unwrap(); // Safe to unwrap since not at end of source

        if !self.consume('\'') {
            let mut closed = false;

            while !self.at_end() {
                // Safe to unwrap since not at end
                if self.peek().unwrap() == &'\'' {
                    closed = true;
                    self.advance();
                    break;
                }

                self.advance();
            }

            return self.create_token(
                if closed {
                    TokenKind::Error(
                        "Character literals can only contain one codepoint".to_string(),
                    )
                } else {
                    TokenKind::Error("Unterminated character literal. Expected closing quote `'`, instead found EoF (End of File)".to_string())
                },
                1,
            );
        }

        self.create_token(TokenKind::Char(value), 1)
    }

    fn lex_number(&mut self, first_char: char) -> Spanned<TokenKind> {
        let mut is_integer = true; // True if the number is an integer, set to false if number is found to be a float
        let mut value = String::from(first_char);

        // Safe to unwrap since && will short circuit
        while !self.at_end() && self.peek().unwrap().is_numeric() {
            value.push(self.advance().unwrap()); // Safe to unwrap since not the end of input
        }

        if let Some(c) = self.peek() {
            if c == &'.' {
                is_integer = false; // Set is_integer to false since dot indicates value is a float
                value.push(self.advance().unwrap()); // Safe to unwrap since not end of input

                // Safe to unwrap since && will short circuit
                while !self.at_end() && self.peek().unwrap().is_numeric() {
                    value.push(self.advance().unwrap()); // Safe to unwrap since not the end of input
                }
            }
        }

        let len = value.len();

        self.create_token(
            if is_integer {
                TokenKind::Integer(value)
            } else {
                TokenKind::Float(value)
            },
            len,
        )
    }

    fn lex_identifier(&mut self, first_char: char) -> Spanned<TokenKind> {
        let mut value = String::from(first_char);

        if self.at_end() {
            return self.create_token(TokenKind::Ident(value), 1);
        }

        // Add to the value as long as next character is valid identifier
        while !self.at_end() && UnicodeXID::is_xid_continue(*self.peek().unwrap()) {
            value.push(self.advance().unwrap());
        }

        let token_type = get_keyword(&value); // Check whether token is a Lite keyword
        self.create_token(token_type, value.len())
    }

    fn lex_token(&mut self) -> Spanned<TokenKind> {
        if let Some(c) = self.advance() {
            return match c {
                // Punctuation
                '(' => self.create_token(TokenKind::OpenParen, 1),
                ')' => self.create_token(TokenKind::CloseParen, 1),
                '[' => self.create_token(TokenKind::OpenBracket, 1),
                ']' => self.create_token(TokenKind::CloseBracket, 1),
                '{' => self.create_token(TokenKind::OpenBrace, 1),
                '}' => self.create_token(TokenKind::CloseBrace, 1),
                ',' => self.create_token(TokenKind::Comma, 1),
                '.' => self.create_token(TokenKind::Dot, 1),
                ':' => self.create_token(TokenKind::Colon, 1),

                // Operators
                '=' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::EqualEqual, 2)
                    } else {
                        self.create_token(TokenKind::Equal, 1)
                    }
                }
                '!' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::BangEqual, 2)
                    } else {
                        self.create_token(TokenKind::Bang, 1)
                    }
                }
                '>' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::GreaterEqual, 2)
                    } else {
                        self.create_token(TokenKind::Greater, 1)
                    }
                }
                '<' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::LessEqual, 2)
                    } else {
                        self.create_token(TokenKind::Less, 1)
                    }
                }
                '+' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::PlusEqual, 2)
                    } else {
                        self.create_token(TokenKind::Plus, 1)
                    }
                }
                '-' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::MinusEqual, 2)
                    } else if self.consume('>') {
                        self.create_token(TokenKind::Arrow, 2)
                    } else {
                        self.create_token(TokenKind::Minus, 1)
                    }
                }
                '*' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::StarEqual, 2)
                    } else {
                        self.create_token(TokenKind::Star, 1)
                    }
                }
                '/' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::SlashEqual, 2)
                    } else if self.consume('/') {
                        // Safe to unwrap since lexer is never at the end of input inside the loop
                        while !self.at_end() {
                            if self.peek().unwrap() == &'\n' {
                                break;
                            }
                            self.advance();
                        }
                        self.lex_token()
                    } else {
                        self.create_token(TokenKind::Slash, 1)
                    }
                }

                // Literals
                '"' => self.lex_string(),
                '\'' => self.lex_char(),
                c if c.is_numeric() => self.lex_number(c),
                c if UnicodeXID::is_xid_start(c) || c == '_' => self.lex_identifier(c),

                // Whitespace
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                    self.lex_token()
                }
                c if c.is_whitespace() => self.lex_token(),

                c => self.create_token(TokenKind::Error(format!("Unknown character {c}")), 1),
            };
        }

        self.create_token(TokenKind::EoF, 0)
    }

    /// Wrapper around `lex_token` which lexes and returns the
    /// current`Token` in the token stream
    pub fn next_token(&mut self) -> Spanned<TokenKind> {
        self.lex_token()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Spanned<TokenKind>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lex_token();
        token.0.ne(&TokenKind::EoF).then_some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_string(string: &str) -> Vec<TokenKind> {
        let lexer = Lexer::new(string);
        lexer.map(|s| s.0).collect()
    }

    #[test]
    fn test_punctuation() {
        let tokens = lex_string("()[]{},.:");
        assert_eq!(
            tokens,
            &[
                TokenKind::OpenParen,
                TokenKind::CloseParen,
                TokenKind::OpenBracket,
                TokenKind::CloseBracket,
                TokenKind::OpenBrace,
                TokenKind::CloseBrace,
                TokenKind::Comma,
                TokenKind::Dot,
                TokenKind::Colon
            ]
        );
    }

    #[test]
    fn test_operators() {
        let tokens = lex_string("= == ! != > >= < <= + += - -= * *= / /=");
        assert_eq!(
            tokens,
            &[
                TokenKind::Equal,
                TokenKind::EqualEqual,
                TokenKind::Bang,
                TokenKind::BangEqual,
                TokenKind::Greater,
                TokenKind::GreaterEqual,
                TokenKind::Less,
                TokenKind::LessEqual,
                TokenKind::Plus,
                TokenKind::PlusEqual,
                TokenKind::Minus,
                TokenKind::MinusEqual,
                TokenKind::Star,
                TokenKind::StarEqual,
                TokenKind::Slash,
                TokenKind::SlashEqual,
            ]
        );
    }

    #[test]
    fn test_literals() {
        let tokens = lex_string("\"Hello World\" 'a' 10 3.5");
        assert_eq!(
            tokens,
            &[
                TokenKind::String("Hello World".to_owned()),
                TokenKind::Char('a'),
                TokenKind::Integer("10".to_owned()),
                TokenKind::Float("3.5".to_owned())
            ]
        );
    }

    #[test]
    fn test_identifiers() {
        let tokens = lex_string("hello_world _ _foo a");
        assert_eq!(
            tokens,
            &[
                TokenKind::Ident("hello_world".to_owned()),
                TokenKind::Ident("_".to_owned()),
                TokenKind::Ident("_foo".to_owned()),
                TokenKind::Ident("a".to_owned())
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let tokens = lex_string(
            "do else false for fun if in import let mut pub return trait true type while",
        );
        assert_eq!(
            tokens,
            &[
                TokenKind::Do,
                TokenKind::Else,
                TokenKind::False,
                TokenKind::For,
                TokenKind::Fun,
                TokenKind::If,
                TokenKind::In,
                TokenKind::Import,
                TokenKind::Let,
                TokenKind::Mut,
                TokenKind::Pub,
                TokenKind::Return,
                TokenKind::Trait,
                TokenKind::True,
                TokenKind::Type,
                TokenKind::While
            ]
        );
    }

    #[test]
    fn test_errors() {
        let unterminated_str = lex_string("\"hello");
        let unterminated_char = lex_string("'a");
        let invalid_char = lex_string("'abc'");
        let unknown = lex_string("@");

        assert_eq!(unterminated_str.first().unwrap(), &TokenKind::Error("Unterminated string literal. Expected closing quote, instead found EoF (End of File)".to_owned()));
        assert_eq!(unterminated_char.first().unwrap(), &TokenKind::Error("Unterminated character literal. Expected closing quote `'`, instead found EoF (End of File)".to_owned()));
        assert_eq!(
            invalid_char.first().unwrap(),
            &TokenKind::Error("Character literals can only contain one codepoint".to_owned())
        );
        assert_eq!(
            unknown.first().unwrap(),
            &TokenKind::Error("Unknown character @".to_owned())
        );
    }
}
