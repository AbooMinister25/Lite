//! The lexer takes some source string and generates a stream
//! of `TokenKind`s `TokenKind`s are any meaningful "word" or "character".
//! Meaningful in the sense that things akin to whitespace and comments
//! are filtered out, as they would end up being unnecessary junk for the parser.

#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::must_use_candidate)]

pub mod tokens;

use std::{iter::Peekable, str::Chars};

use crate::tokens::TokenKind;
use span::{Span, Spanned};
use unicode_xid::UnicodeXID;

fn get_keyword(name: &str) -> TokenKind {
    match name {
        "and" => TokenKind::And,
        "class" => TokenKind::Class,
        "do" => TokenKind::Do,
        "else" => TokenKind::Else,
        "end" => TokenKind::End,
        "false" => TokenKind::False,
        "for" => TokenKind::For,
        "func" => TokenKind::Func,
        "if" => TokenKind::If,
        "in" => TokenKind::In,
        "import" => TokenKind::Import,
        "let" => TokenKind::Let,
        "match" => TokenKind::Match,
        "mut" => TokenKind::Mut,
        "new" => TokenKind::New,
        "or" => TokenKind::Or,
        "return" => TokenKind::Return,
        "trait" => TokenKind::Trait,
        "true" => TokenKind::True,
        "while" => TokenKind::While,
        "with" => TokenKind::With,
        _ => TokenKind::Ident(name.to_string()),
    }
}

/// Generates a stream of `Token`s from some UTF-8 encoded
/// string.
///
/// The job of the `Lexer` is to generate tokens on-demand
/// from the given source string, filtering out unnecessary
/// items, such as whitespace or comments.
///
/// # Examples
///
/// ```
/// use lexer::Lexer;
///
/// let source = r#"
/// func main do
///     println("Hello, World")
/// end
/// "#;
///
/// let mut lexer = Lexer::new(source);
/// let (current, next) = lexer.next_token();
/// ```
pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    pub position: usize,
    line: u32,
    column: u32,
}

impl<'a> Lexer<'a> {
    /// Constructs a new `Lexer` from the given source string.
    ///
    /// # Examples
    ///
    /// ```
    /// use lexer::Lexer;
    ///
    /// let source = r#"
    /// func main do
    ///     println("Hello, World")
    /// end
    /// "#;
    ///
    /// let mut lexer = Lexer::new(source);
    /// ```
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.chars().peekable(),
            position: 0,
            line: 1,
            column: 0,
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
                ';' => self.create_token(TokenKind::Semicolon, 1),
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

                c => self.create_token(TokenKind::Error(format!("Unkown character {}", c)), 1),
            };
        }

        self.create_token(TokenKind::EoF, 0)
    }

    /// Wrapper around `lex_token` which lexes and returns the
    /// current`Token` in the token stream
    ///
    /// # Examples
    /// ```
    /// use lexer::Lexer;
    ///
    /// let source = r#"
    /// func main do
    ///     println("Hello, World")
    /// end
    /// "#;
    ///
    /// let mut lexer = Lexer::new(source);
    /// let tok = lexer.next_token();
    /// ```
    pub fn next_token(&mut self) -> Spanned<TokenKind> {
        self.lex_token()
    }
}
