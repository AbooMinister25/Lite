#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::unnecessary_wraps)]

pub mod ast;
pub mod error;
pub mod expression;
pub mod precedence;
pub mod statement;

use crate::error::ParserError;

use ast::Statement;
use lexer::{token::TokenKind, Lexer};
use span::{Span, Spanned};

/// Parses a string into an Abstract Syntax Tree (AST)
pub struct Parser<'a> {
    source: &'a str,
    lexer: Lexer<'a>,
    filename: &'a str,
    current_token_span: Span,
    peeked: Option<Spanned<TokenKind>>,
}

impl<'a> Parser<'a> {
    /// Constructs a new `Parser`.
    pub fn new(source: &'a str, filename: &'a str) -> Self {
        Self {
            source,
            lexer: Lexer::new(source),
            filename,
            current_token_span: Span::from(0..0),
            peeked: None,
        }
    }

    /// Parses and returns a tuple of the parsed AST, and any errors that may have
    /// occurred.
    ///
    /// # Examples
    ///
    /// ```
    /// use parser::Parser;
    ///
    /// let source = r#"
    /// fun main() do
    ///     println("Hello World")
    /// end
    /// "#;
    ///
    /// let mut parser = Parser::new(source, "main.lt");
    /// let (ast, errors) = parser.parse();
    /// ```
    pub fn parse(&mut self) -> (Vec<Spanned<Statement>>, Vec<ParserError>) {
        let mut nodes = vec![];
        let mut errors = vec![];

        while !self.at_end() {
            let node = self.parse_statement();
            match node {
                Ok(n) => nodes.push(n),
                Err(e) => {
                    errors.push(e);
                    self.synchronize();
                }
            }
        }

        (nodes, errors)
    }

    fn synchronize(&mut self) {
        while !self.at_end() {
            match self.peek().0 {
                TokenKind::Fun
                | TokenKind::Let
                | TokenKind::Return
                | TokenKind::Import
                | TokenKind::Trait
                | TokenKind::If
                | TokenKind::For
                | TokenKind::While
                | TokenKind::Match => break,
                _ => self.advance(),
            };
        }
    }

    fn advance(&mut self) -> Spanned<TokenKind> {
        // If a token has been peeked, return that, otherwise advance the lexer and return the next token
        if let Some(t) = self.peeked.take() {
            self.current_token_span = t.1;
            t
        } else {
            let t = self.lexer.next_token();
            self.current_token_span = t.1;
            t
        }
    }

    fn peek(&mut self) -> &Spanned<TokenKind> {
        // If nothing has been peeked, advance and store that token as the peeked value
        if self.peeked.is_none() {
            self.peeked = Some(self.advance());
        }

        self.peeked.as_ref().unwrap()
    }

    fn at_end(&mut self) -> bool {
        self.peek().0 == TokenKind::EoF
    }

    fn consume(&mut self, expected: &TokenKind, message: &str) -> Result<(), ParserError> {
        let token = self.peek();

        if token.0 == *expected {
            self.advance(); //  next token was the expected one, so advance
            return Ok(());
        }

        Err(ParserError::new(message.to_string(), token.1))
    }
}
