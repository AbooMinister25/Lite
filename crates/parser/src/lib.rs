#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::must_use_candidate)]

pub mod ast;
pub mod error;
pub mod precedence;

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
}
