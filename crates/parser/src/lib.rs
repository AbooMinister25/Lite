//! This file contains the main interface to the parser. The parser takes a
//! string and outputs an Abstract Syntax Tree (AST). Lite's parser is implemented
//! as a Pratt parser, and allows for top-down operator precedence parsing.

#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::missing_const_for_fn)]

pub mod ast;
pub mod errors;
pub mod expression;
pub mod precedence;
pub mod statement;

use crate::ast::Statement;
use crate::errors::{ErrorKind, ParserError};
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportBuilder, ReportKind, Source};
use lexer::{tokens::TokenKind, Lexer};
use span::{Span, Spanned};

/// Parses a string into an Abstract Syntax Tree (AST)
///
/// # Examples
///
/// ```
/// use parser::Parser;
///
/// let source = r#"
/// func main() do
///     println("Hello World")
/// end
/// "#;
///
/// let mut parser = Parser::new(source, "main.lt")
/// ```
pub struct Parser<'a> {
    source: &'a str,
    lexer: Lexer<'a>,
    filename: &'a str,
    current_token_span: Span,
    peeked: Option<Spanned<TokenKind>>,
}

impl<'a> Parser<'a> {
    /// Constructs a new `Parser`.
    ///
    /// # Examples
    ///
    /// ```
    /// use parser::Parser;
    ///
    /// let source = r#"
    /// func main() do
    ///     println("Hello World")
    /// end
    /// "#;
    ///
    /// let mut parser = Parser::new(source, "main.lt");
    /// ```
    pub fn new(source: &'a str, filename: &'a str) -> Self {
        Self {
            source,
            lexer: Lexer::new(source),
            filename,
            current_token_span: Span::from(0..0),
            peeked: None,
        }
    }

    /// Parses and returns a tuple of the parsed AST and any errors
    /// that may have occurred.
    ///
    /// # Examples
    ///
    /// ```
    /// use parser::Parser;
    ///
    /// let source = r#"
    /// func main() do
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
                Err(e) => errors.push(e),
            };
        }

        (nodes, errors)
    }

    fn at_end(&mut self) -> bool {
        self.peek().0 == TokenKind::EoF
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

    fn consume(&mut self, expected: &TokenKind, message: &str) -> Result<(), ParserError> {
        let token = self.peek();

        if token.0 == *expected {
            self.advance(); //  next token was the expected one, so advance
            return Ok(());
        }

        Err(ParserError::new(
            ErrorKind::Expected(vec![expected.to_string()], token.0.clone(), token.1),
            message.to_string(),
            None,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, LiteralKind};

    #[test]
    fn its_alive() {
        let mut parser = Parser::new("5", "main.lt");
        let (ast, errors) = parser.parse();

        assert!(errors.is_empty());
        assert_eq!(
            ast,
            vec![(
                Statement::Expression((Expr::Literal(LiteralKind::Int(5)), Span::from(0..1))),
                Span::from(0..1)
            )]
        );
    }
}
