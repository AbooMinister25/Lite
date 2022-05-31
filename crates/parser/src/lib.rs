//! The parser takes a source string and parses it into an Abstract Syntax Tree, or
//! AST. Lite uses a Pratt Parser, which allows top-down operator-precedence parsing.
//! Parsing rules for expressions and statements are separated into two different files,
//! while shared functions are defined here. Lite's parser also employs error recovery
//! through a process called synchronization, so if the parser encounters an error, it
//! can discard tokens until it hits a point at which it can safely start parsing again.

#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::must_use_candidate)]

pub mod ast;
pub mod errors;
pub mod expression;
pub mod precedence;
pub mod statement;

// use crate::ast::Spanned;
use crate::ast::{
    Annotation, AnnotationKind, BinOpKind, Expr, LiteralKind, MatchArm, PatKind, Range, Statement,
    UnaryOpKind,
};
use crate::errors::ParserError;
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportBuilder, ReportKind, Source};
use lexer::{tokens::TokenKind, Lexer};
use span::{Span, Spanned};

/// Creates and emits a parser error
///
/// # Examples
///
/// ```
/// use parser::parser_error;
///
/// parser_error("Unclosed string", 1..6, "\"Hello");
/// ```
pub fn parser_error(message: &str, span: Span, source: &str) {
    Report::build(ReportKind::Error, (), span.start)
        .with_message(message.to_string())
        .with_label(
            Label::new(span)
                .with_message(message)
                .with_color(Color::Fixed(11)),
        )
        .finish()
        .print(Source::from(source))
        .unwrap();
}

/// Parses a source string into an AST (Abstract Syntax Tree)
///
/// # Examples
///
/// ```
/// use parser::Parser;
///
/// let source = r#"
/// func main() do
///     println("Hello, World")
/// end
/// "#;
///
/// let mut parser = Parser::new(source, "main.lt");
pub struct Parser<'a> {
    source: &'a str,
    lexer: Lexer<'a>,
    filename: &'a str,
    current_token_span: Span,
    peeked: Option<Spanned<TokenKind>>,
}

impl<'a> Parser<'a> {
    /// Constructs a new `Parser` with the given source string and filename.
    ///
    /// # Examples
    ///
    /// ```
    /// use parser::Parser;
    ///
    /// let source = r#"
    /// func main() do
    ///     println("Hello, World")
    /// end
    /// "#;
    ///
    /// let mut parser = Parser::new(source, "main.lt");
    pub fn new(source: &'a str, filename: &'a str) -> Self {
        Self {
            source,
            lexer: Lexer::new(source),
            filename,
            current_token_span: Span::from(0..0),
            peeked: None,
        }
    }

    /// Parses and returns a tuple consisting of the parsed AST and any
    /// errors that may have occured.
    ///
    /// # Examples
    ///
    /// ```
    /// use parser::Parser;
    ///
    /// let source = r#"
    /// func main() do
    ///     println("Hello, World")
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
            // Consume newlines
            if self.peek().0 == TokenKind::Newline {
                self.advance();
                continue; // Don't use a while loop since we want the parser to stop if it has reached the end of input
            }

            let node = self.parse_statement();
            match node {
                Ok(n) => nodes.push(n),
                Err(e) => errors.push(e),
            };
        }

        (nodes, errors)
    }

    pub fn at_end(&mut self) -> bool {
        self.peek().0 == TokenKind::EoF
    }

    fn advance(&mut self) -> Spanned<TokenKind> {
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
        if self.peeked.is_none() {
            self.peeked = Some(self.advance());
        }

        self.peeked.as_ref().unwrap()
    }

    fn consume(&mut self, expected: TokenKind, message: &str) {
        let token = self.peek();

        if token.0 == expected {
            self.advance(); // next `Token` is the expected one, so advance
            return;
        }

        parser_error(message, token.1, self.source);
    }

    fn maybe_newline(&mut self) {
        if self.peek().0 == TokenKind::Newline {
            self.advance();
        }
    }

    const fn current_span(&self) -> Span {
        self.current_token_span
    }
}
