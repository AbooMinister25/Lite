pub mod ast;
pub mod expression;
pub mod precedence;

use crate::ast::Spanned;
use crate::ast::{
    Annotation, AnnotationKind, BinOpKind, Expr, LiteralKind, MatchArm, PatKind, Range, UnaryOpKind,
};
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportBuilder, ReportKind, Source};
use lexer::{tokens::TokenKind, Lexer};

/// Creates and emits a parser error
///
/// # Examples
///
/// ```
/// use parser::parser_error;
///
/// parser_error("Unclosed string", 1..6, "\"Hello");
/// ```
pub fn parser_error(message: &str, span: std::ops::Range<usize>, source: &str) {
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
            peeked: None,
        }
    }

    pub fn at_end(&mut self) -> bool {
        self.peek().0 == TokenKind::EoF
    }

    fn advance(&mut self) -> Spanned<TokenKind> {
        match self.peeked.take() {
            Some(t) => t,
            None => self.lexer.next_token(),
        }
    }

    fn peek(&mut self) -> &Spanned<TokenKind> {
        if self.peeked.is_none() {
            self.peeked = Some(self.advance());
            self.peeked.as_ref().unwrap()
        } else {
            self.peeked.as_ref().unwrap()
        }
    }

    fn consume(&mut self, expected: TokenKind, message: &str) {
        let token = self.peek();

        if token.0 == expected {
            self.advance(); // next `Token` is the expected one, so advance
            return;
        }

        parser_error(message, token.1.clone(), self.source);
    }
}
