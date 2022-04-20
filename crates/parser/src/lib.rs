pub mod ast;
pub mod expression;
pub mod precedence;

// use crate::ast::Spanned;
// use crate::ast::{
//     Annotation, AnnotationKind, BinOpKind, Expr, LiteralKind, MatchArm, PatKind, Range, UnaryOpKind,
// };
// use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
// use lexer::{tokens::TokenKind, Lexer};

// /// Parses a source string into an AST (Abstract Syntax Tree)
// ///
// /// # Examples
// ///
// /// ```
// /// use parser::Parser;
// ///
// /// let source = r#"
// /// func main() do
// ///     println("Hello, World")
// /// end
// /// "#;
// ///
// /// let mut parser = Parser::new(source, "main.lt")
// pub struct Parser<'a> {
//     source: &'a str,
//     lexer: Lexer<'a>,
//     filename: &'a str,
//     next: Option<Spanned<TokenKind>>,
// }

// impl<'a> Parser<'a> {
//     /// Constructs a new `Parser` with the given source string and filename.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// use parser::Parser;
//     ///
//     /// let source = r#"
//     /// func main() do
//     ///     println("Hello, World")
//     /// end
//     /// "#
//     ///
//     /// let mut parser = Parser::new(source, "main.lt")
//     pub fn new(source: &'a str, filename: &'a str) -> Self {
//         Self {
//             source,
//             lexer: Lexer::new(source),
//             filename,
//             next: None,
//         }
//     }

//     fn advance(&mut self) -> Spanned<TokenKind> {
//         if let Some(t) = self.next {
//             let current = t;
//             self.next = Some(self.lexer.next_token());

//             return current;
//         }

//         let current = self.lexer.next_token();
//         self.next = Some(self.lexer.next_token());

//         current
//     }

//     fn consume(&mut self, expected: TokenKind, message: &str) {
//         // Safe to unwrap since `consume` is never called in the parser before
//         // an initial `advance`, which would give `self.next` a value.
//         let next = self.next.as_ref().unwrap();
//         if next.kind != expected {
//             self.emit_error(message, next.position);
//             return;
//         }

//         self.advance(); // next `Token` is the expected one, so advance
//     }

//     fn emit_error(&self, message: &str, position: Position) {
//         Report::build(ReportKind::Error, self.filename, 12)
//             .with_message(message.to_string())
//             .with_label(
//                 Label::new((self.filename, position.span))
//                     .with_message(message)
//                     .with_color(Color::Fixed(11)),
//             )
//             .finish()
//             .print((self.filename, Source::from(self.source)))
//             .unwrap();
//     }
// }
