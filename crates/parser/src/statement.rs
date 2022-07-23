use crate::ast::Statement;
use crate::errors::{ErrorKind, ParserError};
use crate::Parser;
use lexer::tokens::TokenKind;
use span::{Span, Spanned};

type StatementResult = Result<Spanned<Statement>, ParserError>;

impl<'a> Parser<'a> {
    /// Parses a statement
    ///
    /// # Examples
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
    /// let (statement, span) = parser.parse_statement().expect("Parser encountered an error");
    /// ```
    ///
    /// # Errors
    /// This functions returns a `ParserError` if any errors are encountered
    /// during parsing.
    pub fn parse_statement(&mut self) -> StatementResult {
        let peeked = self.peek();

        match peeked.0 {
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> StatementResult {
        let expr = self.parse_expression(1)?;
        let span = expr.1;

        Ok((Statement::Expression(expr), span))
    }
}
