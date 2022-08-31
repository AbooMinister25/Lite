use crate::ast::{Expr, Statement};
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
            TokenKind::Let => self.parse_let(),
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> StatementResult {
        let expr = self.parse_expression(1)?;
        let span = expr.1;

        Ok((Statement::Expression(expr), span))
    }

    fn parse_let(&mut self) -> StatementResult {
        let span_start = self.advance().1;
        let mutable = if self.peek().0 == TokenKind::Mut {
            self.advance();
            true
        } else {
            false
        };

        // Set the precedence level high because we don't want to parse anything beyond an ident
        // TODO: Parse a pattern here instead (when patterns are implemented).
        let name = self.parse_expression(10)?;
        self.consume(&TokenKind::Equal, "Expected to find `=`")?;
        let value = self.parse_expression(1)?;

        let span = Span::from(span_start.start..value.1.end);
        Ok((
            Statement::Let {
                name,
                value,
                mutable,
            },
            span,
        ))
    }
}
