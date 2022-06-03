use crate::ast::{Expr, Statement};
use crate::errors::ParserError;
use crate::Parser;
use lexer::tokens::TokenKind;
use span::{Span, Spanned};

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
    pub fn parse_statement(&mut self) -> Result<Spanned<Statement>, ParserError> {
        let peeked = self.peek();

        match peeked.0 {
            TokenKind::Return => self.parse_return(),
            TokenKind::Import => self.parse_import(),
            TokenKind::Let => self.parse_let(),
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> Result<Spanned<Statement>, ParserError> {
        let expr = self.parse_expression(1)?;
        let span = Span::from(expr.1.start..expr.1.end);
        self.maybe_newline();
        Ok((Statement::Expression(expr), span))
    }

    fn parse_return(&mut self) -> Result<Spanned<Statement>, ParserError> {
        let span_start = self.advance().1;
        let expr = self.parse_expression(1)?;

        let span = Span::from(span_start.start..expr.1.end);
        Ok((Statement::Return(expr), span))
    }

    fn parse_import(&mut self) -> Result<Spanned<Statement>, ParserError> {
        let span_start = self.advance().1;
        let expr = self.parse_expression(1)?;

        let span = Span::from(span_start.start..expr.1.end);
        Ok((Statement::Import(expr), span))
    }

    fn parse_let(&mut self) -> Result<Spanned<Statement>, ParserError> {
        let span_start = self.advance().1;
        let mutable = if self.peek().0 == TokenKind::Mut {
            self.advance();
            true
        } else {
            false
        };

        let name = self.parse_expression(1)?;
        self.consume(TokenKind::Equal, "Expected to find `=`")?;
        let value = self.parse_expression(1)?;

        let span = Span::from(span_start.start..value.1.end);
        Ok((
            (Statement::Let {
                name,
                value,
                mutable,
            }),
            span,
        ))
    }
}
