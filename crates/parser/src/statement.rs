use crate::ast::{Expr, Statement};
use crate::errors::{ErrorKind, ParserError};
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

    fn parse_function(&mut self) -> Result<Spanned<Statement>, ParserError> {
        let span = self.advance().1;

        let i = match self.peek().0 {
            TokenKind::Ident(i) => i,
            _ => {
                return Err(ParserError::new(
                    ErrorKind::Expected(
                        vec!["identifier".to_string()],
                        self.peek().0,
                        self.peek().1,
                    ),
                    format!(
                        "Expected to find identifier, instead found {}",
                        self.peek().0
                    ),
                    None,
                ));
            }
        };

        let foo = 10;
    }

    fn parse_function_parameters(&mut self) -> Result<Spanned<Vec<String>>, ParserError> {
        let mut args = vec![];

        self.consume(
            TokenKind::OpenParen,
            "Expected to find opening parenthesis `(`",
        )?;

        let start = self.current_span();

        while self.peek().0 != TokenKind::CloseParen {
            let arg = self.parse_expression(1)?;
            args.push(arg);

            // Don't use `consume` since we don't want to error if there isn't a comma
            if self.peek().0 == TokenKind::Comma {
                self.advance(); // Consume the comma
            }
        }
    }
}
