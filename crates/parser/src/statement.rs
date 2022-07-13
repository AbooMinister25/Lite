use crate::ast::{Annotation, Expr, Statement};
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
        let mut return_annotation = None;

        let name = match self.peek().0 {
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

        let (parameters, annotations) = self.parse_function_parameters()?;

        if self.peek().0 == TokenKind::Arrow {
            self.advance();
            let expr = self.parse_expression(1)?;

            return_annotation = Some(self.parse_annotation_from(&expr.0)?);
        }

        // let body = self.parse_e
    }

    fn parse_function_parameters(
        &mut self,
    ) -> Result<Spanned<(Vec<String>, Vec<Spanned<Annotation>>)>, ParserError> {
        let mut parameters = vec![];
        let mut annotations = vec![];

        self.consume(
            TokenKind::OpenParen,
            "Expected to find opening parenthesis `(`",
        )?;

        let start = self.current_span();

        while self.peek().0 != TokenKind::CloseParen {
            if matches!(self.peek().0, TokenKind::Ident(_)) {
                return Err(ParserError::new(
                    ErrorKind::Unexpected(self.peek().0, self.peek().1),
                    "Unexpected token in function definition".to_string(),
                    None,
                ));
            }

            let param = self.parse_expression(1)?.0.to_string(); // Always an identifier
            if self.peek().0 == TokenKind::Colon {
                let annotation = self.parse_annotation()?;
                annotations.push(annotation)
            }
            parameters.push(param);

            // Don't use `consume` since we don't want to error if there isn't a comma
            if self.peek().0 == TokenKind::Comma {
                self.advance(); // Consume the comma
            }
        }

        Ok((
            (parameters, annotations),
            Span::from(start.start..self.current_span().end),
        ))
    }

    fn parse_annotation(&mut self) -> Result<Spanned<Annotation>, ParserError> {
        self.consume(
            TokenKind::Colon,
            "Expected to find colon `:` for annotation",
        )?;

        let start = self.current_span();
        let annotation = self.parse_expression(1)?;

        Ok((
            self.parse_annotation_from(&annotation.0)?,
            Span::from(start.start..self.current_span().end),
        ))
    }

    fn parse_annotation_from(&mut self, expr: &Expr) -> Result<Annotation, ParserError> {
        match expr {
            Expr::Ident(i) => Ok(Annotation::Single(*i)),
            Expr::Tuple(t) => Ok(Annotation::Tuple(
                t.iter()
                    .map(|(e, _)| self.parse_annotation_from(e))
                    .collect::<Result<Vec<Annotation>, ParserError>>()?,
            )),
            Expr::Array(a) => Ok(Annotation::Array(
                a.iter()
                    .map(|(e, _)| self.parse_annotation_from(e))
                    .collect::<Result<Vec<Annotation>, ParserError>>()?,
            )),
            e => Err(ParserError::new(
                ErrorKind::Other(
                    format!("Invalid annotation {e}, expected a single annotation, tuple annotation, or array annotation"),
                    self.current_span()
                ),
                format!(
                    "Invalid annotation {e}, expected a single annotation, tuple annotation, or array annotation"
                ),
                None
            ))
        }
    }
}
