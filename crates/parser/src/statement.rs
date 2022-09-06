use crate::ast::{Annotation, Expr, Statement};
use crate::errors::{ErrorKind, ParserError};
use crate::Parser;
use lexer::tokens::TokenKind;
use span::{Span, Spanned};

type StatementResult = Result<Spanned<Statement>, ParserError>;
type FunctionParamResult = Result<Spanned<(Vec<String>, Vec<Spanned<Annotation>>)>, ParserError>;

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
            TokenKind::Func => self.parse_function(),
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

    fn parse_function(&mut self) -> StatementResult {
        let span_start = self.advance().1;

        // TODO: Implement public visibility modifier in lexer and here

        // Set the precedence level high because we don't want to parse anything beyond an ident
        // TODO: Parse a pattern here instead once patterns are implemented.
        let name = self.parse_expression(10)?;
        let (parameters, annotations) = self.parse_function_parameters()?.0;

        let return_annotation = if self.peek().0 == TokenKind::Arrow {
            self.advance();
            Some(self.parse_annotation()?)
        } else {
            None
        };

        if self.peek().0 != TokenKind::Do {
            return Err(ParserError::new(
                ErrorKind::Expected(vec!["do".to_string()], TokenKind::Do, self.peek().1),
                "Expected to find `do` in function".to_string(),
                None,
            ));
        }

        // Always a block, since next token is confirmed to be `do`
        let body = self.parse_expression(1)?;
        let span = Span::from(span_start.start..self.current_token_span.end);

        Ok((
            Statement::Function {
                name,
                public: false,
                params: parameters,
                annotations,
                return_annotation,
                body,
            },
            span,
        ))
    }

    fn parse_function_parameters(&mut self) -> FunctionParamResult {
        let mut parameters = vec![];
        let mut annotations = vec![];

        self.consume(
            &TokenKind::OpenParen,
            "Expected to find opening parenthesis `(`",
        )?;

        let start = self.current_token_span;

        while self.peek().0 != TokenKind::CloseParen {
            if !matches!(self.peek().0, TokenKind::Ident(_)) {
                return Err(ParserError::new(
                    ErrorKind::Unexpected(self.peek().0.clone(), self.peek().1),
                    "Unexpected token in function declaration".to_string(),
                    None,
                ));
            }

            let param = self.parse_expression(1)?.0.to_string(); // Always an identifier
            self.consume(
                &TokenKind::Colon,
                "Expected to find `:` after parameter name for annotation",
            )?;

            let annotation = self.parse_annotation()?;
            annotations.push(annotation);

            parameters.push(param);

            // Consume a comma if we haven't reached the end of the parameter list
            if self.peek().0 != TokenKind::CloseParen {
                self.consume(&TokenKind::Comma, "Expected to find a comma")
                    .map_err(|e| e.with_help("Did you forget a comma?".to_string()))?;
            }
        }

        self.consume(
            &TokenKind::CloseParen,
            "Expected to find closing parenthesis `)`",
        )?;

        Ok((
            (parameters, annotations),
            Span::from(start.start..self.current_token_span.end),
        ))
    }

    fn parse_annotation(&mut self) -> Result<Spanned<Annotation>, ParserError> {
        let start = self.current_token_span;
        let annotation = self.parse_expression(1)?;

        Ok((
            self.parse_annotation_from(&annotation.0)?,
            Span::from(start.start..self.current_token_span.end),
        ))
    }

    fn parse_annotation_from(&self, expr: &Expr) -> Result<Annotation, ParserError> {
        match expr {
            Expr::Ident(i) => Ok(Annotation::Single(i.clone())),
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
                    format!(
                        "Invalid annotation {e}, expected a single, tuple, or array annotation"
                    ),
                    self.current_token_span,
                ),
                format!("Invalid annotation {e}, expected a single, tuple, or array annotation"),
                None,
            )),
        }
    }
}
