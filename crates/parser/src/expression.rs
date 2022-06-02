use crate::ast::{
    Annotation, AnnotationKind, BinOpKind, Expr, LiteralKind, MatchArm, PatKind, Range, UnaryOpKind,
};
use crate::errors::{ErrorKind, ParserError};
use crate::precedence::get_precedence;
use crate::Parser;
use lexer::tokens::TokenKind;
use span::{Span, Spanned};

impl<'a> Parser<'a> {
    fn prefix_rule(&mut self, token: Spanned<TokenKind>) -> Result<Spanned<Expr>, ParserError> {
        match token.0 {
            TokenKind::Integer(_)
            | TokenKind::String(_)
            | TokenKind::Char(_)
            | TokenKind::Float(_)
            | TokenKind::True
            | TokenKind::False => self.parse_literal(token),
            TokenKind::Ident(n) => Ok((Expr::Ident(n), token.1)), // No need for any extra work
            TokenKind::OpenParen => self.parse_grouping(),
            TokenKind::Minus | TokenKind::Bang => self.parse_unary(token),
            TokenKind::OpenBracket => self.parse_array(token),
            TokenKind::Do => self.parse_block(token),
            TokenKind::If => self.parse_conditional(token),
            TokenKind::For => self.parse_for(token),
            TokenKind::While => self.parse_while(token),
            TokenKind::End => Err(ParserError::new(
                ErrorKind::Unexpected(TokenKind::End, token.1),
                "Invalid Syntax - Unexpected `end`, doesn't close anything".to_string(),
                Some("Remove this `end`".to_string()),
            )),
            TokenKind::Newline => self.parse_expression(1), // Ignore newline
            _ => {
                let repr = token.0.to_string();
                Err(ParserError::new(
                    ErrorKind::Expected(vec!["expression".to_string()], token.0, token.1),
                    format!("Invalid Syntax - Expected expression, found {}", repr),
                    None,
                ))
            }
        }
    }

    fn infix_rule(&mut self, lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ParserError> {
        match self.peek().0 {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::EqualEqual
            | TokenKind::BangEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::And
            | TokenKind::Or => self.parse_binary(lhs),
            TokenKind::OpenParen => self.parse_call(lhs),
            TokenKind::Equal
            | TokenKind::PlusEqual
            | TokenKind::MinusEqual
            | TokenKind::SlashEqual
            | TokenKind::StarEqual => self.parse_assignment(lhs),
            _ => todo!(),
        }
    }

    /// Parses an expression and returns the parsed node wrapped in a `Result`.
    ///
    /// # Examples
    ///
    /// ```
    /// use parser::Parser;
    ///
    /// let mut parser = Parser::new("5 + 5", "main.lt");
    ///
    /// let (expr, span) = parser.parse_expression(1).unwrap();
    /// ```
    ///
    /// # Errors
    /// This functions returns a `ParserError` if any errors is encountered
    /// during parsing.
    pub fn parse_expression(&mut self, precedence: u8) -> Result<Spanned<Expr>, ParserError> {
        let token = self.advance();

        let mut lhs = self.prefix_rule(token)?;

        while precedence <= get_precedence(&self.peek().0) {
            lhs = self.infix_rule(lhs)?;
        }

        Ok(lhs)
    }

    fn parse_literal(&mut self, current: Spanned<TokenKind>) -> Result<Spanned<Expr>, ParserError> {
        Ok((
            match current.0 {
                TokenKind::Integer(i) => Expr::Literal(LiteralKind::Int(i.parse().unwrap())), // Safe to unwrap since value is confirmed to be valid integer
                TokenKind::Float(f) => Expr::Literal(LiteralKind::Float(f.parse().unwrap())), // Safe to unwrap since value is confirmed to be valid float
                TokenKind::String(s) => Expr::Literal(LiteralKind::String(s)),
                TokenKind::Char(c) => Expr::Literal(LiteralKind::Char(c)),
                TokenKind::True => Expr::Literal(LiteralKind::Bool(true)),
                TokenKind::False => Expr::Literal(LiteralKind::Bool(false)),
                _ => unreachable!(), // Unreachable since parse_literal is only called when the token to parse is a literal
            },
            current.1,
        ))
    }

    fn parse_function_args(&mut self) -> Result<Spanned<Vec<Spanned<Expr>>>, ParserError> {
        let mut args = Vec::new();

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

        self.consume(
            TokenKind::CloseParen,
            "Expected to find closing parenthesis `(`",
        )?;

        let span = Span::from(start.start..self.current_span().end);
        Ok((args, span))
    }

    fn parse_grouping(&mut self) -> Result<Spanned<Expr>, ParserError> {
        let expr = self.parse_expression(1)?;
        // If next token is a comma, parse as a tuple
        if self.peek().0 == TokenKind::Comma {
            return self.parse_tuple(expr);
        }

        self.consume(
            TokenKind::CloseParen,
            "Expected to find closing parenthesis `)`",
        )?;
        Ok(expr)
    }

    fn parse_tuple(&mut self, first_value: Spanned<Expr>) -> Result<Spanned<Expr>, ParserError> {
        let start = first_value.1.start; // Store start of span for later use
        let mut items = vec![first_value];

        while self.peek().0 != TokenKind::CloseParen {
            // Don't use `consume` since we don't want to error if there isn't a comma
            if self.peek().0 == TokenKind::Comma {
                self.advance();
            }

            let item = self.parse_expression(1)?;
            items.push(item);
        }

        self.consume(
            TokenKind::CloseParen,
            "Expected to find closing parenthesis `)`",
        )?;

        let span = Span::from(start - 1..items.last().unwrap().1.end + 1); // Safe to unwrap since `items` has at least one value
        Ok((Expr::Tuple(items), span))
    }

    fn parse_array(&mut self, current: Spanned<TokenKind>) -> Result<Spanned<Expr>, ParserError> {
        let mut items = Vec::new();

        while self.peek().0 != TokenKind::CloseBracket {
            let item = self.parse_expression(1)?;
            items.push(item);

            // Don't use `consume` since we don't want to error if there isn't a comma
            if self.peek().0 == TokenKind::Comma {
                self.advance();
            }
        }

        self.consume(
            TokenKind::CloseBracket,
            "Expected to find closing bracket `]`",
        )?;

        let span = Span::from(current.1.start..self.current_span().end);

        Ok((Expr::Array(items), span))
    }

    fn parse_unary(&mut self, current: Spanned<TokenKind>) -> Result<Spanned<Expr>, ParserError> {
        let expr = self.parse_expression(8)?; // 8 is the precedence level for `!` and `-` as unary operators

        let span = Span::from(current.1.start..expr.1.end);

        Ok((
            Expr::Unary {
                op: current.0.to_string(),
                rhs: Box::new(expr),
            },
            span,
        ))
    }

    fn parse_block(&mut self, current: Spanned<TokenKind>) -> Result<Spanned<Expr>, ParserError> {
        self.maybe_newline();
        let mut expressions = vec![];

        while self.peek().0 != TokenKind::End {
            expressions.push(self.parse_expression(1)?);
            self.maybe_newline();
        }
        self.consume(TokenKind::End, "Expected to find keyword `end`")?; // Consume closing `end`

        let span = Span::from(current.1.start..self.current_span().end);

        Ok((Expr::Block(expressions), span))
    }

    fn parse_conditional(
        &mut self,
        current: Spanned<TokenKind>,
    ) -> Result<Spanned<Expr>, ParserError> {
        let condition = self.parse_expression(1)?;

        // Can't use `parse_expression` or `parse_block` since if expression syntax
        // has `end` at the end of the end of the entire `if/else/else if` chain and not
        // at individual blocks.
        self.consume(TokenKind::Do, "Expected to find `do` in `if`")?;
        let span_start = self.current_span().start; // Start of span for next token
        self.maybe_newline();

        let mut body = vec![];
        while !(self.peek().0 == TokenKind::Else || self.peek().0 == TokenKind::End) {
            body.push(self.parse_expression(1)?);
            self.maybe_newline();
        }

        let span = Span::from(span_start..self.current_span().end);
        let if_body = (Expr::Block(body), span);

        let else_ = if self.peek().0 == TokenKind::Else {
            let else_position = self.advance().1; // Consume `else` token
            Some(if self.peek().0 == TokenKind::If {
                let if_t = self.advance();
                self.parse_conditional(if_t)?
            } else {
                // Don't use `consume` since we don't want to consume the next token
                if self.peek().0 != TokenKind::Do {
                    return Err(ParserError::new(
                        ErrorKind::Expected(
                            vec!["do".to_string()],
                            self.peek().0.clone(),
                            else_position,
                        ),
                        "Expected to find `do` after `else`".to_string(),
                        Some("Add a `do` after `else".to_string()),
                    ));
                }
                self.parse_expression(1)? // Always a block, since next token is confirmed to be `do`
            })
        } else {
            self.consume(TokenKind::End, "Expected to find `end` at end of block")?;
            None
        };

        let span = Span::from(current.1.start..self.current_span().end);

        Ok((
            Expr::If {
                condition: Box::new(condition),
                body: Box::new(if_body),
                else_: Box::new(else_),
            },
            span,
        ))
    }

    fn parse_for(&mut self, current: Spanned<TokenKind>) -> Result<Spanned<Expr>, ParserError> {
        let var = self.parse_expression(1)?;
        self.consume(TokenKind::In, "Expected to find `in` in `for`")?;
        let it = self.parse_expression(1)?;

        // Don't use `consume` since we don't want to consume the next token
        let peeked = self.peek();
        if peeked.0 != TokenKind::Do {
            return Err(ParserError::new(
                ErrorKind::Expected(vec!["do".to_string()], peeked.0.clone(), self.peek().1),
                "Expected to find `do` in `for`".to_string(),
                None,
            ));
        }
        let body = self.parse_expression(1)?; // Always a block since next token is confirmed to be `do`

        let span = Span::from(current.1.start..body.1.end);

        Ok((
            Expr::For {
                var: Box::new(var),
                iter: Box::from(it),
                body: Box::from(body),
            },
            span,
        ))
    }

    fn parse_while(&mut self, current: Spanned<TokenKind>) -> Result<Spanned<Expr>, ParserError> {
        let expr = self.parse_expression(1)?;

        // Don't use `consume` since we don't want to consume the next token
        let peeked = self.peek();
        if peeked.0 != TokenKind::Do {
            return Err(ParserError::new(
                ErrorKind::Expected(vec!["do".to_string()], peeked.0.clone(), self.peek().1),
                "Expected to find `do` in `while`".to_string(),
                None,
            ));
        }
        let body = self.parse_expression(1)?; // Always a block since next token is confirmed to be `do`

        let span = Span::from(current.1.start..body.1.end);

        Ok((
            Expr::While {
                expr: Box::new(expr),
                body: Box::from(body),
            },
            span,
        ))
    }

    fn parse_binary(&mut self, lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ParserError> {
        let op = self.advance();
        let precedence = get_precedence(&op.0);

        let rhs = self.parse_expression(precedence)?;
        let span = Span::from(lhs.1.start..rhs.1.end);

        Ok((
            Expr::Binary {
                op: op.0.to_string(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            span,
        ))
    }

    fn parse_call(&mut self, lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ParserError> {
        let args = self.parse_function_args()?;
        let span = Span::from(lhs.1.start..args.1.end);
        Ok((
            Expr::Call {
                callee: Box::new(lhs),
                args: args.0,
            },
            span,
        ))
    }

    fn parse_assignment(&mut self, lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ParserError> {
        let op = self.advance().0.to_string();
        let value = self.parse_expression(1)?;
        let span = Span::from(lhs.1.start..value.1.end);

        Ok((
            Expr::Assignment {
                name: Box::new(lhs),
                op,
                value: Box::new(value),
            },
            span,
        ))
    }
}
