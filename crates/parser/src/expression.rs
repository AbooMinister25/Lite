use crate::ast::{Expr, LiteralKind};
use crate::errors::{ErrorKind, ParserError};
use crate::precedence::get_precedence;
use crate::Parser;
use lexer::tokens::TokenKind;
use span::{Span, Spanned};

type ExprResult = Result<Spanned<Expr>, ParserError>;

impl<'a> Parser<'a> {
    /// Parses an expression.
    ///
    /// # Examples
    ///
    /// ```
    /// use parser::Parser;
    ///
    /// let mut parser = Parser::new("5 + 5", "main.lt")
    /// let (expr, span) = parser.parse_expression(1).expect("Parser encountered an error");
    /// ```
    ///
    /// # Errors
    /// This function returns a `ParserError` if any errors are
    /// encountered during parsing.
    pub fn parse_expression(&mut self, precedence: u8) -> ExprResult {
        let token = self.advance();
        let lhs = self.prefix_rule(token)?;

        Ok(lhs)
    }

    fn prefix_rule(&mut self, token: Spanned<TokenKind>) -> ExprResult {
        match token.0 {
            TokenKind::Integer(_)
            | TokenKind::String(_)
            | TokenKind::Char(_)
            | TokenKind::Float(_)
            | TokenKind::True
            | TokenKind::False => self.parse_literal(token),
            TokenKind::Ident(n) => Ok((Expr::Ident(n), token.1)),
            TokenKind::OpenParen => self.parse_grouping(),
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

    fn parse_literal(&mut self, current: Spanned<TokenKind>) -> ExprResult {
        Ok((
            match current.0 {
                TokenKind::Integer(i) => Expr::Literal(LiteralKind::Int(i.parse().unwrap())), // Safe to unwrap since value is confirmed to be valid integer
                TokenKind::Float(f) => Expr::Literal(LiteralKind::Float(f.parse().unwrap())), // Safe to unwrap since value is confirmed to be valid float
                TokenKind::String(s) => Expr::Literal(LiteralKind::String(s)),
                TokenKind::Char(c) => Expr::Literal(LiteralKind::Char(c)),
                TokenKind::True => Expr::Literal(LiteralKind::Bool(true)),
                TokenKind::False => Expr::Literal(LiteralKind::Bool(false)),
                _ => unreachable!("parse_literal is only called when `current` is a literal"),
            },
            current.1,
        ))
    }

    fn parse_grouping(&mut self) -> ExprResult {
        let expr = self.parse_expression(1)?;

        // If next token is a comma, parse as a tuple
        if self.peek().0 == TokenKind::Comma {
            return self.parse_tuple(expr);
        }

        self.consume(
            &TokenKind::CloseParen,
            "Expected to find closing parenthesis `)`",
        )?;
        Ok(expr)
    }

    fn parse_tuple(&mut self, first_value: Spanned<Expr>) -> ExprResult {
        let start = first_value.1.start;
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
            &TokenKind::CloseParen,
            "Expected to find a closing parenthesis `)`",
        )?;

        let span = Span::from(start - 1..self.current_token_span.end);
        Ok((Expr::Tuple(items), span))
    }
}
