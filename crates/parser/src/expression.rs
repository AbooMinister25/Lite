use crate::ast::{Expr, LiteralKind};
use crate::error::ParserError;
use crate::precedence::get_precedence;
use crate::Parser;
use lexer::token::TokenKind;
use span::{Span, Spanned};

type ExprResult = Result<Spanned<Expr>, ParserError>;

impl<'a> Parser<'a> {
    /// Parses an expression
    ///
    /// # Errors
    /// This function returns a `ParserError` if any errors are
    /// encountered during parsing.
    pub fn parse_expression(&mut self, precedence: u8) -> ExprResult {
        let token = self.advance();
        let mut lhs = self.prefix_rule(token)?;

        while precedence <= get_precedence(&self.peek().0) {
            lhs = self.infix_rule(lhs)?;
        }

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
            TokenKind::Ident(n) => Ok((Expr::Ident(n), token.1)), // No need for any extra work
            TokenKind::OpenParen => self.parse_grouping(),
            TokenKind::Minus | TokenKind::Bang => self.parse_unary(&token),
            TokenKind::OpenBracket => self.parse_array(&token),
            _ => todo!(),
        }
    }

    fn infix_rule(&mut self, lhs: Spanned<Expr>) -> ExprResult {
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

    fn parse_literal(&self, current: Spanned<TokenKind>) -> ExprResult {
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
            // Consume a comma if we haven't reached the end of the tuple
            if self.peek().0 != TokenKind::CloseBracket {
                self.consume(&TokenKind::Comma, "Expected to find a comma")?;
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

    fn parse_unary(&mut self, current: &Spanned<TokenKind>) -> ExprResult {
        // 8 is the precedence level for the `!` and `-` unary operators.
        let expr = self.parse_expression(8)?;
        let span = Span::from(current.1.start..expr.1.end);

        Ok((
            Expr::Unary {
                op: current.0.to_string(),
                rhs: Box::new(expr),
            },
            span,
        ))
    }

    fn parse_array(&mut self, current: &Spanned<TokenKind>) -> ExprResult {
        let mut items = Vec::new();

        while self.peek().0 != TokenKind::CloseBracket {
            let item = self.parse_expression(1)?;
            items.push(item);

            // Consume a comma if we haven't reached the end of the array
            if self.peek().0 != TokenKind::CloseBracket {
                self.consume(&TokenKind::Comma, "Expected to find a comma")?;
            }
        }

        self.consume(
            &TokenKind::CloseBracket,
            "Expected to find closing bracket `]`",
        )?;

        let span = Span::from(current.1.start..self.current_token_span.end);
        Ok((Expr::Array(items), span))
    }

    fn parse_binary(&mut self, lhs: Spanned<Expr>) -> ExprResult {
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

    fn parse_call(&mut self, lhs: Spanned<Expr>) -> ExprResult {
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

    fn parse_function_args(&mut self) -> Result<Spanned<Vec<Spanned<Expr>>>, ParserError> {
        let mut args = Vec::new();
        self.consume(
            &TokenKind::OpenParen,
            "Expected to find opening parenthesis `(`",
        )?;

        let start = self.current_token_span.start;

        while self.peek().0 != TokenKind::CloseParen {
            let arg = self.parse_expression(1)?;
            args.push(arg);

            // Consume a comma if we haven't reached the end of the argument list
            if self.peek().0 != TokenKind::CloseParen {
                self.consume(&TokenKind::Comma, "Expected to find a comma")?;
            }
        }

        self.consume(
            &TokenKind::CloseParen,
            "Expected to find closing parenthesis `(`",
        )?;

        let span = Span::from(start..self.current_token_span.end);
        Ok((args, span))
    }

    fn parse_assignment(&mut self, lhs: Spanned<Expr>) -> ExprResult {
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

#[cfg(test)]
mod tests {
    use super::*;

    fn check(string: &str, with: &[&str]) {
        let mut parser = Parser::new(string, "test");
        let mut it = with.iter();

        while !parser.at_end() {
            let res = parser.parse_expression(1).unwrap();
            assert_eq!(&res.0.to_string(), it.next().unwrap());
        }
    }

    #[test]
    fn test_literals() {
        check(
            "10 3.5 true false \"foo\" 'c'",
            &[
                "(Int 10)",
                "(Float 3.5)",
                "(Bool true)",
                "(Bool false)",
                "(String foo)",
                "(Char c)",
            ],
        );
    }

    #[test]
    fn test_identifiers() {
        check(
            "hello_world _ _foo a",
            &[
                "(Ident hello_world)",
                "(Ident _)",
                "(Ident _foo)",
                "(Ident a)",
            ],
        );
    }

    #[test]
    fn test_tuples() {
        check(
            "(1, 'b', \"hello\")",
            &["(Tuple (Int 1), (Char b), (String hello))"],
        );
        check("((1, 2, 3), (4, 5, 6), (7, (8, 9)))", &["(Tuple (Tuple (Int 1), (Int 2), (Int 3)), (Tuple (Int 4), (Int 5), (Int 6)), (Tuple (Int 7), (Tuple (Int 8), (Int 9))))"]);
    }

    #[test]
    fn test_array() {
        check(
            "[1, 'b', \"hello\"] [[1, 2, 3], [4, 5, 6], [7, [8, 9]]]",
            &[
                "(Array (Int 1), (Char b), (String hello))",
                "(Array (Array (Int 1), (Int 2), (Int 3)), (Array (Int 4), (Int 5), (Int 6)), (Array (Int 7), (Array (Int 8), (Int 9))))"
            ],
        );
    }

    #[test]
    fn test_unary() {
        check("!10", &["(Unary (! (Int 10)))"]);
        check("-20", &["(Unary (- (Int 20)))"]);
    }

    #[test]
    fn test_binary() {
        check("5 + 5", &["(Binary (+ (Int 5) (Int 5)))"]);
        check("5 + 5 * 2 / (3 - 8)", &["(Binary (+ (Int 5) (Binary (* (Int 5) (Binary (/ (Int 2) (Binary (- (Int 3) (Int 8)))))))))"]);
    }

    #[test]
    fn parse_assignment() {
        check("foo = 10", &["(Assignment (= (Ident foo) (Int 10)))"]);
        check("foo += 10", &["(Assignment (+= (Ident foo) (Int 10)))"]);
        check("foo -= 10", &["(Assignment (-= (Ident foo) (Int 10)))"]);
        check("foo /= 10", &["(Assignment (/= (Ident foo) (Int 10)))"]);
        check("foo *= 10", &["(Assignment (*= (Ident foo) (Int 10)))"]);
    }
}
