use crate::ast::{
    Annotation, AnnotationKind, BinOpKind, Expr, LiteralKind, MatchArm, PatKind, Range, Spanned,
    UnaryOpKind,
};
use crate::precedence::get_precedence;
use crate::{parser_error, Parser};
use chumsky::{prelude::*, stream::Stream};
use lexer::tokens::TokenKind;

pub type Span = std::ops::Range<usize>;

// pub fn parse_expression() -> impl Parser<TokenKind, Spanned<Expr>, Error = Simple<TokenKind>> {
//     recursive(|expr| {
//         let literal = select! {
//             TokenKind::String(s) => Expr::Literal(LiteralKind::String(s)),
//             TokenKind::Char(c) => Expr::Literal(LiteralKind::Char(c)),
//             TokenKind::Integer(i) => Expr::Literal(LiteralKind::Int(i.parse().unwrap())),
//             TokenKind::Float(f) => Expr::Literal(LiteralKind::Float(f.parse().unwrap())),
//             TokenKind::True => Expr::Literal(LiteralKind::Bool(true)),
//             TokenKind::False => Expr::Literal(LiteralKind::Bool(false)),
//         }
//         .labelled("literal");

//         let ident = select! {
//             TokenKind::Ident(i) => Expr::Ident(i)
//         }
//         .labelled("identifier");

//         let items = expr
//             .clone()
//             .separated_by(just(TokenKind::Comma))
//             .allow_trailing();

//         let tuple = items
//             .clone()
//             .delimited_by(just(TokenKind::OpenParen), just(TokenKind::CloseParen))
//             .map(Expr::Tuple);

//         let list = items
//             .clone()
//             .delimited_by(just(TokenKind::OpenBracket), just(TokenKind::CloseBracket))
//             .map(Expr::Array);

//         let assignment = ident
//             .then_ignore(just(TokenKind::Equal))
//             .then(expr.clone())
//             .map(|(name, value)| Expr::Assignment {
//                 name: name.to_string(),
//                 value: Box::new(value),
//             });

//         let call = ident
//             .map_with_span(|e, span: Span| (e, span))
//             .then(
//                 items
//                     .delimited_by(just(TokenKind::OpenParen), just(TokenKind::CloseParen))
//                     .map_with_span(|args, span: Span| (args, span))
//                     .repeated(),
//             )
//             .foldl(|e, args| {
//                 let span = e.1.start..args.1.end;
//                 (
//                     Expr::Call {
//                         name: Box::new(e),
//                         args: args.0,
//                     },
//                     span,
//                 )
//             })
//             .map(|e| e.0);

//         let atom = literal
//             .or(ident)
//             .or(assignment)
//             .or(call)
//             .or(tuple)
//             .or(list)
//             .map_with_span(|e, s| (e, s))
//             .or(expr
//                 .clone()
//                 .delimited_by(just(TokenKind::OpenParen), just(TokenKind::CloseParen)));

//         let unary = just(TokenKind::Minus)
//             .repeated()
//             .then(atom.clone())
//             .foldr(|op_, rhs| {
//                 let span = rhs.1.clone();
//                 (
//                     Expr::Unary {
//                         op: op_.to_string(),
//                         rhs: Box::new(rhs),
//                     },
//                     span,
//                 )
//             });

//         let product = unary
//             .clone()
//             .then(
//                 just(TokenKind::Star)
//                     .or(just(TokenKind::Slash))
//                     .then(unary)
//                     .repeated(),
//             )
//             .foldl(|lhs, (op, rhs)| {
//                 let span = rhs.1.clone();
//                 (
//                     Expr::Binary {
//                         op: op.to_string(),
//                         lhs: Box::new(lhs),
//                         rhs: Box::new(rhs),
//                     },
//                     span,
//                 )
//             });

//         let sum = product
//             .clone()
//             .then(
//                 just(TokenKind::Plus)
//                     .or(just(TokenKind::Minus))
//                     .then(product)
//                     .repeated(),
//             )
//             .foldl(|lhs, (op, rhs)| {
//                 let span = rhs.1.clone();
//                 (
//                     Expr::Binary {
//                         op: op.to_string(),
//                         lhs: Box::new(lhs),
//                         rhs: Box::new(rhs),
//                     },
//                     span,
//                 )
//             });

//         let compare = sum
//             .clone()
//             .then(
//                 choice((
//                     just(TokenKind::EqualEqual),
//                     just(TokenKind::BangEqual),
//                     just(TokenKind::Greater),
//                     just(TokenKind::GreaterEqual),
//                     just(TokenKind::Less),
//                     just(TokenKind::LessEqual),
//                 ))
//                 .then(sum)
//                 .repeated(),
//             )
//             .foldl(|lhs, (op, rhs)| {
//                 let span = rhs.1.clone();
//                 (
//                     Expr::Binary {
//                         op: op.to_string(),
//                         lhs: Box::new(lhs),
//                         rhs: Box::new(rhs),
//                     },
//                     span,
//                 )
//             });

//         let block = expr
//             .clone()
//             .repeated()
//             .delimited_by(just(TokenKind::Do), just(TokenKind::End))
//             .map_with_span(|body, span| (Expr::Block(body), span));

//         let if_ = recursive(|if_| {
//             just(TokenKind::If)
//                 .ignore_then(expr.clone())
//                 .then(block.clone().or(expr.clone()))
//                 .then(
//                     just(TokenKind::Else)
//                         .ignore_then(block.clone().or(if_))
//                         .or_not(),
//                 )
//                 .map_with_span(|((cond, body), else_), span| {
//                     (
//                         Expr::If {
//                             condition: Box::new(cond),
//                             body: Box::new(body),
//                             else_: Box::new(else_),
//                         },
//                         span,
//                     )
//                 })
//         });

//         let for_ = just(TokenKind::For)
//             .ignore_then(ident.map_with_span(|i, span: Span| (i, span)))
//             .then_ignore(just(TokenKind::In))
//             .then(expr.clone())
//             .then(block.clone().or(expr.clone()))
//             .map_with_span(|((var, iter), body), span: Span| {
//                 (
//                     Expr::For {
//                         var: Box::new(var),
//                         iter: Box::new(iter),
//                         body: Box::new(body),
//                     },
//                     span,
//                 )
//             });

//         let while_ = just(TokenKind::While)
//             .ignore_then(expr.clone())
//             .then(block.clone().or(expr.clone()))
//             .map_with_span(|(e, body), span: Span| {
//                 (
//                     Expr::While {
//                         expr: Box::new(e),
//                         body: Box::new(body),
//                     },
//                     span,
//                 )
//             });

//         atom.or(compare).or(block).or(if_).or(for_).or(while_)
//     })
// }

impl<'a> Parser<'a> {
    fn prefix_rule(&mut self, token: Spanned<TokenKind>) -> Option<Result<Spanned<Expr>, ()>> {
        match token.0 {
            TokenKind::Integer(_)
            | TokenKind::String(_)
            | TokenKind::Char(_)
            | TokenKind::Float(_)
            | TokenKind::True
            | TokenKind::False => Some(self.parse_literal(token)),
            TokenKind::Ident(_) => Some(self.parse_ident(token)),
            TokenKind::OpenParen => Some(self.parse_grouping()),
            TokenKind::Minus => Some(self.parse_unary(token)),
            TokenKind::Bang => Some(self.parse_unary(token)),
            TokenKind::OpenBracket => Some(self.parse_array(token)),
            _ => {
                parser_error(
                    &format!("Invalid Syntax - Expected expression, found {}", token.0),
                    token.1,
                    self.source,
                );
                None
            }
        }
    }

    fn infix_rule(&mut self, lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ()> {
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
            _ => todo!(),
        }
    }

    /// Parses an expression and returns the parsed node wrapped in a `Result`
    /// indicating whether or not the parser encountered an error.
    ///
    /// # Examples
    ///
    /// ```
    /// use parser::Parser;
    ///
    /// let mut parser = Parser::new("5 + 5", "main.lt");
    ///
    /// let (expr, span) = parser.parse_expression().unwrap();
    pub fn parse_expression(&mut self, precedence: u8) -> Result<Spanned<Expr>, ()> {
        let token = self.advance();

        if let Some(mut lhs) = self.prefix_rule(token) {
            while precedence < get_precedence(&self.peek().0) {
                lhs = self.infix_rule(lhs?);
            }

            return lhs;
        }

        Err(())
    }

    fn parse_literal(&mut self, current: Spanned<TokenKind>) -> Result<Spanned<Expr>, ()> {
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

    fn parse_ident(&mut self, current: Spanned<TokenKind>) -> Result<Spanned<Expr>, ()> {
        if let TokenKind::Ident(name) = current.0 {
            return Ok(match self.peek().0 {
                TokenKind::Equal => {
                    self.advance();
                    let value = self.parse_expression(1)?;

                    let span = current.1.start..value.1.end;

                    (
                        Expr::Assignment {
                            name,
                            value: Box::new(value),
                        },
                        span,
                    )
                }
                TokenKind::OpenParen => {
                    let args = self.parse_function_args()?;
                    (
                        Expr::Call { name, args: args.0 },
                        current.1.start..args.1.end,
                    )
                }
                _ => (Expr::Ident(name), current.1),
            });
        }

        unreachable!(); // Unreachable since `current` will always be of kind Ident
    }

    fn parse_function_args(&mut self) -> Result<Spanned<Vec<Spanned<Expr>>>, ()> {
        let mut args = Vec::new();

        self.consume(
            TokenKind::OpenParen,
            "Expected to find opening parenthesis `(`",
        );

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
        );

        let span = if args.is_empty() {
            self.lexer.position..self.lexer.position + 1
        } else {
            self.lexer.position..args.last().unwrap().1.end // Safe to unwrap since `args` is confirmed to not be empty
        };

        Ok((args, span))
    }

    fn parse_grouping(&mut self) -> Result<Spanned<Expr>, ()> {
        let expr = self.parse_expression(1)?;
        // If next token is a comma, parse as a tuple
        if self.peek().0 == TokenKind::Comma {
            return self.parse_tuple(expr);
        }

        self.consume(
            TokenKind::CloseParen,
            "Expected to find closing parenthesis `)`",
        );
        Ok(expr)
    }

    fn parse_tuple(&mut self, first_value: Spanned<Expr>) -> Result<Spanned<Expr>, ()> {
        let start = first_value.1.start; // Store start of span for later use
        let mut items = vec![first_value];

        while self.peek().0 != TokenKind::CloseParen {
            let item = self.parse_expression(1)?;
            items.push(item);

            // Don't use `consume` since we don't want to error if there isn't a comma
            if self.peek().0 == TokenKind::Comma {
                self.advance();
            }
        }

        self.consume(
            TokenKind::CloseParen,
            "Expected to find closing parenthesis `)`",
        );

        let span = start - 1..items.last().unwrap().1.end + 1; // Safe to unwrap since `items` has at least one value
        Ok((Expr::Tuple(items), span))
    }

    fn parse_array(&mut self, current: Spanned<TokenKind>) -> Result<Spanned<Expr>, ()> {
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
        );

        let span = if items.is_empty() {
            current.1.start..current.1.start + 1
        } else {
            current.1.start..items.last().unwrap().1.end // Safe to unwrap since `args` is confirmed to not be empty
        };

        Ok((Expr::Array(items), span))
    }

    fn parse_unary(&mut self, current: Spanned<TokenKind>) -> Result<Spanned<Expr>, ()> {
        let expr = self.parse_expression(8)?; // 8 is the precedence level for `!` and `-` as unary operators

        let span = current.1.start..expr.1.end;

        Ok((
            Expr::Unary {
                op: current.0.to_string(),
                rhs: Box::new(expr),
            },
            span,
        ))
    }

    fn parse_binary(&mut self, lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ()> {
        let op = self.advance();
        let precedence = get_precedence(&op.0);

        let rhs = self.parse_expression(precedence)?;
        let span = lhs.1.start..rhs.1.end;

        Ok((
            Expr::Binary {
                op: op.0.to_string(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            span,
        ))
    }
}
