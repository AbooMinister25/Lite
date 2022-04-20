use crate::ast::{
    Annotation, AnnotationKind, BinOpKind, Expr, LiteralKind, MatchArm, PatKind, Range, Spanned,
    UnaryOpKind,
};
use crate::precedence::get_precedence;
use chumsky::{prelude::*, stream::Stream};
use lexer::tokens::TokenKind;

pub fn parse_expression() -> impl Parser<TokenKind, Spanned<Expr>, Error = Simple<TokenKind>> {
    recursive(|expr| {
        let literal = select! {
            TokenKind::String(s) => Expr::Literal(LiteralKind::String(s)),
            TokenKind::Char(c) => Expr::Literal(LiteralKind::Char(c)),
            TokenKind::Integer(i) => Expr::Literal(LiteralKind::Int(i.parse().unwrap())),
            TokenKind::Float(f) => Expr::Literal(LiteralKind::Float(f.parse().unwrap())),
            TokenKind::True => Expr::Literal(LiteralKind::Bool(true)),
            TokenKind::False => Expr::Literal(LiteralKind::Bool(false)),
        }
        .labelled("literal");

        let ident = select! {
            TokenKind::Ident(i) => Expr::Ident(i)
        }
        .labelled("identifier");

        let items = expr
            .clone()
            .separated_by(just(TokenKind::Comma))
            .allow_trailing();

        let tuple = items
            .clone()
            .delimited_by(just(TokenKind::OpenParen), just(TokenKind::CloseParen))
            .map(Expr::Tuple);

        let list = items
            .clone()
            .delimited_by(just(TokenKind::OpenBracket), just(TokenKind::CloseBracket))
            .map(Expr::Array);

        let assignment = ident
            .clone()
            .then_ignore(just(TokenKind::Equal))
            .then(expr.clone())
            .map(|(name, value)| Expr::Assignment {
                name: name.to_string(),
                value: Box::new(value),
            });

        literal
            .or(ident)
            .or(assignment)
            .or(tuple)
            .or(list)
            .map_with_span(|e, s| (e, s))
            .or(expr
                .clone()
                .delimited_by(just(TokenKind::OpenParen), just(TokenKind::CloseParen)))
    })
}

// impl<'a> Parser<'a> {
//     fn prefix_rule(&mut self, token: Token) -> Option<Result<Expr, ()>> {
//         match token.kind {
//             TokenKind::Integer(_)
//             | TokenKind::String(_)
//             | TokenKind::Char(_)
//             | TokenKind::Float(_)
//             | TokenKind::True
//             | TokenKind::False => Some(self.parse_literal(token)),
//             TokenKind::Ident(_) => Some(self.parse_ident(token)),
//             _ => {
//                 self.emit_error(
//                     &format!("Invalid Syntax - Expected expression, found {}", token.kind),
//                     token.position,
//                 );
//                 None
//             }
//         }
//     }

//     /// Parses an expression
//     ///
//     /// Parses an expression and returns the parsed node wrapped in a `Result`
//     /// indicating whether or not the parser encountered an error.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// use parser::Parser;
//     ///
//     /// let mut parser = Parser::new("5 + 5", "main.lt");
//     ///
//     /// let expr = parser.parse_expression().unwrap();
//     pub fn parse_expression(&mut self, precedence: u8) -> Result<Expr, ()> {
//         let token = self.advance();
//         let kind = &token.kind;

//         if let Some(lhs) = self.prefix_rule(token) {
//             return lhs;
//         }

//         Err(())
//     }

//     fn parse_literal(&mut self, current: Token) -> Result<Expr, ()> {
//         Ok(match current.kind {
//             TokenKind::Integer(i) => Expr::Literal {
//                 value: LiteralKind::Int(i.parse().unwrap()), // Safe to unwrap since value is confirmed to be valid integer
//                 position: current.position,
//             },
//             TokenKind::Float(f) => Expr::Literal {
//                 value: LiteralKind::Float(f.parse().unwrap()), // Safe to unwrap since value is confirmed to be valid float
//                 position: current.position,
//             },
//             TokenKind::String(s) => Expr::Literal {
//                 value: LiteralKind::String(s),
//                 position: current.position,
//             },
//             TokenKind::Char(c) => Expr::Literal {
//                 value: LiteralKind::Char(c),
//                 position: current.position,
//             },
//             TokenKind::True => Expr::Literal {
//                 value: LiteralKind::Bool(true),
//                 position: current.position,
//             },
//             TokenKind::False => Expr::Literal {
//                 value: LiteralKind::Bool(false),
//                 position: current.position,
//             },
//             _ => unreachable!(), // Unreachable since parse_literal is only called when the token to parse is a literal
//         })
//     }

//     fn parse_ident(&mut self, current: Token) -> Result<Expr, ()> {
//         if let TokenKind::Ident(name) = current.kind {
//             // Safe to unwrap since this function is never called in the parser before
//             // an initial `advance`, which would give `self.next` a value.
//             return Ok(match self.next.as_ref().unwrap().kind {
//                 TokenKind::Equal => {
//                     self.advance();
//                     let value = self.parse_expression(1)?;

//                     Expr::Assignment {
//                         name,
//                         value: Box::new(value),
//                         position: current.position,
//                     }
//                 }
//                 TokenKind::OpenParen => {
//                     let args = self.parse_function_args()?;
//                     Expr::Call {
//                         name,
//                         args,
//                         position: current.position,
//                     }
//                 }
//                 _ => Expr::Literal {
//                     value: LiteralKind::Ident(name),
//                     position: current.position,
//                 },
//             });
//         }

//         unreachable!(); // Unreachable since `current` will always be of kind Ident
//     }

//     fn parse_function_args(&mut self) -> Result<Vec<Expr>, ()> {
//         let mut args = Vec::new();

//         self.consume(
//             TokenKind::OpenParen,
//             "Expected to find opening parenthesis `(`",
//         );

//         // Safe to unwrap since this function is never called in the parser before
//         // an initial `advance`, which would give `self.next` a value.
//         while self.next.as_ref().unwrap().kind != TokenKind::CloseParen {
//             let arg = self.parse_expression(1)?;
//             args.push(arg);

//             if self.next.as_ref().unwrap().kind == TokenKind::Comma {
//                 self.advance(); // Consume the comma
//             }
//         }

//         self.consume(
//             TokenKind::CloseParen,
//             "Expected to find closing parenthesis `(`",
//         );
//         Ok(args)
//     }
// }
