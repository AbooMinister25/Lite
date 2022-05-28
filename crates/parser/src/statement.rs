use crate::ast::{Expr, Spanned, Statement};
use crate::{parser_error, Parser};
use lexer::tokens::TokenKind;

impl<'a> Parser<'a> {
    pub fn parse_statement(&mut self) -> Result<Spanned<Statement>, ()> {
        let peeked = self.peek();

        match peeked.0 {
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> Result<Spanned<Statement>, ()> {
        let expr = self.parse_expression(1)?;
        let span = expr.1.start..expr.1.end;
        Ok((Statement::Expression(expr), span))
    }
}
