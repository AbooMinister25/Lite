use crate::ast::{Expr, Statement};
use crate::errors::ParserError;
use crate::Parser;
use lexer::tokens::TokenKind;
use span::{Span, Spanned};

impl<'a> Parser<'a> {
    pub fn parse_statement(&mut self) -> Result<Spanned<Statement>, ParserError> {
        let peeked = self.peek();

        match peeked.0 {
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> Result<Spanned<Statement>, ParserError> {
        let expr = self.parse_expression(1)?;
        let span = Span::from(expr.1.start..expr.1.end);
        self.maybe_newline();
        Ok((Statement::Expression(expr), span))
    }
}
