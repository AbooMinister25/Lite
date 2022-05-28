use crate::ast::Spanned;
use lexer::tokens::TokenKind;
use std::fmt::Display;
use std::ops::Range;

#[derive(Debug)]
pub enum ParserError {
    /// Expected one of the given tokens, found something else
    Expected(Vec<TokenKind>, Range<usize>),
    /// An unclosed delimeter
    Unclosed(TokenKind, Range<usize>),
    /// Found an unexpected token
    Unexpected(TokenKind, Range<usize>),
    /// Another type of error occured with the given message
    Other(String, Range<usize>),
}
