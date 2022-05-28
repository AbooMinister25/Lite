use crate::ast::Spanned;
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportBuilder, ReportKind, Source};
use lexer::tokens::TokenKind;
use std::fmt;
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

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::Expected(expected, _) => {
                let message = if expected.len() == 1 {
                    format!("Expected to find token {}", expected[0])
                } else {
                    format!(
                        "Expected to find one of {:?}",
                        expected
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                };

                write!(f, "{}", message)
            }
            ParserError::Unclosed(token, _) => write!(f, "Unclosed delimiter {}", token),
            ParserError::Unexpected(token, _) => write!(f, "Unexpected token {}", token),
            ParserError::Other(message, _) => write!(f, "{}", message),
        }
    }
}
