use ariadne::ReportKind;
use error::LiteError;
use lexer::tokens::TokenKind;
use span::{Span, Spanned};
use std::{error::Error, fmt};

/// Represents the different types of errors the
/// parser might encounter
#[derive(Debug)]
pub enum ErrorKind {
    /// Expected one of the given items, found something else
    Expected(Vec<String>, TokenKind, Span),
    /// An unclosed delimeter
    Unclosed(TokenKind, Span),
    /// Found an unexpected token
    Unexpected(TokenKind, Span),
    /// Another type of error occured with the given message
    Other(String, Span),
}

/// This struct holds information related to an error
/// in the parser, including its type, the error
/// message, and an optional help message
#[derive(Debug)]
pub struct ParserError {
    kind: ErrorKind,
    message: String,
    help: Option<String>,
}

impl ParserError {
    pub fn new(kind: ErrorKind, message: String, help: Option<String>) -> Self {
        Self {
            kind,
            message,
            help,
        }
    }

    pub fn with_help(self, help: String) -> Self {
        Self {
            kind: self.kind,
            message: self.message,
            help: Some(help),
        }
    }
}

impl LiteError for ParserError {
    fn labels(&self) -> Vec<Spanned<String>> {
        let label = match &self.kind {
            ErrorKind::Expected(expected, _, span) => {
                let message = if expected.len() == 1 {
                    format!("Expected to find {}", expected[0])
                } else {
                    format!("Expected to find one of {:?}", expected)
                };

                (message, *span)
            }
            ErrorKind::Unclosed(token, span) => (format!("Unclosed delimiter {token}"), *span),
            ErrorKind::Unexpected(token, span) => (format!("Unexpected token {token}"), *span),
            ErrorKind::Other(message, span) => (message.to_string(), *span),
        };

        vec![label]
    }

    fn message(&self) -> &String {
        &self.message
    }

    fn kind(&self) -> ReportKind {
        ReportKind::Error
    }

    fn help(&self) -> Option<&String> {
        self.help.as_ref()
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl Error for ParserError {}
