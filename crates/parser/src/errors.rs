use ariadne::ReportKind;
use error::LiteError;
use lexer::tokens::TokenKind;
use std::ops::Range;

#[derive(Debug)]
pub enum ParserError {
    /// Expected one of the given tokens, found something else
    Expected(Vec<TokenKind>, TokenKind, Range<usize>),
    /// An unclosed delimeter
    Unclosed(TokenKind, Range<usize>),
    /// Found an unexpected token
    Unexpected(TokenKind, Range<usize>),
    /// Another type of error occured with the given message
    Other(String, Range<usize>),
}

impl LiteError for ParserError {
    fn labels(&self) -> Vec<(String, std::ops::Range<usize>)> {
        let label = match self {
            ParserError::Expected(expected, _, span) => {
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

                (message, span.start..span.end)
            }
            ParserError::Unclosed(token, span) => {
                (format!("Unclosed delimiter {token}"), span.start..span.end)
            }
            ParserError::Unexpected(token, span) => {
                (format!("Unexpected token {token}"), span.start..span.end)
            }
            ParserError::Other(message, span) => (message.to_string(), span.start..span.end),
        };

        vec![label]
    }

    fn message(&self) -> String {
        match self {
            ParserError::Expected(expected, found, _) => {
                if expected.len() == 1 {
                    format!(
                        "Expected to find token {}, instead found {found}",
                        expected[0]
                    )
                } else {
                    format!(
                        "Expected to find one of {:?}, instead found {found}",
                        expected
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
            }
            ParserError::Unclosed(token, _) => format!("Unclosed delimiter {token}"),
            ParserError::Unexpected(token, _) => format!("Unexpected token {token}"),
            ParserError::Other(message, _) => message.to_string(),
        }
    }

    fn kind(&self) -> ReportKind {
        ReportKind::Error
    }
}
