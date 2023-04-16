use std::fmt::Display;

use span::Span;

#[derive(Debug)]
pub struct ParserError {
    message: String,
    span: Span,
}

impl ParserError {
    pub const fn new(message: String, span: Span) -> Self {
        Self { message, span }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Parser Error: {} at {}", self.message, self.span)?;
        Ok(())
    }
}
