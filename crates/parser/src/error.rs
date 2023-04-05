use std::fmt::Display;

use span::Span;

pub struct ParserError {
    message: String,
    span: Span,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Parser Error: {} at {}", self.message, self.span)?;
        Ok(())
    }
}
