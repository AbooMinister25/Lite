use ariadne::ReportKind;
use error::LiteError;
use span::{Span, Spanned};
use std::{error::Error, fmt};

/// Represents the different types of errors
/// which may be encountered during name resolution.
#[derive(Debug)]
pub enum ErrorKind {
    ReadLocalInInit(String, Span),
}

/// This struct holds information related to an
/// error which may have occurred during name resolution.
#[derive(Debug)]
pub struct ResolutionError {
    kind: ErrorKind,
    message: String,
    help: Option<String>,
}

impl ResolutionError {
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

impl LiteError for ResolutionError {
    fn labels(&self) -> Vec<Spanned<String>> {
        let label = match &self.kind {
            ErrorKind::ReadLocalInInit(name, span) => (
                format!("Can't read local variable {name} in its own initializer."),
                *span,
            ),
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

impl fmt::Display for ResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl Error for ResolutionError {}
