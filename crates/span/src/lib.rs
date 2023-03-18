//! Contains the `Span` struct and `Spanned` type for representing the
//! position of different items in Lite throughout the source code.

#![warn(clippy::pedantic, clippy::nursery)]

use std::fmt;
use std::ops::Range;

use serde::Serialize;

pub type Spanned<T> = (T, Span);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<Range<usize>> for Span {
    fn from(r: Range<usize>) -> Self {
        Self {
            start: r.start,
            end: r.end,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}
