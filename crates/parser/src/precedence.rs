//! The precedence rules for Lite's parser

use lexer::tokens::TokenKind;

/// Represents possible prefix and infix precedence values
pub struct Precedence {
    pub prefix: Option<u8>,
    pub infix: Option<u8>,
}

impl Precedence {
    pub fn new(prefix: Option<u8>, infix: Option<u8>) -> Self {
        Self { prefix, infix }
    }
}

/// Returns the precedence value for a given token
pub fn get_precedence(kind: &TokenKind) -> u8 {
    match kind {
        // Assignment operators = += -= *= /=
        TokenKind::Equal
        | TokenKind::PlusEqual
        | TokenKind::MinusEqual
        | TokenKind::StarEqual
        | TokenKind::SlashEqual => 1,
        TokenKind::Or => 2,
        TokenKind::And => 3,
        TokenKind::EqualEqual | TokenKind::BangEqual => 4,
        TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual => 5,
        TokenKind::Plus | TokenKind::Minus => 6,
        TokenKind::Star | TokenKind::Slash => 7,
        TokenKind::OpenParen | TokenKind::Dot => 9,
        _ => 0,
    }
}
