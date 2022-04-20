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
pub fn get_precedence(kind: TokenKind) -> Precedence {
    match kind {
        // Assignment operators = += -= *= /=
        TokenKind::Equal
        | TokenKind::PlusEqual
        | TokenKind::MinusEqual
        | TokenKind::StarEqual
        | TokenKind::SlashEqual => Precedence::new(None, Some(1)),
        TokenKind::Or => Precedence::new(None, Some(2)),
        TokenKind::And => Precedence::new(None, Some(3)),
        TokenKind::EqualEqual | TokenKind::BangEqual => Precedence::new(None, Some(4)),
        TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual => {
            Precedence::new(None, Some(5))
        }
        TokenKind::Plus => Precedence::new(None, Some(6)),
        TokenKind::Minus => Precedence::new(Some(8), Some(6)),
        TokenKind::Star | TokenKind::Slash => Precedence::new(None, Some(7)),
        TokenKind::Bang => Precedence::new(Some(8), None),
        TokenKind::OpenParen | TokenKind::Dot => Precedence::new(None, Some(9)),
        _ => Precedence::new(None, Some(0)),
    }
}
