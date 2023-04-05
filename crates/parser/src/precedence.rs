//! The precedence rules for Lite's parser

use lexer::token::TokenKind;

/// Returns the precedence value for a given token
pub const fn get_precedence(kind: &TokenKind) -> u8 {
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
