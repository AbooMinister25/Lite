use crate::Position;
use std::fmt;

/// The `TokenKind` enum consists of the Tokens that are part of the
/// Lite language.
#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // Punctuation
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Comma,
    Dot,
    Semicolon,
    Colon,

    // Operators
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Star,
    StarEqual,
    Slash,
    SlashEqual,

    // Literals
    String(String),
    Char(char),
    Integer(String),
    Float(String),

    // Identifiers
    Ident(String),

    // Keywords
    And,
    Class,
    Do,
    Else,
    End,
    False,
    For,
    Func,
    If,
    Import,
    Let,
    Match,
    Mut,
    New,
    Or,
    Return,
    Trait,
    True,
    While,
    With,

    // Misc
    Error(String),
    EoF,
}

/// The `Token` struct represents a single Token. The
/// kind of token is represented by a `TokenKind`, and location
/// information is stored in the `Position` struct.
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::OpenParen => "(",
                TokenKind::CloseParen => ")",
                TokenKind::OpenBracket => "[",
                TokenKind::CloseBracket => "]",
                TokenKind::OpenBrace => "{",
                TokenKind::CloseBrace => "}",
                TokenKind::Comma => ",",
                TokenKind::Dot => ".",
                TokenKind::Semicolon => ";",
                TokenKind::Colon => ":",
                TokenKind::Equal => "=",
                TokenKind::EqualEqual => "==",
                TokenKind::Bang => "!",
                TokenKind::BangEqual => "!=",
                TokenKind::Greater => ">",
                TokenKind::GreaterEqual => ">=",
                TokenKind::Less => "<",
                TokenKind::LessEqual => "<=",
                TokenKind::Plus => "+",
                TokenKind::PlusEqual => "+=",
                TokenKind::Minus => "-",
                TokenKind::MinusEqual => "-=",
                TokenKind::Star => "*",
                TokenKind::StarEqual => "*=",
                TokenKind::Slash => "/",
                TokenKind::SlashEqual => "/=",
                TokenKind::String(_) => "{string}",
                TokenKind::Char(_) => "{char}",
                TokenKind::Integer(_) => "{integer}",
                TokenKind::Float(_) => "{float}",
                TokenKind::Ident(_) => "{identifier}",
                TokenKind::And => "and",
                TokenKind::Class => "class",
                TokenKind::Do => "do",
                TokenKind::Else => "else",
                TokenKind::End => "end",
                TokenKind::False => "false",
                TokenKind::For => "for",
                TokenKind::Func => "func",
                TokenKind::If => "if",
                TokenKind::Import => "import",
                TokenKind::Let => "let",
                TokenKind::Match => "match",
                TokenKind::Mut => "mut",
                TokenKind::New => "new",
                TokenKind::Or => "or",
                TokenKind::Return => "return",
                TokenKind::Trait => "trait",
                TokenKind::True => "true",
                TokenKind::While => "while",
                TokenKind::With => "with",
                TokenKind::Error(s) => s,
                TokenKind::EoF => "End of File",
            },
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}: {}]", self.kind, self.position)
    }
}
