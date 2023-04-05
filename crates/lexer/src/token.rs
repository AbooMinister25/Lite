use std::fmt;

/// The `TokenKind` enum represents the most basic meaningful
/// piece of information in some Lite source code, a token.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    Colon,
    Arrow,

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
    Do,
    Else,
    End,
    False,
    For,
    Fun,
    If,
    In,
    Import,
    Let,
    Match,
    Mut,
    Pub,
    Return,
    Trait,
    True,
    Type,
    While,

    // Misc
    Error(String),
    EoF,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::OpenParen => write!(f, "("),
            Self::CloseParen => write!(f, ")"),
            Self::OpenBracket => write!(f, "["),
            Self::CloseBracket => write!(f, "]"),
            Self::OpenBrace => write!(f, "{{"),
            Self::CloseBrace => write!(f, "}}"),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),
            Self::Colon => write!(f, ":"),
            Self::Arrow => write!(f, "=>"),
            Self::Equal => write!(f, "="),
            Self::EqualEqual => write!(f, "=="),
            Self::Bang => write!(f, "!"),
            Self::BangEqual => write!(f, "!="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Plus => write!(f, "+"),
            Self::PlusEqual => write!(f, "+="),
            Self::Minus => write!(f, "-"),
            Self::MinusEqual => write!(f, "-="),
            Self::Star => write!(f, "*"),
            Self::StarEqual => write!(f, "*="),
            Self::Slash => write!(f, "/"),
            Self::SlashEqual => write!(f, "/="),
            Self::String(s) => write!(f, "string({s})"),
            Self::Char(c) => write!(f, "char({c})"),
            Self::Integer(i) => write!(f, "integer({i})"),
            Self::Float(fl) => write!(f, "float({fl})"),
            Self::Ident(i) => write!(f, "ident({i})"),
            Self::Do => write!(f, "do"),
            Self::Else => write!(f, "else"),
            Self::End => write!(f, "end"),
            Self::False => write!(f, "false"),
            Self::For => write!(f, "for"),
            Self::Fun => write!(f, "fun"),
            Self::If => write!(f, "if"),
            Self::In => write!(f, "in"),
            Self::Import => write!(f, "import"),
            Self::Let => write!(f, "let"),
            Self::Match => write!(f, "match"),
            Self::Mut => write!(f, "mut"),
            Self::Pub => write!(f, "pub"),
            Self::Return => write!(f, "return"),
            Self::Trait => write!(f, "trait"),
            Self::True => write!(f, "true"),
            Self::Type => write!(f, "type"),
            Self::While => write!(f, "while"),
            Self::Error(s) => write!(f, "{s}"),
            Self::EoF => write!(f, "End of File"),
        }
    }
}