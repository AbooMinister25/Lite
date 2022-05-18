//! The lexer takes some source string and generates a stream
//! of `TokenKind`s `TokenKind`s are any meaningful "word" or "character".
//! Meaningful in the sense that things akin to whitespace and comments
//! are filtered out, as they would end up being unnecessary junk for the parser.

pub mod tokens;

use std::{iter::Peekable, str::Chars};

use crate::tokens::TokenKind;
use chumsky::prelude::*;
use unicode_xid::UnicodeXID;

type Spanned<T> = (T, std::ops::Range<usize>);

// /// Generates a `Vec` consisting of a `TokenKind` paired with its corresponding span from a
// /// UTF-8 encoded string.
// ///
// /// The job of the `lexer` is to take some string input and tokenize it, filtering out
// /// unnecessary items, such as whitespace or comments.
// ///
// /// # Examples
// ///
// /// ```
// /// use lexer::lexer;
// /// use chumsky::Parser;
// ///
// /// let source = r#"
// /// func main() do
// ///     println("Hello, World")
// /// end
// /// "#;
// ///
// /// let (tokens, _) = lexer().parse_recovery(source);
// /// ```
// pub fn lexer() -> impl Parser<char, Vec<Spanned<TokenKind>>, Error = Simple<char>> {
//     // Literals
//     let string = just('"')
//         .ignore_then(filter(|c| *c != '"').repeated())
//         .then_ignore(just('"'))
//         .collect::<String>()
//         .map(TokenKind::String);

//     let char = just('\'')
//         .ignore_then(filter(|c| *c != '\'').repeated())
//         .then_ignore(just('\''))
//         .collect::<String>()
//         .validate(|value, span, emit| {
//             if value.len() > 1 {
//                 emit(Simple::custom(
//                     span,
//                     "Character literals can only contain one codepoint".to_string(),
//                 ))
//             }
//             value
//         })
//         .map(|c| TokenKind::Char(c.chars().next().unwrap())); // Safe to unwrap, since string has a single character

//     let num = text::int(10)
//         .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
//         .collect::<String>()
//         .map(|s| {
//             if s.contains('.') {
//                 TokenKind::Float(s)
//             } else {
//                 TokenKind::Integer(s)
//             }
//         });

//     // Symbols e.g punctuation and operators
//     let symbol = just('(')
//         .to(TokenKind::OpenParen)
//         .or(just(')').to(TokenKind::CloseParen))
//         .or(just(')').to(TokenKind::CloseParen))
//         .or(just('[').to(TokenKind::OpenBracket))
//         .or(just(']').to(TokenKind::CloseBracket))
//         .or(just('{').to(TokenKind::OpenBrace))
//         .or(just('}').to(TokenKind::CloseBrace))
//         .or(just(',').to(TokenKind::Comma))
//         .or(just('.').to(TokenKind::Dot))
//         .or(just(';').to(TokenKind::Semicolon))
//         .or(just(':').to(TokenKind::Colon))
//         // Operators
//         .or(just("==").to(TokenKind::EqualEqual))
//         .or(just("!=").to(TokenKind::BangEqual))
//         .or(just(">=").to(TokenKind::GreaterEqual))
//         .or(just("<=").to(TokenKind::LessEqual))
//         .or(just("+=").to(TokenKind::PlusEqual))
//         .or(just("-=").to(TokenKind::MinusEqual))
//         .or(just("*=").to(TokenKind::StarEqual))
//         .or(just("/=").to(TokenKind::SlashEqual))
//         .or(just('=').to(TokenKind::Equal))
//         .or(just('!').to(TokenKind::Bang))
//         .or(just('>').to(TokenKind::Greater))
//         .or(just('<').to(TokenKind::Less))
//         .or(just('+').to(TokenKind::Plus))
//         .or(just('-').to(TokenKind::Minus))
//         .or(just('*').to(TokenKind::Star))
//         .or(just('/').to(TokenKind::Slash));

//     // Identifiers and keywords
//     let ident = text::ident().map(|ident: String| match ident.as_str() {
//         "and" => TokenKind::And,
//         "class" => TokenKind::Class,
//         "do" => TokenKind::Do,
//         "else" => TokenKind::Else,
//         "end" => TokenKind::End,
//         "false" => TokenKind::False,
//         "for" => TokenKind::For,
//         "func" => TokenKind::Func,
//         "if" => TokenKind::If,
//         "in" => TokenKind::In,
//         "import" => TokenKind::Import,
//         "let" => TokenKind::Let,
//         "match" => TokenKind::Match,
//         "mut" => TokenKind::Mut,
//         "new" => TokenKind::New,
//         "or" => TokenKind::Or,
//         "return" => TokenKind::Return,
//         "trait" => TokenKind::Trait,
//         "true" => TokenKind::True,
//         "while" => TokenKind::While,
//         "with" => TokenKind::With,
//         _ => TokenKind::Ident(ident),
//     });

//     let token = string
//         .or(char)
//         .or(num)
//         .or(ident)
//         .or(symbol)
//         .recover_with(skip_then_retry_until([]));

//     // Comments
//     let comment = just("//").then(take_until(just('\n'))).padded();

//     token
//         .map_with_span(|tok, span| (tok, span))
//         .padded_by(comment.repeated())
//         .padded()
//         .repeated()
// }

// #[cfg(test)]
// mod tests {
//     use chumsky::error::SimpleReason;

//     use super::*;

//     #[test]
//     fn test_punctuation() {
//         let source = "( ) [ ] { } , . ; :";

//         let (tokens, errors) = lexer().parse_recovery(source);
//         assert!(errors.is_empty());
//         assert!(tokens.is_some());

//         assert_eq!(
//             tokens
//                 .unwrap()
//                 .into_iter()
//                 .map(|(t, _)| t)
//                 .collect::<Vec<TokenKind>>(),
//             vec![
//                 TokenKind::OpenParen,
//                 TokenKind::CloseParen,
//                 TokenKind::OpenBracket,
//                 TokenKind::CloseBracket,
//                 TokenKind::OpenBrace,
//                 TokenKind::CloseBrace,
//                 TokenKind::Comma,
//                 TokenKind::Dot,
//                 TokenKind::Semicolon,
//                 TokenKind::Colon,
//             ]
//         )
//     }

//     #[test]
//     fn test_operators() {
//         let source = "= == ! != > >= < <= + += - -= * *= / /=";

//         let (tokens, errors) = lexer().parse_recovery(source);
//         assert!(errors.is_empty());
//         assert!(tokens.is_some());

//         assert_eq!(
//             tokens
//                 .unwrap()
//                 .into_iter()
//                 .map(|(t, _)| t)
//                 .collect::<Vec<TokenKind>>(),
//             vec![
//                 TokenKind::Equal,
//                 TokenKind::EqualEqual,
//                 TokenKind::Bang,
//                 TokenKind::BangEqual,
//                 TokenKind::Greater,
//                 TokenKind::GreaterEqual,
//                 TokenKind::Less,
//                 TokenKind::LessEqual,
//                 TokenKind::Plus,
//                 TokenKind::PlusEqual,
//                 TokenKind::Minus,
//                 TokenKind::MinusEqual,
//                 TokenKind::Star,
//                 TokenKind::StarEqual,
//                 TokenKind::Slash,
//                 TokenKind::SlashEqual,
//             ]
//         )
//     }

//     #[test]
//     fn test_literals() {
//         let source = "10 1.5 \"Hello World\" 'h' foo true false";

//         let (tokens, errors) = lexer().parse_recovery(source);
//         assert!(errors.is_empty());
//         assert!(tokens.is_some());

//         assert_eq!(
//             tokens
//                 .unwrap()
//                 .into_iter()
//                 .map(|(t, _)| t)
//                 .collect::<Vec<TokenKind>>(),
//             vec![
//                 TokenKind::Integer("10".to_string()),
//                 TokenKind::Float("1.5".to_string()),
//                 TokenKind::String("Hello World".to_string()),
//                 TokenKind::Char('h'),
//                 TokenKind::Ident("foo".to_string()),
//                 TokenKind::True,
//                 TokenKind::False,
//             ]
//         )
//     }

//     #[test]
//     fn test_keywords() {
//         let source =
//             "and class do else end for func if in import let match mut new or return trait while with";

//         let (tokens, errors) = lexer().parse_recovery(source);
//         assert!(errors.is_empty());
//         assert!(tokens.is_some());

//         assert_eq!(
//             tokens
//                 .unwrap()
//                 .into_iter()
//                 .map(|(t, _)| t)
//                 .collect::<Vec<TokenKind>>(),
//             vec![
//                 TokenKind::And,
//                 TokenKind::Class,
//                 TokenKind::Do,
//                 TokenKind::Else,
//                 TokenKind::End,
//                 TokenKind::For,
//                 TokenKind::Func,
//                 TokenKind::If,
//                 TokenKind::In,
//                 TokenKind::Import,
//                 TokenKind::Let,
//                 TokenKind::Match,
//                 TokenKind::Mut,
//                 TokenKind::New,
//                 TokenKind::Or,
//                 TokenKind::Return,
//                 TokenKind::Trait,
//                 TokenKind::While,
//                 TokenKind::With,
//             ]
//         )
//     }

//     #[test]
//     fn test_errors() {
//         let source = "\"Hi";

//         let (_, errors) = lexer().parse_recovery(source);
//         assert!(!errors.is_empty());
//         assert_eq!(*errors[0].expected().next().unwrap(), Some('"'));

//         let source = "'h";
//         let (_, errors) = lexer().parse_recovery(source);
//         assert!(!errors.is_empty());
//         assert_eq!(*errors[0].expected().next().unwrap(), Some('\''));

//         let source = "'foo'";
//         let (_, errors) = lexer().parse_recovery(source);
//         assert!(!errors.is_empty());

//         let reason = errors[0].reason();

//         if let SimpleReason::Custom(s) = reason {
//             assert_eq!(s, "Character literals can only contain one codepoint");
//         }
//     }
// }

fn get_keyword(value: &str, keyword: &str, length: usize, token: TokenKind) -> TokenKind {
    if value[length..] == keyword[length..] {
        token
    } else {
        TokenKind::Ident(value.to_string())
    }
}

fn ident_type(value: &str) -> TokenKind {
    match &value[..1] {
        "a" => get_keyword(value, "and", 1, TokenKind::And),
        "c" => get_keyword(value, "class", 1, TokenKind::Class),
        "d" => get_keyword(value, "do", 1, TokenKind::Do),
        "e" => {
            if value.len() < 2 {
                return TokenKind::Ident(value.to_string());
            }

            match &value[1..2] {
                "l" => get_keyword(value, "else", 2, TokenKind::Else),
                "n" => get_keyword(value, "end", 2, TokenKind::End),
                _ => TokenKind::Ident(value.to_string()),
            }
        }
        "f" => {
            if value.len() < 2 {
                return TokenKind::Ident(value.to_string());
            }

            match &value[1..2] {
                "a" => get_keyword(value, "false", 2, TokenKind::False),
                "o" => get_keyword(value, "for", 2, TokenKind::For),
                "u" => get_keyword(value, "func", 2, TokenKind::Func),
                _ => TokenKind::Ident(value.to_string()),
            }
        }
        "i" => {
            if value.len() < 2 {
                return TokenKind::Ident(value.to_string());
            }

            match &value[1..2] {
                "f" => TokenKind::If,
                "m" => get_keyword(value, "import", 2, TokenKind::Import),
                "n" => TokenKind::In,
                _ => TokenKind::Ident(value.to_string()),
            }
        }
        "l" => get_keyword(value, "let", 1, TokenKind::Let),
        "m" => {
            if value.len() < 2 {
                return TokenKind::Ident(value.to_string());
            }

            match &value[1..2] {
                "a" => get_keyword(value, "match", 2, TokenKind::Match),
                "u" => get_keyword(value, "mut", 2, TokenKind::Mut),
                _ => TokenKind::Ident(value.to_string()),
            }
        }
        "n" => get_keyword(value, "new", 1, TokenKind::New),
        "o" => get_keyword(value, "or", 1, TokenKind::Or),
        "r" => get_keyword(value, "return", 1, TokenKind::Return),
        "t" => {
            if value.len() < 3 {
                return TokenKind::Ident(value.to_string());
            }

            if &value[1..2] != "r" {
                return TokenKind::Ident(value.to_string());
            }

            match &value[2..3] {
                "u" => get_keyword(value, "true", 3, TokenKind::True),
                "a" => get_keyword(value, "trait", 3, TokenKind::Trait),
                _ => TokenKind::Ident(value.to_string()),
            }
        }
        "w" => {
            if value.len() < 2 {
                return TokenKind::Ident(value.to_string());
            }

            match &value[1..2] {
                "h" => get_keyword(value, "While", 2, TokenKind::While),
                "i" => get_keyword(value, "With", 2, TokenKind::With),
                _ => TokenKind::Ident(value.to_string()),
            }
        }
        _ => TokenKind::Ident(value.to_string()),
    }
}

/// Generates a stream of `Token`s from some UTF-8 encoded
/// string.
///
/// The job of the `Lexer` is to generate tokens on-demand
/// from the given source string, filtering out unnecessary
/// items, such as whitespace or comments.
///
/// # Examples
///
/// ```
/// use lexer::Lexer;
///
/// let source = r#"
/// func main do
///     println("Hello, World")
/// end
/// "#;
///
/// let mut lexer = Lexer::new(source);
/// let (current, next) = lexer.next_token();
/// ```
pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    pub position: usize,
    line: u32,
    column: u32,
}

impl<'a> Lexer<'a> {
    /// Constructs a new `Lexer` from the given source string.
    ///
    /// # Examples
    ///
    /// ```
    /// use lexer::Lexer;
    ///
    /// let source = r#"
    /// func main do
    ///     println("Hello, World")
    /// end
    /// "#;
    ///
    /// let mut lexer = Lexer::new(source);
    /// ```
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.chars().peekable(),
            position: 0,
            line: 1,
            column: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.position += 1;
        self.source.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.source.peek()
    }

    fn consume(&mut self, expected: char) -> bool {
        if let Some(c) = self.peek() {
            if c == &expected {
                self.advance();
                return true;
            }
        }

        false
    }

    pub fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn create_token(&mut self, kind: TokenKind, len: usize) -> Spanned<TokenKind> {
        (kind, self.position - len..self.position)
    }

    fn lex_string(&mut self) -> Spanned<TokenKind> {
        let mut value = String::new();

        // Safe to unwrap since && will short circuit
        while !self.at_end() && self.peek().unwrap() != &'"' {
            value.push(self.advance().unwrap()); // Safe to unwrap since not the end of input
        }

        let len = value.len();

        // If at the end of the source string and closing quote not found, return an error token
        if self.at_end() {
            return self.create_token(
                TokenKind::Error("Unterminated string literal. Expected closing quote, instead found EoF (End of File)".to_string()), 
                len
            );
        }

        self.advance(); // Consume closing quote
        self.create_token(TokenKind::String(value), len)
    }

    fn lex_char(&mut self) -> Spanned<TokenKind> {
        if self.at_end() {
            return self.create_token(
                TokenKind::Error("Unterminated character literal. Expected closing quote `'`, instead found EoF (End of File)".to_string()), 
                1
            );
        }

        let value = self.advance().unwrap(); // Safe to unwrap since not at end of source

        if !self.consume('\'') {
            let mut closed = false;

            while !self.at_end() {
                // Safe to unwrap since not at end
                if self.peek().unwrap() == &'\'' {
                    closed = true;
                    self.advance();
                    break;
                }

                self.advance();
            }

            return self.create_token(
                if closed {
                    TokenKind::Error(
                        "Character literals can only contain one codepoint".to_string(),
                    )
                } else {
                    TokenKind::Error("Unterminated character literal. Expected closing quote `'`, instead found EoF (End of File)".to_string())
                },
                1,
            );
        }

        self.create_token(TokenKind::Char(value), 1)
    }

    fn lex_number(&mut self, first_char: char) -> Spanned<TokenKind> {
        let mut is_integer = true; // True if the number is an integer, set to false if number is found to be a float
        let mut value = String::from(first_char);

        // Safe to unwrap since && will short circuit
        while !self.at_end() && self.peek().unwrap().is_numeric() {
            value.push(self.advance().unwrap()); // Safe to unwrap since not the end of input
        }

        if let Some(c) = self.peek() {
            if c == &'.' {
                is_integer = false; // Set is_integer to false since dot indicates value is a float
                value.push(self.advance().unwrap()); // Safe to unwrap since not end of input

                // Safe to unwrap since && will short circuit
                while !self.at_end() && self.peek().unwrap().is_numeric() {
                    value.push(self.advance().unwrap()); // Safe to unwrap since not the end of input
                }
            }
        }

        let len = value.len();

        self.create_token(
            if is_integer {
                TokenKind::Integer(value)
            } else {
                TokenKind::Float(value)
            },
            len,
        )
    }

    fn lex_identifier(&mut self, first_char: char) -> Spanned<TokenKind> {
        let mut value = String::from(first_char);

        if self.at_end() {
            return self.create_token(TokenKind::Ident(value), 1);
        }

        // Add to the value as long as next character is valid identifier
        while !self.at_end() && UnicodeXID::is_xid_continue(*self.peek().unwrap()) {
            value.push(self.advance().unwrap());
        }

        let token_type = ident_type(&value); // Check whether token is a Lite keyword
        self.create_token(token_type, value.len())
    }

    fn lex_token(&mut self) -> Spanned<TokenKind> {
        if let Some(c) = self.advance() {
            return match c {
                // Punctuation
                '(' => self.create_token(TokenKind::OpenParen, 1),
                ')' => self.create_token(TokenKind::CloseParen, 1),
                '[' => self.create_token(TokenKind::OpenBracket, 1),
                ']' => self.create_token(TokenKind::CloseBracket, 1),
                '{' => self.create_token(TokenKind::OpenBrace, 1),
                '}' => self.create_token(TokenKind::CloseBrace, 1),
                ',' => self.create_token(TokenKind::Comma, 1),
                '.' => self.create_token(TokenKind::Dot, 1),
                ';' => self.create_token(TokenKind::Semicolon, 1),
                ':' => self.create_token(TokenKind::Colon, 1),

                // Operators
                '=' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::EqualEqual, 2)
                    } else {
                        self.create_token(TokenKind::Equal, 1)
                    }
                }
                '!' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::BangEqual, 2)
                    } else {
                        self.create_token(TokenKind::Bang, 1)
                    }
                }
                '>' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::GreaterEqual, 2)
                    } else {
                        self.create_token(TokenKind::Greater, 1)
                    }
                }
                '<' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::LessEqual, 2)
                    } else {
                        self.create_token(TokenKind::Less, 1)
                    }
                }
                '+' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::PlusEqual, 2)
                    } else {
                        self.create_token(TokenKind::Plus, 1)
                    }
                }
                '-' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::MinusEqual, 2)
                    } else {
                        self.create_token(TokenKind::Minus, 1)
                    }
                }
                '*' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::StarEqual, 2)
                    } else {
                        self.create_token(TokenKind::Star, 1)
                    }
                }
                '/' => {
                    if self.consume('=') {
                        self.create_token(TokenKind::SlashEqual, 2)
                    } else if self.consume('/') {
                        // Safe to unwrap since lexer is never at the end of input inside the loop
                        while !self.at_end() {
                            if self.peek().unwrap() == &'\n' {
                                break;
                            }
                            self.advance();
                        }
                        self.lex_token()
                    } else {
                        self.create_token(TokenKind::Slash, 1)
                    }
                }

                // Literals
                '"' => self.lex_string(),
                '\'' => self.lex_char(),
                c if c.is_numeric() => self.lex_number(c),
                c if UnicodeXID::is_xid_start(c) || c == '_' => self.lex_identifier(c),

                // Whitespace
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                    self.lex_token()
                }
                c if c.is_whitespace() => self.lex_token(),

                c => self.create_token(TokenKind::Error(format!("Unkown character {}", c)), 1),
            };
        }

        self.create_token(TokenKind::EoF, 0)
    }

    /// Wrapper around `lex_token` which lexes and returns the
    /// current`Token` in the token stream
    ///
    /// # Examples
    /// ```
    /// use lexer::Lexer;
    ///
    /// let source = r#"
    /// func main do
    ///     println("Hello, World")
    /// end
    /// "#;
    ///
    /// let mut lexer = Lexer::new(source);
    /// let tok = lexer.next_token();
    /// ```
    pub fn next_token(&mut self) -> Spanned<TokenKind> {
        self.lex_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_punctuation() {
        let source = "( ) [ ] { } , . ; :";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();

        loop {
            let tok = lexer.next_token();

            if tok.0 == TokenKind::EoF {
                tokens.push(tok.0);
                break;
            }

            tokens.push(tok.0);
        }

        assert_eq!(
            tokens,
            vec![
                TokenKind::OpenParen,
                TokenKind::CloseParen,
                TokenKind::OpenBracket,
                TokenKind::CloseBracket,
                TokenKind::OpenBrace,
                TokenKind::CloseBrace,
                TokenKind::Comma,
                TokenKind::Dot,
                TokenKind::Semicolon,
                TokenKind::Colon,
                TokenKind::EoF,
            ]
        )
    }

    #[test]
    fn test_operators() {
        let source = "= == ! != > >= < <= + += - -= * *= / /=";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();

        loop {
            let tok = lexer.next_token();

            if tok.0 == TokenKind::EoF {
                tokens.push(tok.0);
                break;
            }

            tokens.push(tok.0);
        }

        assert_eq!(
            tokens,
            vec![
                TokenKind::Equal,
                TokenKind::EqualEqual,
                TokenKind::Bang,
                TokenKind::BangEqual,
                TokenKind::Greater,
                TokenKind::GreaterEqual,
                TokenKind::Less,
                TokenKind::LessEqual,
                TokenKind::Plus,
                TokenKind::PlusEqual,
                TokenKind::Minus,
                TokenKind::MinusEqual,
                TokenKind::Star,
                TokenKind::StarEqual,
                TokenKind::Slash,
                TokenKind::SlashEqual,
                TokenKind::EoF,
            ]
        )
    }

    #[test]
    fn test_literals() {
        let source = "10 1.5 \"Hello World\" 'h' foo true false";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();

        loop {
            let tok = lexer.next_token();

            if tok.0 == TokenKind::EoF {
                tokens.push(tok.0);
                break;
            }

            tokens.push(tok.0);
        }

        assert_eq!(
            tokens,
            vec![
                TokenKind::Integer("10".to_string()),
                TokenKind::Float("1.5".to_string()),
                TokenKind::String("Hello World".to_string()),
                TokenKind::Char('h'),
                TokenKind::Ident("foo".to_string()),
                TokenKind::True,
                TokenKind::False,
                TokenKind::EoF,
            ]
        )
    }

    #[test]
    fn test_keywords() {
        let source =
            "and class do else end for func if in import let match mut new or return trait while with";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();

        loop {
            let tok = lexer.next_token();

            if tok.0 == TokenKind::EoF {
                tokens.push(tok.0);
                break;
            }

            tokens.push(tok.0);
        }

        assert_eq!(
            tokens,
            vec![
                TokenKind::And,
                TokenKind::Class,
                TokenKind::Do,
                TokenKind::Else,
                TokenKind::End,
                TokenKind::For,
                TokenKind::Func,
                TokenKind::If,
                TokenKind::In,
                TokenKind::Import,
                TokenKind::Let,
                TokenKind::Match,
                TokenKind::Mut,
                TokenKind::New,
                TokenKind::Or,
                TokenKind::Return,
                TokenKind::Trait,
                TokenKind::While,
                TokenKind::With,
                TokenKind::EoF,
            ]
        )
    }

    #[test]
    fn test_errors() {
        let source = "\"Hi";
        let mut lexer = Lexer::new(source);
        let tok = lexer.next_token();

        assert_eq!(
            tok.0,
            TokenKind::Error("Unterminated string literal. Expected closing quote, instead found EoF (End of File)".to_string())
        );

        let source = "'H";
        let mut lexer = Lexer::new(source);
        let tok = lexer.next_token();

        assert_eq!(
            tok.0,
            TokenKind::Error("Unterminated character literal. Expected closing quote `'`, instead found EoF (End of File)".to_string())
        );

        let source = "'Hello'";
        let mut lexer = Lexer::new(source);
        let tok = lexer.next_token();

        assert_eq!(
            tok.0,
            TokenKind::Error("Character literals can only contain one codepoint".to_string())
        );
    }
}
