//! The lexer takes some source string and generates a stream
//! of `TokenKind`s `TokenKind`s are any meaningful "word" or "character".
//! Meaningful in the sense that things akin to whitespace and comments
//! are filtered out, as they would end up being unnecessary junk for the parser.

pub mod tokens;

use crate::tokens::TokenKind;
use chumsky::prelude::*;

type Spanned<T> = (T, std::ops::Range<usize>);

/// Generates a `Vec` consisting of a `TokenKind` paired with its corresponding span from a
/// UTF-8 encoded string.
///
/// The job of the `lexer` is to take some string input and tokenize it, filtering out
/// unnecessary items, such as whitespace or comments.
///
/// # Examples
///
/// ```
/// use lexer::lexer;
/// use chumsky::Parser;
///
/// let source = r#"
/// func main() do
///     println("Hello, World")
/// end
/// "#;
///
/// let (tokens, _) = lexer().parse_recovery(source);
/// ```
pub fn lexer() -> impl Parser<char, Vec<Spanned<TokenKind>>, Error = Simple<char>> {
    // Literals
    let string = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(TokenKind::String);

    let char = just('\'')
        .ignore_then(filter(|c| *c != '\'').repeated())
        .then_ignore(just('\''))
        .collect::<String>()
        .validate(|value, span, emit| {
            if value.len() > 1 {
                emit(Simple::custom(
                    span,
                    "Character literals can only contain one codepoint".to_string(),
                ))
            }
            value
        })
        .map(|c| TokenKind::Char(c.chars().next().unwrap())); // Safe to unwrap, since string has a single character

    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(|s| {
            if s.contains('.') {
                TokenKind::Float(s)
            } else {
                TokenKind::Integer(s)
            }
        });

    // Symbols e.g punctuation and operators
    let symbol = just('(')
        .to(TokenKind::OpenParen)
        .or(just(')').to(TokenKind::CloseParen))
        .or(just(')').to(TokenKind::CloseParen))
        .or(just('[').to(TokenKind::OpenBracket))
        .or(just(']').to(TokenKind::CloseBracket))
        .or(just('{').to(TokenKind::OpenBrace))
        .or(just('}').to(TokenKind::CloseBrace))
        .or(just(',').to(TokenKind::Comma))
        .or(just('.').to(TokenKind::Dot))
        .or(just(';').to(TokenKind::Semicolon))
        .or(just(':').to(TokenKind::Colon))
        // Operators
        .or(just("==").to(TokenKind::EqualEqual))
        .or(just("!=").to(TokenKind::BangEqual))
        .or(just(">=").to(TokenKind::GreaterEqual))
        .or(just("<=").to(TokenKind::LessEqual))
        .or(just("+=").to(TokenKind::PlusEqual))
        .or(just("-=").to(TokenKind::MinusEqual))
        .or(just("*=").to(TokenKind::StarEqual))
        .or(just("/=").to(TokenKind::SlashEqual))
        .or(just('=').to(TokenKind::Equal))
        .or(just('!').to(TokenKind::Bang))
        .or(just('>').to(TokenKind::Greater))
        .or(just('<').to(TokenKind::Less))
        .or(just('+').to(TokenKind::Plus))
        .or(just('-').to(TokenKind::Minus))
        .or(just('*').to(TokenKind::Star))
        .or(just('/').to(TokenKind::Slash));

    // Identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "and" => TokenKind::And,
        "class" => TokenKind::Class,
        "do" => TokenKind::Do,
        "else" => TokenKind::Else,
        "end" => TokenKind::End,
        "false" => TokenKind::False,
        "for" => TokenKind::For,
        "func" => TokenKind::Func,
        "if" => TokenKind::If,
        "import" => TokenKind::Import,
        "let" => TokenKind::Let,
        "match" => TokenKind::Match,
        "mut" => TokenKind::Mut,
        "new" => TokenKind::New,
        "or" => TokenKind::Or,
        "return" => TokenKind::Return,
        "trait" => TokenKind::Trait,
        "true" => TokenKind::True,
        "while" => TokenKind::While,
        "with" => TokenKind::With,
        _ => TokenKind::Ident(ident),
    });

    let token = string
        .or(char)
        .or(num)
        .or(ident)
        .or(symbol)
        .recover_with(skip_then_retry_until([]));

    // Comments
    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}

#[cfg(test)]
mod tests {
    use chumsky::error::SimpleReason;

    use super::*;

    #[test]
    fn test_punctuation() {
        let source = "( ) [ ] { } , . ; :";

        let (tokens, errors) = lexer().parse_recovery(source);
        assert!(errors.is_empty());
        assert!(tokens.is_some());

        assert_eq!(
            tokens
                .unwrap()
                .into_iter()
                .map(|(t, _)| t)
                .collect::<Vec<TokenKind>>(),
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
            ]
        )
    }

    #[test]
    fn test_operators() {
        let source = "= == ! != > >= < <= + += - -= * *= / /=";

        let (tokens, errors) = lexer().parse_recovery(source);
        assert!(errors.is_empty());
        assert!(tokens.is_some());

        assert_eq!(
            tokens
                .unwrap()
                .into_iter()
                .map(|(t, _)| t)
                .collect::<Vec<TokenKind>>(),
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
            ]
        )
    }

    #[test]
    fn test_literals() {
        let source = "10 1.5 \"Hello World\" 'h' foo true false";

        let (tokens, errors) = lexer().parse_recovery(source);
        assert!(errors.is_empty());
        assert!(tokens.is_some());

        assert_eq!(
            tokens
                .unwrap()
                .into_iter()
                .map(|(t, _)| t)
                .collect::<Vec<TokenKind>>(),
            vec![
                TokenKind::Integer("10".to_string()),
                TokenKind::Float("1.5".to_string()),
                TokenKind::String("Hello World".to_string()),
                TokenKind::Char('h'),
                TokenKind::Ident("foo".to_string()),
                TokenKind::True,
                TokenKind::False,
            ]
        )
    }

    #[test]
    fn test_keywords() {
        let source =
            "and class do else end for func if import let match mut new or return trait while with";

        let (tokens, errors) = lexer().parse_recovery(source);
        assert!(errors.is_empty());
        assert!(tokens.is_some());

        assert_eq!(
            tokens
                .unwrap()
                .into_iter()
                .map(|(t, _)| t)
                .collect::<Vec<TokenKind>>(),
            vec![
                TokenKind::And,
                TokenKind::Class,
                TokenKind::Do,
                TokenKind::Else,
                TokenKind::End,
                TokenKind::For,
                TokenKind::Func,
                TokenKind::If,
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
            ]
        )
    }

    #[test]
    fn test_errors() {
        let source = "\"Hi";

        let (_, errors) = lexer().parse_recovery(source);
        assert!(!errors.is_empty());
        assert_eq!(*errors[0].expected().next().unwrap(), Some('"'));

        let source = "'h";
        let (_, errors) = lexer().parse_recovery(source);
        assert!(!errors.is_empty());
        assert_eq!(*errors[0].expected().next().unwrap(), Some('\''));

        let source = "'foo'";
        let (_, errors) = lexer().parse_recovery(source);
        assert!(!errors.is_empty());

        let reason = errors[0].reason();

        if let SimpleReason::Custom(s) = reason {
            assert_eq!(s, "Character literals can only contain one codepoint");
        }
    }
}
