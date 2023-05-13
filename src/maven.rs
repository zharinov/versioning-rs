#![allow(dead_code)]

// use nom::{
//     character::complete::{satisfy, u32},
//     multi::many1,
//     AsChar, IResult,
// };
use winnow::ascii::digit1;
use winnow::combinator::{alt, repeat};
use winnow::token::take_while;
use winnow::{IResult, Parser};

#[derive(Debug, PartialEq)]
enum RawToken {
    Num(u32),
    Qual(String),
    Dot,
    Hyphen,
}

enum RawTokenType {
    NumT,
    QualT,
    DotT,
    HyphenT,
}

#[derive(Debug, PartialEq)]
enum Prefix {
    Dot,
    Hyphen,
    ImplicitHyphen,
}

#[derive(Debug, PartialEq)]
enum Token {
    Qualifier { prefix: Prefix, value: String },
    Number { prefix: Prefix, value: u32 },
}

fn number(input: &str) -> IResult<&str, RawToken> {
    digit1
        .try_map(|s| -> Result<RawToken, std::num::ParseIntError> {
            let num = str::parse::<u32>(s)?;
            Ok(RawToken::Num(num))
        })
        .parse_next(input)
}

fn qualifier(input: &str) -> IResult<&str, RawToken> {
    take_while(
        1..,
        |c: char| matches!(c, 'a'..='z' | 'A'..='Z' | '+' | '_'),
    )
    .map(|s: &str| RawToken::Qual(s.to_string()))
    .parse_next(input)
}

fn dot_separator(input: &str) -> IResult<&str, RawToken> {
    '.'.map(|_| RawToken::Dot).parse_next(input)
}

fn hyphen_separator(input: &str) -> IResult<&str, RawToken> {
    '-'.map(|_| RawToken::Hyphen).parse_next(input)
}

fn raw_token(input: &str) -> IResult<&str, RawToken> {
    alt((number, dot_separator, hyphen_separator, qualifier)).parse_next(input)
}

fn raw_tokens(input: &str) -> IResult<&str, Vec<RawToken>> {
    repeat(1.., raw_token).parse_next(input)
}

fn calculate_token(
    raw_token: Option<RawToken>,
    prev_token_type: Option<RawTokenType>,
) -> (Option<Token>, Option<RawTokenType>) {
    use RawToken::{Dot, Hyphen, Num, Qual};
    use RawTokenType::{DotT, HyphenT, NumT, QualT};
    use Token::{Number, Qualifier};

    let raw_token_type = match raw_token {
        Some(Num(_)) => Some(NumT),
        Some(Qual(_)) => Some(QualT),
        Some(Dot) => Some(DotT),
        Some(Hyphen) => Some(HyphenT),
        None => None,
    };

    #[rustfmt::skip]
    let token = match (prev_token_type, raw_token) {
        // Leading hyphen is the specifics of this implementation
        (None, Some(Num(value)))  => Some(Number    { prefix: Prefix::Hyphen, value }),
        (None, Some(Qual(value))) => Some(Qualifier { prefix: Prefix::Hyphen, value }),
        
        // Empty tokens are replaced with "0"
        (None,           Some(Hyphen)) => Some(Number { prefix: Prefix::Hyphen, value: 0 }),
        (None,           Some(Dot))    => Some(Number { prefix: Prefix::Dot, value: 0 }),
        (Some(HyphenT), Some(Hyphen))  => Some(Number { prefix: Prefix::Hyphen, value: 0 }),
        (Some(HyphenT), Some(Dot))     => Some(Number { prefix: Prefix::Hyphen, value: 0 }),
        (Some(DotT),    Some(Hyphen))  => Some(Number { prefix: Prefix::Dot, value: 0 }),
        (Some(DotT),    Some(Dot))     => Some(Number { prefix: Prefix::Dot, value: 0 }),
        (Some(HyphenT), None)          => Some(Number { prefix: Prefix::Hyphen, value: 0 }),
        (Some(DotT),    None)          => Some(Number { prefix: Prefix::Dot, value: 0 }),

        // Normal transitions explicitly prefixed by '.' or '-'
        (Some(HyphenT), Some(Num(value)))  => Some(Number    { prefix: Prefix::Hyphen, value }),
        (Some(HyphenT), Some(Qual(value))) => Some(Qualifier { prefix: Prefix::Hyphen, value }),
        (Some(DotT),    Some(Num(value)))  => Some(Number    { prefix: Prefix::Dot, value }),
        (Some(DotT),    Some(Qual(value))) => Some(Qualifier { prefix: Prefix::Dot, value }),
        
        // A transition between digits and characters is equivalent to a hyphen
        (Some(QualT), Some(Num(value)))  => Some(Number    { prefix: Prefix::ImplicitHyphen, value }),
        (Some(NumT),  Some(Qual(value))) => Some(Qualifier { prefix: Prefix::ImplicitHyphen, value }),

        // Transitions that don't produce new tokens
        (Some(NumT),  Some(Num(_)))  => None,
        (Some(NumT),  Some(Hyphen))  => None,
        (Some(NumT),  Some(Dot))     => None,
        (Some(QualT), Some(Qual(_))) => None,
        (Some(QualT), Some(Hyphen))  => None,
        (Some(QualT), Some(Dot))     => None,
        (Some(NumT),  None)          => None,
        (Some(QualT), None)          => None,
        (None,        None)          => None
    };

    (token, raw_token_type)
}

fn tokens(input: &str) -> IResult<&str, Vec<Token>> {
    raw_tokens
        .map(|raw_tokens: Vec<RawToken>| {
            let mut tokens: Vec<Token> = Vec::with_capacity(raw_tokens.len());
            let mut prev_raw_token_type: Option<RawTokenType> = None;
            for raw_token in raw_tokens {
                let (token, raw_token_type) = calculate_token(Some(raw_token), prev_raw_token_type);
                if let Some(token) = token {
                    tokens.push(token);
                }
                prev_raw_token_type = raw_token_type;
            }

            let (token, _) = calculate_token(None, prev_raw_token_type);
            if let Some(token) = token {
                tokens.push(token);
            }

            tokens
        })
        .parse_next(input)
}

// Maven tests module
#[cfg(test)]
mod maven_test {
    use super::*;

    #[test]
    fn test_number() {
        assert_eq!(number("123"), Ok(("", RawToken::Num(123))));
        assert_eq!(number("0"), Ok(("", RawToken::Num(0))));
        assert_eq!(number("1"), Ok(("", RawToken::Num(1))));
        assert_eq!(number("01"), Ok(("", RawToken::Num(1))));
    }

    #[test]
    fn test_qualifier() {
        assert_eq!(
            qualifier("foobar"),
            Ok(("", RawToken::Qual("foobar".to_string())))
        );
        assert_eq!(
            qualifier("foo_bar"),
            Ok(("", RawToken::Qual("foo_bar".to_string())))
        );
        assert_eq!(
            qualifier("foo+bar"),
            Ok(("", RawToken::Qual("foo+bar".to_string())))
        );
        assert_eq!(
            qualifier("foo0bar"),
            Ok(("0bar", RawToken::Qual("foo".to_string())))
        );
    }

    #[test]
    fn test_tokens() {
        assert_eq!(
            tokens("1.2.3"),
            Ok((
                "",
                vec![
                    Token::Number {
                        prefix: Prefix::Hyphen,
                        value: 1
                    },
                    Token::Number {
                        prefix: Prefix::Dot,
                        value: 2
                    },
                    Token::Number {
                        prefix: Prefix::Dot,
                        value: 3
                    }
                ]
            ))
        );

        assert_eq!(
            tokens("1.2.3-foo"),
            Ok((
                "",
                vec![
                    Token::Number {
                        prefix: Prefix::Hyphen,
                        value: 1
                    },
                    Token::Number {
                        prefix: Prefix::Dot,
                        value: 2
                    },
                    Token::Number {
                        prefix: Prefix::Dot,
                        value: 3
                    },
                    Token::Qualifier {
                        prefix: Prefix::Hyphen,
                        value: "foo".to_string()
                    }
                ]
            ))
        );
    }
}
