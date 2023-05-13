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
    RNum(u32),
    RQual(String),
    RDot,
    RHyphen,
}

enum RawTokenType {
    RNumT,
    RQualT,
    RDotT,
    RHyphenT,
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
            Ok(RawToken::RNum(num))
        })
        .parse_next(input)
}

fn qualifier(input: &str) -> IResult<&str, RawToken> {
    take_while(1.., |c: char| match c {
        'a'..='z' | 'A'..='Z' | '+' | '_' => true,
        _ => false,
    })
    .map(|s: &str| RawToken::RQual(s.to_string()))
    .parse_next(input)
}

fn dot_separator(input: &str) -> IResult<&str, RawToken> {
    '.'.map(|_| RawToken::RDot).parse_next(input)
}

fn hyphen_separator(input: &str) -> IResult<&str, RawToken> {
    '-'.map(|_| RawToken::RHyphen).parse_next(input)
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
    use Prefix::{Dot, Hyphen, ImplicitHyphen};
    use RawToken::{RDot, RHyphen, RNum, RQual};
    use RawTokenType::{RDotT, RHyphenT, RNumT, RQualT};
    use Token::{Number, Qualifier};

    let raw_token_type = match raw_token {
        Some(RNum(_)) => Some(RNumT),
        Some(RQual(_)) => Some(RQualT),
        Some(RDot) => Some(RDotT),
        Some(RHyphen) => Some(RHyphenT),
        None => None,
    };

    #[rustfmt::skip]
    let token = match (prev_token_type, raw_token) {
        // Leading hyphen is the specifics of this implementation
        (None, Some(RNum(value)))  => Some(Number    { prefix: Hyphen, value }),
        (None, Some(RQual(value))) => Some(Qualifier { prefix: Hyphen, value }),
        
        // Empty tokens are replaced with "0"
        (None,           Some(RHyphen)) => Some(Number { prefix: Hyphen, value: 0 }),
        (None,           Some(RDot))    => Some(Number { prefix: Dot, value: 0 }),
        (Some(RHyphenT), Some(RHyphen)) => Some(Number { prefix: Hyphen, value: 0 }),
        (Some(RHyphenT), Some(RDot))    => Some(Number { prefix: Hyphen, value: 0 }),
        (Some(RDotT),    Some(RHyphen)) => Some(Number { prefix: Dot, value: 0 }),
        (Some(RDotT),    Some(RDot))    => Some(Number { prefix: Dot, value: 0 }),
        (Some(RHyphenT), None)          => Some(Number { prefix: Hyphen, value: 0 }),
        (Some(RDotT),    None)          => Some(Number { prefix: Dot, value: 0 }),

        // Normal transitions explicitly prefixed by '.' or '-'
        (Some(RHyphenT), Some(RNum(value)))  => Some(Number    { prefix: Hyphen, value }),
        (Some(RHyphenT), Some(RQual(value))) => Some(Qualifier { prefix: Hyphen, value }),
        (Some(RDotT),    Some(RNum(value)))  => Some(Number    { prefix: Dot, value }),
        (Some(RDotT),    Some(RQual(value))) => Some(Qualifier { prefix: Dot, value }),
        
        // A transition between digits and characters is equivalent to a hyphen
        (Some(RQualT), Some(RNum(value)))  => Some(Number    { prefix: ImplicitHyphen, value }),
        (Some(RNumT),  Some(RQual(value))) => Some(Qualifier { prefix: ImplicitHyphen, value }),

        // Transitions that don't produce new tokens
        (Some(RNumT),  Some(RNum(_)))  => None,
        (Some(RNumT),  Some(RHyphen))  => None,
        (Some(RNumT),  Some(RDot))     => None,
        (Some(RQualT), Some(RQual(_))) => None,
        (Some(RQualT), Some(RHyphen))  => None,
        (Some(RQualT), Some(RDot))     => None,
        (Some(RNumT),  None)           => None,
        (Some(RQualT), None)           => None,
        (None,         None)           => None
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
        assert_eq!(number("123"), Ok(("", RawToken::RNum(123))));
        assert_eq!(number("0"), Ok(("", RawToken::RNum(0))));
        assert_eq!(number("1"), Ok(("", RawToken::RNum(1))));
        assert_eq!(number("01"), Ok(("", RawToken::RNum(1))));
    }

    #[test]
    fn test_qualifier() {
        assert_eq!(
            qualifier("foobar"),
            Ok(("", RawToken::RQual("foobar".to_string())))
        );
        assert_eq!(
            qualifier("foo_bar"),
            Ok(("", RawToken::RQual("foo_bar".to_string())))
        );
        assert_eq!(
            qualifier("foo+bar"),
            Ok(("", RawToken::RQual("foo+bar".to_string())))
        );
        assert_eq!(
            qualifier("foo0bar"),
            Ok(("0bar", RawToken::RQual("foo".to_string())))
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
