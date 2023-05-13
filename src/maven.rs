#![allow(dead_code)]

use std::cmp::Ordering;
use winnow::ascii::digit1;
use winnow::combinator::{alt, repeat};
use winnow::token::take_while;
use winnow::{IResult, Parser};

#[derive(Debug, PartialEq)]
enum RawToken<'a> {
    Num(u32),
    Qual(&'a str),
    Dot,
    Hyphen,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Prefix {
    Dot,
    Hyphen,
    ImplicitHyphen,
}

#[derive(Debug, PartialEq)]
enum TokenValue {
    Qualifier(String),
    Number(u32),
}

#[derive(Debug, PartialEq)]
struct Token {
    prefix: Prefix,
    value: TokenValue,
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
    .map(RawToken::Qual)
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
    prev_raw_token: Option<RawToken>,
    current_raw_token: Option<&RawToken>,
) -> Option<Token> {
    use RawToken::{Dot, Hyphen, Num, Qual};
    use TokenValue::{Number, Qualifier};

    #[rustfmt::skip]
    let token = match (prev_raw_token, current_raw_token) {
        // Leading hyphen is the specifics of this implementation
        (None, Some(Num(val)))  => Some(Token { prefix: Prefix::Hyphen, value: Number(*val) }),
        (None, Some(Qual(val))) => Some(Token { prefix: Prefix::Hyphen, value: Qualifier(val.to_string()) }),
        
        // Empty tokens are replaced with "0"
        (None,         Some(Hyphen)) => Some(Token { prefix: Prefix::Hyphen, value: Number(0) }),
        (None,         Some(Dot))    => Some(Token { prefix: Prefix::Dot,    value: Number(0) }),
        (Some(Hyphen), Some(Hyphen)) => Some(Token { prefix: Prefix::Hyphen, value: Number(0) }),
        (Some(Hyphen), Some(Dot))    => Some(Token { prefix: Prefix::Hyphen, value: Number(0) }),
        (Some(Dot),    Some(Hyphen)) => Some(Token { prefix: Prefix::Dot,    value: Number(0) }),
        (Some(Dot),    Some(Dot))    => Some(Token { prefix: Prefix::Dot,    value: Number(0) }),
        (Some(Hyphen), None)         => Some(Token { prefix: Prefix::Hyphen, value: Number(0) }),
        (Some(Dot),    None)         => Some(Token { prefix: Prefix::Dot,    value: Number(0) }),

        // Normal transitions explicitly prefixed by '.' or '-'
        (Some(Hyphen), Some(Num(val)))  => Some(Token { prefix: Prefix::Hyphen, value: Number(*val) }),
        (Some(Hyphen), Some(Qual(val))) => Some(Token { prefix: Prefix::Hyphen, value: Qualifier(val.to_string()) }),
        (Some(Dot),    Some(Num(val)))  => Some(Token { prefix: Prefix::Dot,    value: Number(*val) }),
        (Some(Dot),    Some(Qual(val))) => Some(Token { prefix: Prefix::Dot,    value: Qualifier(val.to_string()) }),
        
        // A transition between digits and characters is equivalent to a hyphen
        (Some(Qual(_)), Some(Num(val)))  => Some(Token { prefix: Prefix::ImplicitHyphen, value: Number(*val) }),
        (Some(Num(_)),  Some(Qual(val))) => Some(Token { prefix: Prefix::ImplicitHyphen, value: Qualifier(val.to_string()) }),

        // Transitions that don't produce new tokens
        (Some(Num(_)),  Some(Num(_)))  => None,
        (Some(Num(_)),  Some(Hyphen))  => None,
        (Some(Num(_)),  Some(Dot))     => None,
        (Some(Qual(_)), Some(Qual(_))) => None,
        (Some(Qual(_)), Some(Hyphen))  => None,
        (Some(Qual(_)), Some(Dot))     => None,
        (Some(Num(_)),  None)          => None,
        (Some(Qual(_)), None)          => None,
        (None,          None)          => None,
    };

    token
}

fn tokens(input: &str) -> IResult<&str, Vec<Token>> {
    raw_tokens
        .map(|raw_tokens: Vec<RawToken>| {
            let max_length = raw_tokens.len() + 1;
            let mut tokens: Vec<Token> = Vec::with_capacity(max_length);

            let mut prev_raw_token: Option<RawToken> = None;
            for raw_token in raw_tokens {
                let token = calculate_token(prev_raw_token, Some(&raw_token));
                prev_raw_token = Some(raw_token);
                if let Some(token) = token {
                    tokens.push(token);
                }
            }

            let token = calculate_token(prev_raw_token, None);
            if let Some(token) = token {
                tokens.push(token);
            }

            tokens
        })
        .parse_next(input)
}

const ALPHA_RANK: usize = 1;
const BETA_RANK: usize = 2;
const MILESTONE_RANK: usize = 3;
const RELEASE_CANDIDATE_RANK: usize = 4;
const SNAPSHOT_RANK: usize = 5;
const RELEASE_RANK: usize = 6;
const SERVICE_PACK_RANK: usize = 7;

fn cmp_tokens(left: &Token, right: &Token) -> Ordering {
    fn token_rank(token: &Token) -> usize {
        match (&token.prefix, &token.value) {
            (Prefix::Dot, TokenValue::Qualifier(_)) => 1,
            (Prefix::Hyphen | Prefix::ImplicitHyphen, TokenValue::Qualifier(_)) => 2,
            (Prefix::Hyphen | Prefix::ImplicitHyphen, TokenValue::Number(_)) => 3,
            (Prefix::Dot, TokenValue::Number(_)) => 4,
        }
    }

    let left_rank = token_rank(left);
    let right_rank = token_rank(right);
    if left_rank != right_rank {
        return left_rank.cmp(&right_rank);
    }

    fn qualifier_rank(token: &Token) -> Option<usize> {
        match token.value {
            TokenValue::Qualifier(ref value) => match value.as_str() {
                "alpha" => Some(ALPHA_RANK),
                "a" if token.prefix == Prefix::ImplicitHyphen => Some(ALPHA_RANK),

                "beta" => Some(BETA_RANK),
                "b" if token.prefix == Prefix::ImplicitHyphen => Some(BETA_RANK),

                "milestone" => Some(MILESTONE_RANK),
                "m" if token.prefix == Prefix::ImplicitHyphen => Some(MILESTONE_RANK),

                "rc" => Some(RELEASE_CANDIDATE_RANK),
                "cr" => Some(RELEASE_CANDIDATE_RANK),
                "preview" => Some(RELEASE_CANDIDATE_RANK),

                "snapshot" => Some(SNAPSHOT_RANK),

                "" => Some(RELEASE_RANK),
                "final" => Some(RELEASE_RANK),
                "ga" => Some(RELEASE_RANK),
                "release" => Some(RELEASE_RANK),
                "latest" => Some(RELEASE_RANK),
                "sr" => Some(RELEASE_RANK),

                "sp" => Some(SERVICE_PACK_RANK),

                _ => None,
            },
            _ => None,
        }
    }

    let left_rank = qualifier_rank(left);
    let right_rank = qualifier_rank(right);
    match (left_rank, right_rank) {
        (Some(left_rank), Some(right_rank)) => left_rank.cmp(&right_rank),
        (Some(left_rank), _) if left_rank < 6 => Ordering::Less,
        (_, Some(right_rank)) if right_rank < 6 => Ordering::Greater,
        _ => match (&left.value, &right.value) {
            (TokenValue::Number(left), TokenValue::Number(right)) => left.cmp(right),
            (TokenValue::Qualifier(left), TokenValue::Qualifier(right)) => left.cmp(right),
            _ => unreachable!(),
        },
    }
}

#[derive(Debug, PartialEq)]
struct Version {
    tokens: Vec<Token>,
}

impl Version {
    fn new(input: &str) -> Result<Version, String> {
        let (_, tokens) = tokens(input).map_err(|err| format!("{}", err))?;
        Ok(Version { tokens })
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Version) -> Option<Ordering> {
        fn null_token(counterpart: &Token) -> Token {
            let prefix = counterpart.prefix;
            match counterpart.value {
                TokenValue::Number(_) => Token {
                    prefix,
                    value: TokenValue::Number(0),
                },
                TokenValue::Qualifier(_) => Token {
                    prefix,
                    value: TokenValue::Qualifier("".to_string()),
                },
            }
        }

        let left_len = self.tokens.len();
        let right_len = other.tokens.len();
        let max_len = left_len.max(right_len);

        for i in 0..max_len {
            let left_token = self.tokens.get(i);
            let right_token = other.tokens.get(i);

            match (left_token, right_token) {
                (Some(left_token), Some(right_token)) => {
                    let order = cmp_tokens(left_token, right_token);
                    if order != Ordering::Equal {
                        return Some(order);
                    }
                }
                (Some(left_token), None) => {
                    let right_token = null_token(left_token);
                    let order = cmp_tokens(left_token, &right_token);
                    if order != Ordering::Equal {
                        return Some(order);
                    }
                }
                (None, Some(right_token)) => {
                    let left_token = null_token(right_token);
                    let order = cmp_tokens(&left_token, right_token);
                    if order != Ordering::Equal {
                        return Some(order);
                    }
                }
                (None, None) => return None,
            }
        }

        Some(Ordering::Equal)
    }
}

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
        assert_eq!(qualifier("foobar"), Ok(("", RawToken::Qual("foobar"))));
        assert_eq!(qualifier("foo_bar"), Ok(("", RawToken::Qual("foo_bar"))));
        assert_eq!(qualifier("foo+bar"), Ok(("", RawToken::Qual("foo+bar"))));
        assert_eq!(qualifier("foo0bar"), Ok(("0bar", RawToken::Qual("foo"))));
    }

    #[test]
    fn test_tokens() {
        assert_eq!(
            tokens("1.2.3"),
            Ok((
                "",
                vec![
                    Token {
                        prefix: Prefix::Hyphen,
                        value: TokenValue::Number(1)
                    },
                    Token {
                        prefix: Prefix::Dot,
                        value: TokenValue::Number(2)
                    },
                    Token {
                        prefix: Prefix::Dot,
                        value: TokenValue::Number(3)
                    }
                ]
            ))
        );

        assert_eq!(
            tokens("1.2.3-foo"),
            Ok((
                "",
                vec![
                    Token {
                        prefix: Prefix::Hyphen,
                        value: TokenValue::Number(1)
                    },
                    Token {
                        prefix: Prefix::Dot,
                        value: TokenValue::Number(2)
                    },
                    Token {
                        prefix: Prefix::Dot,
                        value: TokenValue::Number(3)
                    },
                    Token {
                        prefix: Prefix::Hyphen,
                        value: TokenValue::Qualifier("foo".to_string())
                    }
                ]
            ))
        );
    }

    #[test]
    fn test_versions() {
        let left = Version::new("1.2.3").unwrap();
        let right = Version::new("1.2.3").unwrap();
        assert_eq!(left, right);

        let left = Version::new("1.2.3").unwrap();
        let right = Version::new("1.2.4").unwrap();
        assert!(left < right);
        assert!(right > left);

        let left = Version::new("1.2.3").unwrap();
        let right = Version::new("1.2.3-foo").unwrap();
        assert!(left < right);
        assert!(right > left);
    }
}
