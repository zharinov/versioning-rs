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
    DotChar,
    HyphenChar,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Separator {
    Dot,
    Hyphen,
}

#[derive(Debug, PartialEq)]
enum TokenValue {
    Qualifier(String),
    Number(u32),
}

#[derive(Debug, PartialEq)]
struct Token {
    prefix: Separator,
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
    '.'.map(|_| RawToken::DotChar).parse_next(input)
}

fn hyphen_separator(input: &str) -> IResult<&str, RawToken> {
    '-'.map(|_| RawToken::HyphenChar).parse_next(input)
}

fn raw_token(input: &str) -> IResult<&str, RawToken> {
    alt((number, dot_separator, hyphen_separator, qualifier)).parse_next(input)
}

fn raw_tokens(input: &str) -> IResult<&str, Vec<RawToken>> {
    repeat(1.., raw_token).parse_next(input)
}

fn calculate_token(current: &RawToken, previous: Option<&RawToken>) -> Option<Token> {
    use RawToken::{DotChar, HyphenChar, Num, Qual};
    use Separator as Sep;
    use TokenValue::{Number, Qualifier};

    let previous = if let Some(previous) = previous {
        previous
    } else {
        let (prefix, value) = match current {
            Num(val) => (Sep::Hyphen, Number(*val)),
            Qual(val) => (Sep::Hyphen, Qualifier(val.to_lowercase())),
            _ => (Sep::Hyphen, Number(0)),
        };
        return Some(Token { prefix, value });
    };

    #[rustfmt::skip]
    let (prefix, value) = match (previous, current) {
        // Empty tokens are replaced with "0", e.g. '..1' is equivalent to '0.0.1'
        (DotChar,    DotChar | HyphenChar) => (Sep::Dot,    Number(0)),
        (HyphenChar, DotChar | HyphenChar) => (Sep::Hyphen, Number(0)),

        // Normal transitions separated by '.' or '-'
        (HyphenChar, Num(val))  => (Sep::Hyphen, Number(*val)),
        (HyphenChar, Qual(val)) => (Sep::Hyphen, Qualifier(val.to_lowercase())),
        (DotChar,    Num(val))  => (Sep::Dot,    Number(*val)),
        (DotChar,    Qual(val)) => (Sep::Dot,    Qualifier(val.to_lowercase())),
        
        // Transition between digits and characters is equivalent to a hyphen
        (Qual(_), Num(val))  => (Sep::Hyphen, Number(*val)),
        (Num(_),  Qual(val)) => (Sep::Hyphen, Qualifier(val.to_lowercase())),

        // Skip separator chars
        (Num(_) | Qual(_), DotChar | HyphenChar) => return None,

        // Parsing at the previous stage is incorrect
        (Num(_), Num(_)) => unreachable!("Two consecutive numbers"),
        (Qual(_), Qual(_)) => unreachable!("Two consecutive qualifiers"),
    };

    Some(Token { prefix, value })
}

fn is_null(token: &Token) -> bool {
    match &token.value {
        TokenValue::Number(0) => true,
        TokenValue::Qualifier(x) => x.is_empty() || x == "final" || x == "ga" || x == "release",
        _ => false,
    }
}

fn trim_nulls(tokens: &mut Vec<Token>) {
    while let Some(token) = tokens.last() {
        if is_null(token) {
            tokens.pop();
        } else {
            break;
        }
    }
}

fn tokens(input: &str) -> IResult<&str, Vec<Token>> {
    raw_tokens
        .map(|raw_tokens: Vec<RawToken>| {
            let mut tokens: Vec<Token> = Vec::with_capacity(raw_tokens.len() + 1);
            let mut prev: Option<&RawToken> = None;
            for current in &raw_tokens {
                let token = calculate_token(current, prev);
                if let Some(token) = token {
                    // The `alpha`, `beta` and `milestone` qualifiers can respectively be shortened
                    // to "a", "b" and "m" when directly followed by a number.
                    // This is a special case that is not handled by `calculate_token`.
                    if let (Some(&RawToken::Qual(q)), TokenValue::Number(_)) = (prev, &token.value)
                    {
                        if let Some(s) = match q {
                            "a" => Some("alpha".to_string()),
                            "b" => Some("beta".to_string()),
                            "m" => Some("milestone".to_string()),
                            _ => None,
                        } {
                            if let Some(last) = tokens.last_mut() {
                                last.value = TokenValue::Qualifier(s);
                            }
                        }
                    }

                    if token.prefix == Separator::Hyphen {
                        trim_nulls(&mut tokens);
                    }

                    tokens.push(token);
                }

                prev = Some(current);
            }
            trim_nulls(&mut tokens);

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
            (Separator::Dot, TokenValue::Qualifier(_)) => 1,
            (Separator::Hyphen, TokenValue::Qualifier(_)) => 2,
            (Separator::Hyphen, TokenValue::Number(_)) => 3,
            (Separator::Dot, TokenValue::Number(_)) => 4,
        }
    }

    let left_rank = token_rank(left);
    let right_rank = token_rank(right);
    if left_rank != right_rank {
        return left_rank.cmp(&right_rank);
    }

    fn qualifier_rank(token: &Token) -> Option<usize> {
        match &token.value {
            TokenValue::Qualifier(value) => match value.as_str() {
                "alpha" => Some(ALPHA_RANK),
                "beta" => Some(BETA_RANK),
                "milestone" => Some(MILESTONE_RANK),
                "rc" | "cr" | "preview" => Some(RELEASE_CANDIDATE_RANK),
                "snapshot" => Some(SNAPSHOT_RANK),
                "" | "final" | "ga" | "release" | "latest" | "sr" => Some(RELEASE_RANK),
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
        (Some(left_rank), _) if left_rank < RELEASE_RANK => Ordering::Less,
        (_, Some(right_rank)) if right_rank < RELEASE_RANK => Ordering::Greater,
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
                _ => unreachable!(),
            };
        }

        Some(Ordering::Equal)
    }
}

#[cfg(test)]
#[path = "version_tests.rs"]
mod version_tests;
