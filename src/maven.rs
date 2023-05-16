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
        TokenValue::Qualifier(x) => x == "" || x == "final" || x == "ga" || x == "release",
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
                let token = calculate_token(&current, prev);
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
mod maven_test {
    use super::*;

    #[test]
    fn parse_number() {
        assert_eq!(number("123"), Ok(("", RawToken::Num(123))));
        assert_eq!(number("0"), Ok(("", RawToken::Num(0))));
        assert_eq!(number("1"), Ok(("", RawToken::Num(1))));
        assert_eq!(number("01"), Ok(("", RawToken::Num(1))));
    }

    #[test]
    fn parse_qualifier() {
        assert_eq!(qualifier("foobar"), Ok(("", RawToken::Qual("foobar"))));
        assert_eq!(qualifier("foo_bar"), Ok(("", RawToken::Qual("foo_bar"))));
        assert_eq!(qualifier("foo+bar"), Ok(("", RawToken::Qual("foo+bar"))));
        assert_eq!(qualifier("foo0bar"), Ok(("0bar", RawToken::Qual("foo"))));
    }

    #[test]
    fn tokenize() {
        assert_eq!(
            tokens("1.2.3"),
            Ok((
                "",
                vec![
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Number(1),
                    },
                    Token {
                        prefix: Separator::Dot,
                        value: TokenValue::Number(2),
                    },
                    Token {
                        prefix: Separator::Dot,
                        value: TokenValue::Number(3),
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
                        prefix: Separator::Hyphen,
                        value: TokenValue::Number(1),
                    },
                    Token {
                        prefix: Separator::Dot,
                        value: TokenValue::Number(2),
                    },
                    Token {
                        prefix: Separator::Dot,
                        value: TokenValue::Number(3),
                    },
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Qualifier("foo".to_string()),
                    }
                ]
            ))
        );

        assert_eq!(
            tokens("1.2.3"),
            Ok((
                "",
                vec![
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Number(1),
                    },
                    Token {
                        prefix: Separator::Dot,
                        value: TokenValue::Number(2),
                    },
                    Token {
                        prefix: Separator::Dot,
                        value: TokenValue::Number(3),
                    },
                ]
            ))
        );

        assert_eq!(
            tokens("m1"),
            Ok((
                "",
                vec![
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Qualifier("milestone".to_string()),
                    },
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Number(1),
                    },
                ]
            ))
        );

        assert_eq!(
            tokens("1m"),
            Ok((
                "",
                vec![
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Number(1),
                    },
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Qualifier("m".to_string()),
                    },
                ]
            ))
        );

        assert_eq!(
            tokens("0m0"),
            Ok((
                "",
                vec![Token {
                    prefix: Separator::Hyphen,
                    value: TokenValue::Qualifier("milestone".to_string()),
                },]
            ))
        );

        assert_eq!(
            tokens("1-1.foo-bar1baz-.1"),
            Ok((
                "",
                vec![
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Number(1)
                    },
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Number(1)
                    },
                    Token {
                        prefix: Separator::Dot,
                        value: TokenValue::Qualifier("foo".to_string()),
                    },
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Qualifier("bar".to_string()),
                    },
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Number(1),
                    },
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Qualifier("baz".to_string()),
                    },
                    Token {
                        prefix: Separator::Hyphen,
                        value: TokenValue::Number(0),
                    },
                    Token {
                        prefix: Separator::Dot,
                        value: TokenValue::Number(1),
                    },
                ]
            ))
        );

        assert_eq!(
            tokens("1-1.foo-bar1baz-.1"),
            tokens("1-1.foo-bar-1-baz-0.1"),
        );

        assert_eq!(tokens("1.0.0"), tokens("1"),);
        assert_eq!(tokens("1.ga"), tokens("1"),);
        assert_eq!(tokens("1.final"), tokens("1"),);
        assert_eq!(tokens("1.0"), tokens("1"),);
        assert_eq!(tokens("1."), tokens("1"),);
        assert_eq!(tokens("1-"), tokens("1"),);
        assert_eq!(tokens("1.0.0-foo.0.0"), tokens("1-foo"),);
        assert_eq!(tokens("1.0.0-0.0.0"), tokens("1"),);
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

    use rstest::rstest;

    #[rstest]
    #[case("1", "1")]
    #[case("1", "1.0")]
    #[case("1", "1.0.0")]
    #[case("1.0", "1.0.0")]
    #[case("1", "1-0")]
    #[case("1", "1.0-0")]
    #[case("1.0", "1.0-0")]
    // no separator between number and character
    #[case("1a", "1-a")]
    #[case("1a", "1-a")]
    #[case("1a", "1.0-a")]
    #[case("1a", "1.0.0-a")]
    #[case("1.0a", "1-a")]
    #[case("1.0.0a", "1-a")]
    #[case("1x", "1-x")]
    #[case("1x", "1.0-x")]
    #[case("1x", "1.0.0-x")]
    #[case("1.0x", "1-x")]
    #[case("1.0.0x", "1-x")]
    // aliases
    #[case("1ga", "1")]
    #[case("1release", "1")]
    #[case("1final", "1")]
    #[case("1cr", "1rc")]
    // special "aliases" a, b and m for alpha, beta and milestone
    #[case("1a1", "1-alpha-1")]
    #[case("1b2", "1-beta-2")]
    #[case("1m3", "1-milestone-3")]
    // case insensitive
    #[case("1X", "1x")]
    #[case("1A", "1a")]
    #[case("1B", "1b")]
    #[case("1M", "1m")]
    #[case("1Ga", "1")]
    #[case("1GA", "1")]
    #[case("1RELEASE", "1")]
    #[case("1release", "1")]
    #[case("1RELeaSE", "1")]
    #[case("1Final", "1")]
    #[case("1FinaL", "1")]
    #[case("1FINAL", "1")]
    #[case("1Cr", "1Rc")]
    #[case("1cR", "1rC")]
    #[case("1m3", "1Milestone3")]
    #[case("1m3", "1MileStone3")]
    #[case("1m3", "1MILESTONE3")]
    fn version_equality(#[case] left: &str, #[case] right: &str) {
        let left = Version::new(left).unwrap();
        let right = Version::new(right).unwrap();
        assert_eq!(left.partial_cmp(&right), Some(Ordering::Equal));
        assert_eq!(right.partial_cmp(&left), Some(Ordering::Equal));
    }

    #[rstest]
    #[case("1", "2")]
    #[case("1.5", "2")]
    #[case("1", "2.5")]
    #[case("1.0", "1.1")]
    #[case("1.1", "1.2")]
    #[case("1.0.0", "1.1")]
    #[case("1.0.1", "1.1")]
    #[case("1.1", "1.2.0")]
    #[case("1.0-alpha-1", "1.0")]
    #[case("1.0-alpha-1", "1.0-alpha-2")]
    #[case("1.0-alpha-1", "1.0-beta-1")]
    #[case("1.0-beta-1", "1.0-SNAPSHOT")]
    #[case("1.0-SNAPSHOT", "1.0")]
    #[case("1.0-alpha-1-SNAPSHOT", "1.0-alpha-1")]
    #[case("1.0", "1.0-1")]
    #[case("1.0-1", "1.0-2")]
    #[case("1.0.0", "1.0-1")]
    #[case("2.0-1", "2.0.1")]
    #[case("2.0.1-klm", "2.0.1-lmn")]
    #[case("2.0.1", "2.0.1-xyz")]
    #[case("2.0.1", "2.0.1-123")]
    #[case("2.0.1-xyz", "2.0.1-123")]
    fn version_compare(#[case] left: &str, #[case] right: &str) {
        let left = Version::new(left).unwrap();
        let right = Version::new(right).unwrap();
        assert_eq!(left.partial_cmp(&right), Some(Ordering::Less));
        assert_eq!(right.partial_cmp(&left), Some(Ordering::Greater));
    }
}
