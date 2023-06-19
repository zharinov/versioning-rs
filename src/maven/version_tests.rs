#![cfg_attr(coverage_nightly, no_coverage)]

use super::*;
use insta::*;
use itertools::Itertools;
use rstest::rstest;
use serde::ser::{Serialize, SerializeStruct, Serializer};

/// @see https://github.com/apache/maven/blob/master/maven-artifact/src/test/java/org/apache/maven/artifact/versioning/ComparableVersionTest.java

#[rstest]
#[case("0", 0)]
#[case("1", 1)]
#[case("123", 123)]
#[case("000123", 123)]
#[case("000123000", 123000)]
fn parse_number(#[case] input: &str, #[case] expected: u64) {
    let (_, res) = number(input).unwrap();
    assert_eq!(res, RawToken::Num(expected));
}

#[rstest]
#[case("foobar", "foobar")]
#[case("foo_bar", "foo_bar")]
#[case("foo+bar", "foo+bar")]
#[case("foo0bar", "foo")]
#[case("foo.bar", "foo")]
#[case("foo-bar", "foo")]
fn parse_qualifier(#[case] input: &str, #[case] expected: &str) {
    let (_, res) = qualifier(input).unwrap();
    assert_eq!(res, RawToken::Qual(expected));
}

impl Serialize for Separator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Separator::Dot => serializer.serialize_unit_variant("Separator", 0, "Dot"),
            Separator::Hyphen => serializer.serialize_unit_variant("Separator", 0, "Hyphen"),
        }
    }
}

impl Serialize for TokenValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            TokenValue::Number(num) => {
                serializer.serialize_newtype_variant("TokenValue", 0, "Number", num)
            }
            TokenValue::Qualifier(qual) => {
                serializer.serialize_newtype_variant("TokenValue", 1, "Qualifier", qual)
            }
        }
    }
}

impl Serialize for Token {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Token", 2)?;
        state.serialize_field("prefix", &self.prefix)?;
        state.serialize_field("value", &self.value)?;
        state.end()
    }
}

struct TestSnapshot<I, O>
where
    I: Serialize,
    O: Serialize,
{
    input: I,
    output: O,
}

impl<I, O> Serialize for TestSnapshot<I, O>
where
    I: Serialize,
    O: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("TestingPair", 2)?;
        state.serialize_field("input", &self.input)?;
        state.serialize_field("output", &self.output)?;
        state.end()
    }
}

fn snapshot<I, O>(input: I, output: O) -> TestSnapshot<I, O>
where
    I: Serialize,
    O: Serialize,
{
    TestSnapshot { input, output }
}

#[test]
fn tokenization() {
    let get_tokens = |input: &str| -> Vec<Token> {
        let (_, output) = version_tokens(input).unwrap();
        output.tokens
    };

    assert_yaml_snapshot!(get_tokens("1.2.3"));
    assert_yaml_snapshot!(get_tokens("1.2.3-foo"));
    assert_yaml_snapshot!(get_tokens("m1")); // -> milestone-1
    assert_yaml_snapshot!(get_tokens("1m")); // -> 1-m
    assert_yaml_snapshot!(get_tokens("0m0")); // -> 0-m-0
    assert_yaml_snapshot!(get_tokens("1-1.foo-bar1baz-.1")); // Example from Pomfile docs
}

#[rstest]
#[case("1-1.foo-bar1baz-.1", "1-1.foo-bar-1-baz-0.1")]
#[case("1.0.0", "1")]
#[case("1.ga", "1")]
#[case("1.final", "1")]
#[case("1.0", "1")]
#[case("1.", "1")]
#[case("1-", "1")]
#[case("1.0.0-foo.0.0", "1-foo")]
#[case("1.0.0-0.0.0", "1")]
fn equivalent_tokenization(#[case] input: &str, #[case] expected: &str) {
    assert_eq!(version_tokens(input), version_tokens(expected));
}

#[test]
fn version_constructor() {
    assert_eq!(
        Version::parse("1.2.3").unwrap(),
        Version {
            tokens: vec![
                Token {
                    prefix: Separator::Hyphen,
                    value: TokenValue::Number(1)
                },
                Token {
                    prefix: Separator::Dot,
                    value: TokenValue::Number(2)
                },
                Token {
                    prefix: Separator::Dot,
                    value: TokenValue::Number(3)
                }
            ]
        }
    );

    assert!(Version::parse("1.2.3=#!").is_none());
}

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
#[case("1.x", "1-x")]
#[case("1.0.0.x", "1-x")]
#[case("1.x", "1.0.0-x")]
fn equality(#[case] left: &str, #[case] right: &str) {
    let left = Version::parse(left).unwrap();
    let right = Version::parse(right).unwrap();
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
fn comparison(#[case] left: &str, #[case] right: &str) {
    let left = Version::parse(left).unwrap();
    let right = Version::parse(right).unwrap();
    assert_eq!(left.partial_cmp(&right), Some(Ordering::Less));
    assert_eq!(right.partial_cmp(&left), Some(Ordering::Greater));
}

/// @see https://issues.apache.org/jira/browse/MNG-5568
#[test]
fn mng_5568() {
    let a = Version::parse("6.1.0").unwrap();
    let b = Version::parse("6.1.0rc3").unwrap();
    let c = Version::parse("6.1H.5-beta").unwrap(); // this is the unusual version string, with 'H' in the middle

    assert_eq!(b.partial_cmp(&a), Some(Ordering::Less)); // classical
    assert_eq!(b.partial_cmp(&c), Some(Ordering::Less)); // now b < c, but before MNG-5568, we had b > c
    assert_eq!(a.partial_cmp(&c), Some(Ordering::Less));
}

/// @see https://jira.apache.org/jira/browse/MNG-6572
#[test]
fn mng_6572() {
    let a = Version::parse("20190126.230843").unwrap(); // resembles a SNAPSHOT
    let b = Version::parse("1234567890.12345").unwrap(); // 10 digit number
    let c = Version::parse("123456789012345.1H.5-beta").unwrap(); // 15 digit number
    let d = Version::parse("12345678901234567890.1H.5-beta").unwrap(); // 20 digit number

    assert_eq!(a.partial_cmp(&b), Some(Ordering::Less));
    assert_eq!(b.partial_cmp(&c), Some(Ordering::Less));
    assert_eq!(a.partial_cmp(&c), Some(Ordering::Less));
    assert_eq!(c.partial_cmp(&d), Some(Ordering::Less));
    assert_eq!(b.partial_cmp(&d), Some(Ordering::Less));
    assert_eq!(a.partial_cmp(&d), Some(Ordering::Less));
}

#[test]
fn version_equal_with_leading_zeroes() {
    let versions = vec![
        "0000000000000000001",
        "000000000000000001",
        "00000000000000001",
        "0000000000000001",
        "000000000000001",
        "00000000000001",
        "0000000000001",
        "000000000001",
        "00000000001",
        "0000000001",
        "000000001",
        "00000001",
        "0000001",
        "000001",
        "00001",
        "0001",
        "001",
        "01",
        "1",
    ];

    for combination in versions.into_iter().combinations(2) {
        let (left, right) = (combination[0], combination[1]);
        let left = Version::parse(left).unwrap();
        let right = Version::parse(right).unwrap();
        assert_eq!(left.partial_cmp(&right), Some(Ordering::Equal));
        assert_eq!(right.partial_cmp(&left), Some(Ordering::Equal));
    }
}

#[test]
fn test_version_zero_equal_with_leading_zeroes() {
    let versions = vec![
        "0000000000000000000",
        "000000000000000000",
        "00000000000000000",
        "0000000000000000",
        "000000000000000",
        "00000000000000",
        "0000000000000",
        "000000000000",
        "00000000000",
        "0000000000",
        "000000000",
        "00000000",
        "0000000",
        "000000",
        "00000",
        "0000",
        "000",
        "00",
        "0",
    ];

    for combination in versions.into_iter().combinations(2) {
        let (left, right) = (combination[0], combination[1]);
        let left = Version::parse(left).unwrap();
        let right = Version::parse(right).unwrap();
        assert_eq!(left.partial_cmp(&right), Some(Ordering::Equal));
        assert_eq!(right.partial_cmp(&left), Some(Ordering::Equal));
    }
}

/// @see https://issues.apache.org/jira/browse/MNG-6964
#[test]
fn test_mng_6964() {
    let a = Version::parse("1-0.alpha").unwrap();
    let b = Version::parse("1-0.beta").unwrap();
    let c = Version::parse("1").unwrap();

    assert_eq!(a.partial_cmp(&c), Some(Ordering::Less)); // Now a < c, but before MNG-6964 they were equal
    assert_eq!(b.partial_cmp(&c), Some(Ordering::Less)); // Now b < c, but before MNG-6964 they were equal
    assert_eq!(a.partial_cmp(&b), Some(Ordering::Less)); // Should still be true
}

/// @see https://issues.apache.org/jira/browse/MNG-7644
#[test]
fn test_mng_7644() {
    let quals = vec![
        "abc",
        "alpha",
        "a",
        "beta",
        "b",
        "def",
        "milestone",
        "m",
        "RC",
    ];

    for qual in quals {
        // 1.0.0.X1 < 1.0.0-X2 for any string x
        let a = Version::parse(&format!("1.0.0.{}1", qual)).unwrap();
        let b = Version::parse(&format!("1.0.0-{}2", qual)).unwrap();
        assert_eq!(a.partial_cmp(&b), Some(Ordering::Less));

        // 2.0.X == 2-X == 2.0.0.X for any string x
        let c = Version::parse(&format!("2-{}", qual)).unwrap();
        let d = Version::parse(&format!("2.0.{}", qual)).unwrap();
        let e = Version::parse(&format!("2.0.0.{}", qual)).unwrap();
        assert_eq!(c.partial_cmp(&d), Some(Ordering::Equal)); // previously ordered, now equals
        assert_eq!(c.partial_cmp(&e), Some(Ordering::Equal)); // previously ordered, now equals
        assert_eq!(d.partial_cmp(&e), Some(Ordering::Equal)); // previously ordered, now equals
    }
}
