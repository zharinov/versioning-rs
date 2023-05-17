use super::*;
use insta::*;
use rstest::rstest;
use serde::ser::{Serialize, SerializeStruct, Serializer};

#[rstest]
#[case("0", 0)]
#[case("1", 1)]
#[case("123", 123)]
#[case("000123", 123)]
#[case("000123000", 123000)]
fn parse_number(#[case] input: &str, #[case] expected: u32) {
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
        let (_, output) = tokens(input).unwrap();
        output
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
    assert_eq!(tokens(input), tokens(expected));
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
fn equality(#[case] left: &str, #[case] right: &str) {
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
fn comparison(#[case] left: &str, #[case] right: &str) {
    let left = Version::new(left).unwrap();
    let right = Version::new(right).unwrap();
    assert_eq!(left.partial_cmp(&right), Some(Ordering::Less));
    assert_eq!(right.partial_cmp(&left), Some(Ordering::Greater));
}
