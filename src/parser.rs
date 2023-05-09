use nom::bytes::complete::tag;
use nom::character::complete::u64;
use nom::combinator::map;
use nom::sequence::tuple;
use nom::IResult;

#[derive(Debug, PartialEq)]
pub struct Semver {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
}

impl Semver {
    pub fn new(major: u64, minor: u64, patch: u64) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }
}

#[allow(dead_code)]
pub fn semver(input: &str) -> IResult<&str, Semver> {
    map(
        tuple((u64, tag("."), u64, tag("."), u64)),
        |(major, _, minor, _, patch)| Semver::new(major, minor, patch),
    )(input)
}

#[test]
fn test_semver() {
    assert_eq!(semver("1.2.3"), Ok(("", Semver::new(1, 2, 3))));
    assert_eq!(semver("1.2.3-alpha"), Ok(("-alpha", Semver::new(1, 2, 3))));
}

#[allow(dead_code)]
pub fn range(input: &str) -> IResult<&str, (Semver, Semver)> {
    map(tuple((semver, tag(" - "), semver)), |(start, _, end)| {
        (start, end)
    })(input)
}

#[test]
fn test_range() {
    assert_eq!(
        range("1.2.3 - 2.3.4"),
        Ok(("", (Semver::new(1, 2, 3), Semver::new(2, 3, 4))))
    );
}
