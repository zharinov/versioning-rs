#![allow(dead_code)]

use winnow::ascii::space0;
use winnow::combinator::{alt, opt, separated1};
use winnow::{IResult, Parser};

use crate::maven::version::{version, Version};
use crate::range::{Direction, Edge, Interval};

fn left_open_edge_char(input: &str) -> IResult<&str, char> {
    let (input, _) = ('(', space0).parse_next(input)?;
    Ok((input, '('))
}

fn left_open_edge(input: &str) -> IResult<&str, Edge> {
    left_open_edge_char.map(|_| Edge::Open).parse_next(input)
}

fn left_closed_edge_char(input: &str) -> IResult<&str, char> {
    let (input, _) = ('[', space0).parse_next(input)?;
    Ok((input, '['))
}

fn left_closed_edge(input: &str) -> IResult<&str, Edge> {
    left_closed_edge_char
        .map(|_| Edge::Closed)
        .parse_next(input)
}

fn left_edge_char(input: &str) -> IResult<&str, char> {
    alt((left_open_edge_char, left_closed_edge_char)).parse_next(input)
}

fn left_edge(input: &str) -> IResult<&str, Edge> {
    alt((left_open_edge, left_closed_edge)).parse_next(input)
}

fn right_open_edge_char(input: &str) -> IResult<&str, char> {
    let (input, _) = (space0, ')').parse_next(input)?;
    Ok((input, ')'))
}

fn right_open_edge(input: &str) -> IResult<&str, Edge> {
    right_open_edge_char.map(|_| Edge::Open).parse_next(input)
}

fn right_closed_edge_char(input: &str) -> IResult<&str, char> {
    let (input, _) = (space0, ']').parse_next(input)?;
    Ok((input, ']'))
}

fn right_closed_edge(input: &str) -> IResult<&str, Edge> {
    right_closed_edge_char
        .map(|_| Edge::Closed)
        .parse_next(input)
}

fn right_edge_char(input: &str) -> IResult<&str, char> {
    alt((right_open_edge_char, right_closed_edge_char)).parse_next(input)
}

fn right_edge(input: &str) -> IResult<&str, Edge> {
    alt((right_open_edge, right_closed_edge)).parse_next(input)
}

fn delimiter(input: &str) -> IResult<&str, char> {
    let (input, _) = (space0, ',', space0).parse_next(input)?;
    Ok((input, ','))
}

type IntervalPair = (Option<Interval<Version>>, Option<Interval<Version>>);

fn left_unbounded_interlval(input: &str) -> IResult<&str, IntervalPair> {
    (left_open_edge, delimiter, version, right_edge)
        .map(|(_, _, v, e)| (None, Some(Interval::new(v, e, Direction::Left))))
        .parse_next(input)
}

fn left_bounded_interval(input: &str) -> IResult<&str, IntervalPair> {
    let (input, (left_edge, left_version)) = (left_edge, version).parse_next(input)?;
    let (input, (right_version, right_edge)) = if left_edge == Edge::Closed {
        let (input, c) = alt((delimiter, right_closed_edge_char)).parse_next(input)?;
        if c == ']' {
            (input, (Some(left_version.clone()), Edge::Closed))
        } else {
            (opt(version), right_edge).parse_next(input)?
        }
    } else {
        let (input, _) = delimiter.parse_next(input)?;
        alt((
            right_open_edge.map(|_| (None, Edge::Open)),
            (version, right_edge).map(|(v, e)| (Some(v), e)),
        ))
        .parse_next(input)?
    };

    let left_interval = Some(Interval::new(left_version, left_edge, Direction::Right));
    let right_interval = right_version
        .map(|right_version| Interval::new(right_version, right_edge, Direction::Left));

    Ok((input, (left_interval, right_interval)))
}

fn interval(input: &str) -> IResult<&str, IntervalPair> {
    alt((left_unbounded_interlval, left_bounded_interval)).parse_next(input)
}

fn range(input: &str) -> IResult<&str, Vec<IntervalPair>> {
    separated1(interval, delimiter).parse_next(input)
}
