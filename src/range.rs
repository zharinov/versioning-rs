#![allow(dead_code)]

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Edge {
    Open,
    Closed,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Interval<V>
where
    V: Ord,
{
    version: V,
    edge: Edge,
    direction: Direction,
}

impl<V> Interval<V>
where
    V: Ord,
{
    pub fn new(position: V, edge: Edge, direction: Direction) -> Self {
        Self {
            version: position,
            direction,
            edge,
        }
    }

    pub fn dedupe_union<'a>(x: &'a Self, y: &'a Self) -> Option<&'a Self> {
        use Direction::*;
        use Edge::*;

        if x.direction != y.direction {
            return None;
        }

        let direction = &x.direction;

        if x.version < y.version {
            return match direction {
                Right => Some(x),
                Left => Some(y),
            };
        }

        if y.version < x.version {
            return match direction {
                Right => Some(y),
                Left => Some(x),
            };
        }

        let edge_rank = |edge: &Edge| match edge {
            Open => 0,
            Closed => 1,
        };

        if edge_rank(&x.edge) >= edge_rank(&y.edge) {
            Some(x)
        } else {
            Some(y)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dedupe_union() {
        let x = Interval::new(0, Edge::Open, Direction::Right);
        let y = Interval::new(0, Edge::Closed, Direction::Right);

        let res = Interval::dedupe_union(&x, &y);

        assert_eq!(res, Some(&y));
    }

    #[test]
    fn dedupe_union2() {
        let x = Interval::new(0, Edge::Closed, Direction::Left);
        let y = Interval::new(0, Edge::Open, Direction::Left);

        let res = Interval::dedupe_union(&x, &y);

        assert_eq!(res, Some(&x));
    }
}
