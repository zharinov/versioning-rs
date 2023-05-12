#[derive(Debug, PartialEq, Eq)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Edge {
    Hole,
    Point,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Interval<V>
where
    V: Ord,
{
    position: V,
    direction: Direction,
    edge: Edge,
}

impl<V> Interval<V>
where
    V: Ord,
{
    pub fn new(position: V, direction: Direction, edge: Edge) -> Self {
        Self {
            position,
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

        if x.position < y.position {
            return match direction {
                Right => Some(x),
                Left => Some(y),
            };
        }

        if y.position < x.position {
            return match direction {
                Right => Some(y),
                Left => Some(x),
            };
        }

        let edge_rank = |edge: &Edge| match edge {
            Hole => 0,
            Point => 1,
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
        let x = Interval::new(0, Direction::Right, Edge::Hole);
        let y = Interval::new(0, Direction::Right, Edge::Point);

        let res = Interval::dedupe_union(&x, &y);

        assert_eq!(res, Some(&y));
    }

    #[test]
    fn dedupe_union2() {
        let x = Interval::new(0, Direction::Left, Edge::Point);
        let y = Interval::new(0, Direction::Left, Edge::Hole);

        let res = Interval::dedupe_union(&x, &y);

        assert_eq!(res, Some(&x));
    }
}
