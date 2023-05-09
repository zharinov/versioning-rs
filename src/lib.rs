#[derive(Debug, PartialEq, Eq)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Edge {
    Hole,
    Point,
    Neighborhood(Direction),
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

        return match (&x.edge, &y.edge) {
            (Hole, Hole) => Some(x),
            (Point, Point) => Some(x),

            (Hole, Point) => Some(y),
            (Point, Hole) => Some(x),

            (Hole | Point, Neighborhood(dy)) => {
                if dy == direction {
                    Some(x)
                } else {
                    Some(y)
                }
            }
            (Neighborhood(dx), Hole | Point) => {
                if dx == direction {
                    Some(y)
                } else {
                    Some(x)
                }
            }

            (Neighborhood(dx), Neighborhood(dy)) => {
                if dx == dy {
                    return Some(x);
                }

                if dy == direction {
                    Some(x)
                } else {
                    Some(y)
                }
            }
        };
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
}
