mod point {

    #[derive(Debug, PartialEq, Eq, Hash, Clone)]
    pub struct Point {
        pub x: i32,
        pub y: i32,
    }

    impl TryFrom<(usize, usize)> for Point {
        type Error = std::num::TryFromIntError;
        fn try_from(value: (usize, usize)) -> Result<Self, Self::Error> {
            Ok(Self {
                x: i32::try_from(value.0)?,
                y: i32::try_from(value.1)?,
            })
        }
    }
}

mod grid {
    use super::point::Point;
    use std::collections::{hash_map::Keys, HashMap};

    #[derive(Debug)]
    pub struct Grid {
        cells: HashMap<Point, u32>,
    }

    #[derive(Debug)]
    pub enum ParseError {
        IndexError(std::num::TryFromIntError),
        ValueError,
    }

    #[derive(Clone, Debug)]
    pub enum Direction {
        North,
        East,
        South,
        West,
    }

    impl Grid {
        pub fn get(&self, p: &Point) -> Option<&u32> {
            self.cells.get(p)
        }

        pub fn keys(&self) -> Keys<Point, u32> {
            self.cells.keys()
        }

        pub fn is_visible(&self, p: &Point) -> bool {
            let height = self.get(p).unwrap_or(&0);
            let is_lower = |n| n < height;

            self.walk(Direction::North, p).all(is_lower)
                || self.walk(Direction::East, p).all(is_lower)
                || self.walk(Direction::South, p).all(is_lower)
                || self.walk(Direction::West, p).all(is_lower)
        }

        pub fn viewing_distance(&self, p: &Point, dir: Direction) -> usize {
            let current_height = self.get(p).unwrap_or(&0);
            let mut distance = 0;
            for tree in self.walk(dir, p) {
                distance += 1;
                if tree >= current_height {
                    break;
                }
            }
            distance
        }

        pub fn scenic_score(&self, p: &Point) -> usize {
            self.viewing_distance(p, Direction::North)
                * self.viewing_distance(p, Direction::East)
                * self.viewing_distance(p, Direction::South)
                * self.viewing_distance(p, Direction::West)
        }

        pub fn parse<T>(lines: T) -> Result<Self, ParseError>
        where
            T: IntoIterator<Item = String>,
        {
            let mut cells = HashMap::new();
            for (y, line) in lines.into_iter().enumerate() {
                for (x, c) in line.chars().enumerate() {
                    let p = Point::try_from((x, y)).map_err(ParseError::IndexError)?;
                    let n = c.to_digit(10).ok_or(ParseError::ValueError)?;
                    cells.insert(p, n);
                }
            }
            Ok(Self { cells })
        }

        fn walk(&self, dir: Direction, from: &Point) -> GridIter {
            GridIter {
                grid: &self,
                p: from.clone(),
                dir,
            }
        }
    }

    pub struct GridIter<'a> {
        grid: &'a Grid,
        p: Point,
        dir: Direction,
    }

    impl<'a> Iterator for GridIter<'a> {
        type Item = &'a u32;

        fn next(&mut self) -> Option<Self::Item> {
            let (dx, dy) = match self.dir {
                Direction::North => (0, -1),
                Direction::East => (1, 0),
                Direction::South => (0, 1),
                Direction::West => (-1, 0),
            };
            self.p = Point {
                x: self.p.x + dx,
                y: self.p.y + dy,
            };
            self.grid.get(&self.p)
        }
    }
}

fn main() {
    let lines = std::io::stdin()
        .lines()
        .collect::<Result<Vec<String>, _>>()
        .unwrap();
    let grid = grid::Grid::parse(lines).unwrap();
    let visible_trees = grid.keys().filter(|p| grid.is_visible(*p)).count();
    println!("part 1: {}", visible_trees);

    let highest_scenic_score = grid.keys().map(|p| grid.scenic_score(p)).max().unwrap();
    println!("part 2: {} ", highest_scenic_score);
}
