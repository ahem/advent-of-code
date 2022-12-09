use std::collections::HashSet;

#[derive(Debug)]
enum Error {
    ParseError(String),
    IOError(std::io::Error),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

mod parser {
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{digit1, space1};
    use nom::combinator::{map, map_res};
    use nom::sequence::separated_pair;
    use nom::IResult;

    use super::Direction;

    pub fn parse(input: &str) -> IResult<&str, (Direction, usize)> {
        let direction = map(alt((tag("U"), tag("D"), tag("L"), tag("R"))), |s| match s {
            "U" => Direction::Up,
            "D" => Direction::Down,
            "L" => Direction::Left,
            "R" => Direction::Right,
            _ => unreachable!(),
        });
        let count = map_res(digit1, |d: &str| d.parse::<usize>());
        separated_pair(direction, space1, count)(input)
    }
}

fn parse_input() -> Result<Vec<(Direction, usize)>, Error> {
    std::io::stdin()
        .lines()
        .map(
            |s| match parser::parse(s.map_err(Error::IOError)?.as_str()) {
                Ok(("", result)) => Ok(result),
                Ok((rest, _)) => Err(Error::ParseError(format!("unparsed: {rest:?}"))),
                Err(e) => Err(Error::ParseError(e.to_string())),
            },
        )
        .collect::<Result<Vec<(Direction, usize)>, Error>>()
}

#[derive(Debug, Clone)]
struct RopeSection {
    x: i32,
    y: i32,
}

impl RopeSection {
    pub fn go(&mut self, direction: &Direction) {
        match direction {
            Direction::Up => self.y += 1,
            Direction::Down => self.y -= 1,
            Direction::Left => self.x -= 1,
            Direction::Right => self.x += 1,
        }
    }

    pub fn follow(&mut self, other: &RopeSection) {
        let dx = other.x - self.x;
        let dy = other.y - self.y;
        if dx.abs() <= 1 && dy.abs() <= 1 {
            return;
        }
        self.y += dy.signum();
        self.x += dx.signum();
    }
}

#[derive(Debug, Clone)]
struct Rope {
    sections: Vec<RopeSection>,
}

impl Rope {
    pub fn new(length: usize) -> Self {
        let sections = std::iter::repeat(RopeSection { x: 0, y: 0 })
            .take(length)
            .collect();
        Self { sections }
    }

    pub fn go(&mut self, direction: &Direction) {
        self.sections[0].go(direction);
        for n in 1..self.sections.len() {
            let prev = self.sections[n - 1].clone();
            self.sections[n].follow(&prev);
        }
    }

    pub fn tail(&self) -> (i32, i32) {
        let pos = self.sections.last().unwrap();
        (pos.x, pos.y)
    }

    pub fn track_tail(&mut self, directions: &Vec<Direction>) -> HashSet<(i32, i32)> {
        let mut visited_by_tail: HashSet<(i32, i32)> = HashSet::new();
        for dir in directions {
            self.go(&dir);
            visited_by_tail.insert(self.tail());
        }
        visited_by_tail
    }
}

fn main() {
    let input = parse_input().unwrap();
    let directions: Vec<Direction> = input
        .iter()
        .flat_map(|(dir, n)| std::iter::repeat(dir.clone()).take(*n))
        .collect();

    let visited_by_tail = Rope::new(2).track_tail(&directions);
    println!("part 1: {}", visited_by_tail.len());

    let visited_by_tail = Rope::new(10).track_tail(&directions);
    println!("part 2: {}", visited_by_tail.len());
}
