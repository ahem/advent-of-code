use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Point {
    x: i32,
    y: i32,
}

mod parser {
    use super::Point;
    use nom::bytes::complete::tag;
    use nom::character::complete::digit1;
    use nom::combinator::{map, map_res};
    use nom::multi::separated_list1;
    use nom::sequence::separated_pair;
    use nom::IResult;

    fn int(input: &str) -> IResult<&str, i32> {
        map_res(digit1, |s: &str| s.parse())(input)
    }

    fn point(input: &str) -> IResult<&str, Point> {
        map(separated_pair(int, tag(","), int), |(x, y)| Point { x, y })(input)
    }

    pub fn point_list(input: &str) -> IResult<&str, Vec<Point>> {
        separated_list1(tag(" -> "), point)(input)
    }
}

fn unfold_lines<T>(input: T) -> Vec<Point>
where
    T: IntoIterator<Item = Point>,
{
    let mut iter = input.into_iter();
    let mut result = Vec::new();

    if let Some(start) = iter.next() {
        result.push(start.clone());
        let mut p = start.clone();
        while let Some(q) = iter.next() {
            while p != q {
                p.x += (q.x - p.x).signum();
                p.y += (q.y - p.y).signum();
                result.push(p.clone());
            }
            p = q.clone();
        }
    }

    return result;
}

#[derive(Debug, Clone, PartialEq)]
enum Field {
    Air,
    Sand,
    Rock,
}

struct Grid {
    cells: HashMap<Point, Field>,
    bottom: i32,
}

impl Grid {
    pub fn from_rock_positions<T>(rocks: T) -> Self
    where
        T: IntoIterator<Item = Point>,
    {
        let mut cells = HashMap::new();
        for rock in rocks {
            cells.insert(rock, Field::Rock);
        }
        let bottom = cells.keys().map(|p| p.y).max().unwrap_or(0);
        return Grid { cells, bottom };
    }

    pub fn get(&self, p: &Point) -> Field {
        if p.y > self.bottom + 1 {
            Field::Rock
        } else {
            self.cells.get(p).unwrap_or(&Field::Air).to_owned()
        }
    }

    pub fn drop_sand(&mut self, origin: &Point) -> Point {
        let p = self.flow(origin);
        self.cells.insert(p.clone(), Field::Sand);
        return p;
    }

    pub fn amount_of_sand(&self) -> usize {
        self.cells.values().filter(|x| **x == Field::Sand).count()
    }

    fn flow(&self, origin: &Point) -> Point {
        let mut p = origin.clone();

        loop {
            let y = p.y + 1;
            let candidates = [
                Point { x: p.x, y },
                Point { x: p.x - 1, y },
                Point { x: p.x + 1, y },
            ];
            match candidates.iter().find(|q| self.get(q) == Field::Air) {
                Some(next_p) => p = next_p.clone(),
                None => return p,
            }
        }
    }
}

fn main() {
    let input: Vec<Vec<Point>> = std::io::stdin()
        .lines()
        .filter_map(|s| Some(parser::point_list(&s.unwrap()).ok()?.1))
        .collect();

    let rocks: Vec<Point> = input.into_iter().flat_map(unfold_lines).collect();
    let mut grid = Grid::from_rock_positions(rocks);

    let origin = Point { x: 500, y: 0 };

    while grid.drop_sand(&origin).y < grid.bottom {
        continue;
    }
    println!("Part 1: {}", grid.amount_of_sand() - 1);

    while grid.drop_sand(&origin) != origin {
        continue;
    }
    println!("Part 2: {}", grid.amount_of_sand());
}
