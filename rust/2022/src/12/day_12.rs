use std::collections::{HashMap, HashSet};
use std::str::FromStr;

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn manhattan_dist(&self, other: &Point) -> u32 {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
    }
}

#[derive(Debug)]
struct ParseError;

#[derive(Debug)]
struct Grid {
    start: Point,
    end: Point,
    map: HashMap<Point, u32>,
}

impl FromStr for Grid {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut map = HashMap::new();
        let mut start = Point { x: 0, y: 0 };
        let mut end = Point { x: 0, y: 0 };

        for (y, line) in s.lines().enumerate() {
            for (x, c) in line.chars().enumerate() {
                let p = Point {
                    x: x as i32,
                    y: y as i32,
                };
                if c == 'S' {
                    start = p.clone();
                    map.insert(p, 'a'.into());
                } else if c == 'E' {
                    end = p.clone();
                    map.insert(p, 'z'.into());
                } else if c.is_ascii() {
                    map.insert(p, c.into());
                } else {
                    return Err(ParseError);
                }
            }
        }

        return Ok(Grid { start, end, map });
    }
}

impl Grid {
    fn accessible_neibours(&self, p: &Point) -> Vec<Point> {
        let mut lst = vec![];
        if let Some(current_height) = self.map.get(p) {
            let candidates = [
                Point { x: p.x - 1, y: p.y },
                Point { x: p.x + 1, y: p.y },
                Point { x: p.x, y: p.y - 1 },
                Point { x: p.x, y: p.y + 1 },
            ];
            for q in candidates {
                if self
                    .map
                    .get(&q)
                    .filter(|x| **x <= *current_height + 1)
                    .is_some()
                {
                    lst.push(q);
                }
            }
        }
        return lst;
    }

    fn find_path(&self) -> Option<Vec<Point>> {
        // keep track of visited fields (not part of final path)
        let mut closed: HashSet<Point> = HashSet::new();

        // keep track of potential fields in path. Value is accumulated score
        let mut open: HashMap<Point, u32> = HashMap::from([(self.start, 0)]);

        // keep track of found path
        let mut parent_fields: HashMap<Point, Point> = HashMap::new();

        // guess distance to goal
        let h = |p: &Point| p.manhattan_dist(&self.end);

        loop {
            if let Some((p, score)) = open
                .iter()
                .min_by(|(p_a, score_a), (p_b, score_b)| {
                    let a = *score_a + h(p_a);
                    let b = *score_b + h(p_b);
                    a.cmp(&b)
                })
                .map(|(p, score)| (p.clone(), score.clone()))
            {
                open.remove(&p);
                closed.insert(p);

                if p == self.end {
                    return Some(self.trace_back(parent_fields));
                }

                for q in self.accessible_neibours(&p) {
                    if closed.contains(&q) {
                        continue;
                    }
                    if !open.contains_key(&q) || (open.contains_key(&q) && open[&q] > score + 1) {
                        println!("      inserting {q:?}...");
                        open.insert(q, score + 1);
                        parent_fields.insert(q, p);
                    }
                }
            } else {
                return None;
            }
        }
    }

    fn trace_back(&self, parent_fields: HashMap<Point, Point>) -> Vec<Point> {
        let mut p = self.end;
        let mut path = vec![p];
        loop {
            p = parent_fields[&p];
            if p == self.start {
                break;
            }
            path.push(p);
        }
        path.reverse();
        return path;
    }
}

fn read_input() -> Result<String, std::io::Error> {
    use std::io::Read;
    let mut s = String::new();
    std::io::stdin().lock().read_to_string(&mut s)?;
    return Ok(s);
}

pub fn main() {
    let s = read_input().unwrap();
    let grid = s.parse::<Grid>().unwrap();

    let path = grid.find_path();
    println!("{:?}", path.unwrap().len());
}
