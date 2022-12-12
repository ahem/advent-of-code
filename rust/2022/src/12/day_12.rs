use std::collections::HashMap;
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
        a_star::find_path(
            &self.start,
            &self.end,
            |p: &Point| p.manhattan_dist(&self.end),
            |p: &Point| self.accessible_neibours(p),
        )
    }

    fn find_shortest_path_to_end(&self) -> Option<Vec<Point>> {
        self.map
            .keys()
            .filter(|p| self.map[p] == u32::from('a'))
            .filter_map(|p| {
                a_star::find_path(
                    p,
                    &self.end,
                    |p: &Point| p.manhattan_dist(&self.end),
                    |p: &Point| self.accessible_neibours(p),
                )
            })
            .min_by_key(|p| p.len())
    }
}

mod a_star {
    use std::collections::{HashMap, HashSet};

    pub fn find_path<P, FH, FS, S>(start: &P, end: &P, h: FH, successors: FS) -> Option<Vec<P>>
    where
        P: std::hash::Hash + std::cmp::Eq + Clone,
        FH: Fn(&P) -> u32,
        FS: Fn(&P) -> S,
        S: IntoIterator<Item = P>,
    {
        // keep track of visited fields (not part of final path)
        let mut closed: HashSet<P> = HashSet::new();

        // keep track of potential fields in path. Value is accumulated score
        let mut open: HashMap<P, u32> = HashMap::from([(start.clone(), 0)]);

        // keep track of found path
        let mut parent_fields: HashMap<P, P> = HashMap::new();

        loop {
            let p = open
                .iter()
                .min_by_key(|(p, score)| **score + h(p))?
                .0
                .clone();

            if p == *end {
                return Some(traceback(p, parent_fields));
            }

            let score = open.remove(&p).unwrap();
            closed.insert(p.clone());

            for q in successors(&p) {
                if closed.contains(&q) {
                    continue;
                }
                if !open.contains_key(&q) || (open.contains_key(&q) && open[&q] > score + 1) {
                    open.insert(q.clone(), score + 1);
                    parent_fields.insert(q, p.clone());
                }
            }
        }
    }

    fn traceback<P>(p: P, parent_fields: HashMap<P, P>) -> Vec<P>
    where
        P: std::hash::Hash + std::cmp::Eq + Clone,
    {
        let mut path = vec![p.clone()];
        loop {
            match parent_fields.get(path.last().unwrap()) {
                Some(p) => path.push(p.clone()),
                None => break,
            }
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
    println!("Part 1: {:?}", path.unwrap().len() - 1);

    let t = std::time::Instant::now();
    let path = grid.find_shortest_path_to_end();
    println!(
        "Part 2: {:?}  (took: {:?})",
        path.unwrap().len() - 1,
        t.elapsed()
    );
}
