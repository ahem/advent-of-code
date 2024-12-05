use itertools::Itertools;
use std::{
    collections::HashSet,
    io::{stdin, Read},
    str::FromStr,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
struct Pos {
    x: i32,
    y: i32,
}

impl Pos {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

struct AstroidField {
    grid: HashSet<Pos>,
}

impl FromStr for AstroidField {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let grid: HashSet<Pos> = s
            .split_whitespace()
            .enumerate()
            .flat_map(|(y, line)| {
                line.chars().enumerate().filter_map(move |(x, c)| match c {
                    '#' => Some(Pos::new(x as i32, y as i32)),
                    _ => None,
                })
            })
            .collect();
        Ok(Self { grid })
    }
}

impl AstroidField {
    fn steps(&self, src: Pos, dst: Pos) -> impl Iterator<Item = Pos> {
        let dx = dst.x - src.x;
        let dy = dst.y - src.y;
        let steps = i32::max(dx.abs(), dy.abs());

        (1..=steps).filter_map(move |n| {
            if dx * n % steps == 0 && dy * n % steps == 0 {
                Some(Pos::new(src.x + dx * n / steps, src.y + dy * n / steps))
            } else {
                None
            }
        })
    }

    fn is_obscured(&self, src: Pos, target: Pos) -> bool {
        self.steps(src, target)
            .any(|p| p != target && self.grid.contains(&p))
    }

    fn detected(&self, src: Pos) -> impl Iterator<Item = &Pos> {
        self.grid
            .iter()
            .filter(move |p| **p != src && !self.is_obscured(src, **p))
    }

    fn find_best(&self) -> (usize, Pos) {
        self.grid
            .iter()
            .map(|src| (self.detected(*src).count(), *src))
            .max()
            .unwrap()
    }
}

fn main() -> anyhow::Result<()> {
    let astroid_field = {
        let mut s = String::new();
        stdin().read_to_string(&mut s)?;
        AstroidField::from_str(&s)?
    };
    println!("Part 1: {:?}", astroid_field.find_best().0);

    Ok(())
}

/*
#.........
...A......
...B..a...
.EDCG....a
..F.c.b...
.....c....
..efd.c.gb
.......c..
....f...c.
...e..d..c
*/
