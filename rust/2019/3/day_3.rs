use std::{collections::HashMap, str::FromStr};

use anyhow::anyhow;

#[derive(Debug, Clone, Copy)]
enum Step {
    Up(u32),
    Right(u32),
    Down(u32),
    Left(u32),
}

impl FromStr for Step {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (prefix, n) = s.split_at(1);
        let n = u32::from_str_radix(n, 10)?;
        match prefix {
            "U" => Ok(Self::Up(n)),
            "R" => Ok(Self::Right(n)),
            "D" => Ok(Self::Down(n)),
            "L" => Ok(Self::Left(n)),
            _ => Err(anyhow!("invalid input")),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Point {
    pub x: i64,
    pub y: i64,
}

#[derive(Debug)]
struct Wire(Vec<Step>);

impl Wire {
    pub fn step(&self, index: usize) -> Option<&Step> {
        self.0.get(index)
    }

    pub fn iter(&self) -> WireIterator<'_> {
        WireIterator {
            wire: self,
            idx: 0,
            cnt: 0,
            pos: Point { x: 0, y: 0 },
        }
    }
}

impl FromStr for Wire {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let steps: Result<Vec<Step>, _> = s.split(",").map(|s| s.parse::<Step>()).collect();
        Ok(Wire(steps?))
    }
}

struct WireIterator<'a> {
    wire: &'a Wire,
    idx: usize,
    cnt: u32,
    pos: Point,
}

impl<'a> Iterator for WireIterator<'a> {
    type Item = Point;
    fn next(&mut self) -> Option<Self::Item> {
        let (n, dx, dy) = match self.wire.step(self.idx)? {
            Step::Up(n) => (n, 0, 1),
            Step::Right(n) => (n, 1, 0),
            Step::Down(n) => (n, 0, -1),
            Step::Left(n) => (n, -1, 0),
        };
        self.pos.x += dx;
        self.pos.y += dy;
        self.cnt += 1;
        if self.cnt >= *n {
            self.idx += 1;
            self.cnt = 0;
        }
        Some(self.pos)
    }
}

fn read_input() -> anyhow::Result<Vec<Wire>> {
    std::io::stdin()
        .lines()
        .map(|line| Wire::from_str(&line?))
        .collect()
}

pub fn main() -> anyhow::Result<()> {
    let wires = read_input()?;

    let wire_1: HashMap<Point, usize> = wires[0]
        .iter()
        .enumerate()
        .map(|(idx, p)| (p, idx))
        .collect();

    let part_1_result = wires[1]
        .iter()
        .filter(|p| wire_1.contains_key(p))
        .map(|p| p.x.abs() + p.y.abs())
        .min();

    println!("Part 1: {part_1_result:?}");

    let part_2_result = wires[1]
        .iter()
        .enumerate()
        .filter_map(|(idx, p)| Some(idx + wire_1.get(&p)? + 2)) // +2 because of 0 indexing
        .min();

    println!("Part 2: {part_2_result:?}");

    Ok(())
}
