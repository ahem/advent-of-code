use anyhow::anyhow;
use itertools::Itertools;
use std::{
    collections::HashSet,
    io::{stdin, Read},
    str::FromStr,
};

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
struct Pos {
    x: i32,
    y: i32,
}

impl Pos {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    pub fn walk(&self, dir: Direction) -> Self {
        match dir {
            Direction::Up => Self {
                x: self.x,
                y: self.y - 1,
            },
            Direction::Right => Self {
                x: self.x + 1,
                y: self.y,
            },
            Direction::Down => Self {
                x: self.x,
                y: self.y + 1,
            },
            Direction::Left => Self {
                x: self.x - 1,
                y: self.y,
            },
        }
    }
}

#[derive(Debug, Clone)]
struct Area {
    obstacles: HashSet<Pos>,
    guard_position: Pos,
    width: i32,
    height: i32,
}

impl FromStr for Area {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let iter = s.split_whitespace().enumerate().flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .map(move |(x, c)| (Pos::new(x as _, y as _), c))
        });

        let guard_position = iter
            .clone()
            .find(|(_, c)| *c == '^')
            .ok_or(anyhow!("parse error"))?
            .0;

        let obstacles: HashSet<Pos> = iter
            .clone()
            .filter_map(|(pos, c)| match c {
                '#' => Some(pos),
                _ => None,
            })
            .collect();

        let (max_pos, _) = iter.max().ok_or(anyhow!("parse error"))?;

        Ok(Self {
            obstacles,
            guard_position,
            width: max_pos.x + 1,
            height: max_pos.y + 1,
        })
    }
}

impl Area {
    fn guard_walk(&self) -> impl Iterator<Item = (Pos, Direction)> + '_ {
        let mut pos = self.guard_position;
        let mut dir = Direction::Up;
        std::iter::once((self.guard_position, dir.clone())).chain(std::iter::from_fn(move || {
            while self.obstacles.contains(&pos.walk(dir)) {
                dir = dir.turn_right();
            }
            pos = pos.walk(dir);
            if pos.x < self.width && pos.y < self.height && pos.x >= 0 && pos.y >= 0 {
                Some((pos, dir))
            } else {
                None
            }
        }))
    }

    fn guard_walk_will_loop(&self) -> bool {
        let mut set = HashSet::new();
        self.guard_walk().any(|(pos, dir)| !set.insert((pos, dir)))
    }

    fn empty_positions(&self) -> impl Iterator<Item = Pos> + '_ {
        (0..self.width)
            .flat_map(|x| (0..self.height).map(move |y| Pos { x, y }))
            .filter(|pos| *pos != self.guard_position && !self.obstacles.contains(&pos))
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn turn_right(&self) -> Self {
        match self {
            Self::Up => Self::Right,
            Self::Right => Self::Down,
            Self::Down => Self::Left,
            Self::Left => Self::Up,
        }
    }
}

pub fn main() -> anyhow::Result<()> {
    let area = {
        let mut s = String::new();
        stdin().read_to_string(&mut s)?;
        Area::from_str(&s)?
    };
    let part_1 = area
        .guard_walk()
        .map(|(pos, _)| pos)
        .collect::<HashSet<_>>()
        .len();
    println!("part 1: {part_1}");

    let part_2 = area
        .empty_positions()
        .map(|pos| {
            let mut a = area.clone();
            a.obstacles.insert(pos);
            a
        })
        .filter(|a| a.guard_walk_will_loop())
        .count();

    println!("part 2: {part_2}");

    Ok(())
}
