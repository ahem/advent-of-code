use anyhow::anyhow;
use itertools::*;
use std::{
    collections::HashSet,
    io::{stdin, Read},
    ops::{Add, Mul, Sub},
    str::FromStr,
};

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
struct Pos {
    x: i32,
    y: i32,
}

impl Add for Pos {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Pos {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Sub for Pos {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Pos {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl Mul<i32> for Pos {
    type Output = Self;
    fn mul(self, rhs: i32) -> Self::Output {
        Pos {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

#[derive(Debug)]
struct Antennas {
    groups: Vec<AntennaGroup>,
    width: i32,
    height: i32,
}

impl Antennas {
    pub fn antinodes(&self) -> impl Iterator<Item = Pos> + '_ {
        self.groups
            .iter()
            .flat_map(|group| group.antinodes(2))
            .filter(|pos| pos.x >= 0 && pos.x < self.width && pos.y >= 0 && pos.y < self.height)
    }

    pub fn antinodes_with_resonant_harmonics(&self) -> impl Iterator<Item = Pos> + '_ {
        let n = i32::max(self.width, self.height);
        (0..n)
            .flat_map(|n| self.groups.iter().flat_map(move |group| group.antinodes(n)))
            .filter(|pos| pos.x >= 0 && pos.x < self.width && pos.y >= 0 && pos.y < self.height)
    }
}

#[derive(Debug)]
struct AntennaGroup {
    id: char,
    positions: HashSet<Pos>,
}

impl AntennaGroup {
    pub fn antinodes(&self, n: i32) -> impl Iterator<Item = Pos> + '_ {
        self.positions.iter().permutations(2).flat_map(move |v| {
            let p1 = *v[0] + (*v[1] - *v[0]) * n;
            let p2 = *v[1] + (*v[0] - *v[1]) * n;
            // println!("   {n}: ({:?}, {:?}) -> [{p1:?}, {p2:?}", *v[0], *v[1]);
            std::iter::once(p1).chain(std::iter::once(p2))
        })
    }
}

impl FromStr for Antennas {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let groups = s
            .lines()
            .enumerate()
            .flat_map(|(y, s)| {
                s.chars().enumerate().filter_map(move |(x, c)| match c {
                    '.' => None,
                    c => Some((
                        c,
                        Pos {
                            x: x as _,
                            y: y as _,
                        },
                    )),
                })
            })
            .sorted()
            .fold(Vec::new(), |mut acc: Vec<AntennaGroup>, (c, pos)| {
                match acc.last_mut() {
                    Some(last) if last.id == c => {
                        last.positions.insert(pos);
                    }
                    _ => acc.push(AntennaGroup {
                        id: c,
                        positions: HashSet::from_iter(std::iter::once(pos)),
                    }),
                };
                acc
            });
        let height = s.lines().count() as i32;
        let width = s.lines().next().ok_or(anyhow!("parse error"))?.len() as i32;
        Ok(Self {
            groups,
            width,
            height,
        })
    }
}

pub fn main() -> anyhow::Result<()> {
    let antennas = {
        let mut s = String::new();
        stdin().read_to_string(&mut s)?;
        s.parse::<Antennas>()?
    };
    let antinodes = antennas.antinodes().collect::<HashSet<_>>();
    println!("Part 1: {}", antinodes.len());

    let antinodes = antennas
        .antinodes_with_resonant_harmonics()
        .collect::<HashSet<_>>();

    // println!("{antinodes:#?}");

    println!("Part 2: {}", antinodes.len());
    Ok(())
}
