use std::{
    collections::HashSet,
    io::{Read, stdin},
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

#[derive(Clone)]
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

    pub fn shoot(&mut self, src: &Pos, angle: f64) -> Option<(f64, Pos)> {
        let angle = if angle > 360.0 { angle - 360.0 } else { angle };

        let target = self
            .detected(*src)
            .map(|p| {
                let dx = (p.x - src.x) as f64;
                let dy = (p.y - src.y) as f64;
                let mut angle = f64::atan(dy / dx).to_degrees();
                if dx < 0.0 {
                    angle += 180.0;
                } else if dy < 0.0 {
                    angle += 360.0;
                }

                (angle, p)
            })
            .filter(|(a, _)| *a >= angle)
            .min_by_key(|(a, _)| (a * 100000.0) as i64)
            .map(|(a, p)| (a, p.to_owned()));

        match target {
            Some((angle, pos)) => {
                self.grid.remove(&pos);
                Some((angle, pos))
            }
            None => None,
        }
    }

    pub fn count(&self) -> usize {
        self.grid.len()
    }
}

fn main() -> anyhow::Result<()> {
    let astroid_field = {
        let mut s = String::new();
        stdin().read_to_string(&mut s)?;
        AstroidField::from_str(&s)?
    };

    let (visible_cnt, best_pos) = astroid_field.find_best();

    println!("Part 1: {visible_cnt} ({best_pos:?}");

    let mut astroid_field = astroid_field.clone();

    let mut idx = 0;
    let mut laser_angle = 270.0; // straight up
    while astroid_field.count() > 1 {
        if let Some((target_angle, target)) = astroid_field.shoot(&best_pos, laser_angle) {
            idx += 1;
            if idx == 200 {
                println!("Part 2: {}", target.x * 100 + target.y);
            }
            // println!("{idx}: {target:?} ({target_angle})");
            laser_angle = target_angle + 0.0001;
        } else {
            laser_angle = 0.0;
        }
    }

    Ok(())
}
