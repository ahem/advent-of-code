use anyhow::anyhow;
use intcode::IntCode;
use std::collections::HashMap;

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct Pos {
    x: i64,
    y: i64,
}

impl Pos {
    pub fn step(&self, direction: Direction) -> Self {
        match direction {
            Direction::Up => Self {
                x: self.x,
                y: self.y + 1,
            },
            Direction::Right => Self {
                x: self.x + 1,
                y: self.y,
            },
            Direction::Down => Self {
                x: self.x,
                y: self.y - 1,
            },
            Direction::Left => Self {
                x: self.x - 1,
                y: self.y,
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    pub fn turn_left(&self) -> Self {
        match self {
            Direction::Up => Direction::Left,
            Direction::Left => Direction::Down,
            Direction::Down => Direction::Right,
            Direction::Right => Direction::Up,
        }
    }
    pub fn turn_right(&self) -> Self {
        match self {
            Direction::Up => Direction::Right,
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
        }
    }
}

fn read_input() -> anyhow::Result<Vec<i64>> {
    std::io::stdin()
        .lines()
        .next()
        .ok_or(anyhow::Error::msg("missing input"))??
        .split(',')
        .map(|s| i64::from_str_radix(s, 10))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| anyhow::Error::from(err))
}

struct Robot {
    program: Vec<i64>,
}

impl Robot {
    pub fn new(program: &[i64]) -> Self {
        Self {
            program: program.to_vec(),
        }
    }

    pub fn paint(&self, panels: &mut HashMap<Pos, i64>) -> anyhow::Result<()> {
        let mut pos = Pos { x: 0, y: 0 };
        let mut direction = Direction::Up;
        let (input, output, thread) = IntCode::new(&self.program).run_async()?;
        input.send(*panels.get(&pos).unwrap_or(&0))?;

        while let Ok(color) = output.recv() {
            panels.insert(pos, color);
            match output.recv() {
                Ok(0) => direction = direction.turn_left(),
                Ok(1) => direction = direction.turn_right(),
                _ => return Err(anyhow!("program error")),
            }
            pos = pos.step(direction);
            let _ = input.send(*panels.get(&pos).unwrap_or(&0));
        }

        thread.join().map_err(|err| anyhow!("{err:?}"))?
    }
}

pub fn main() -> anyhow::Result<()> {
    let robot = Robot::new(&read_input()?);
    let mut panels: HashMap<Pos, i64> = Default::default();
    robot.paint(&mut panels)?;

    println!("part 1: {}", panels.len());

    let mut panels: HashMap<Pos, i64> = Default::default();
    panels.insert(Pos { x: 0, y: 0 }, 1);
    robot.paint(&mut panels)?;

    let width_min = panels.keys().map(|p| p.x).min().ok_or(anyhow!("bad"))?;
    let width_max = panels.keys().map(|p| p.x).max().ok_or(anyhow!("bad"))?;
    let height_min = panels.keys().map(|p| p.y).min().ok_or(anyhow!("bad"))?;
    let height_max = panels.keys().map(|p| p.y).max().ok_or(anyhow!("bad"))?;
    for y in (height_min..=height_max).rev() {
        for x in width_min..=width_max {
            match panels.get(&Pos { x, y }) {
                Some(1) => print!("#"),
                _ => print!(" "),
            };
        }
        print!("\n");
    }

    Ok(())
}
