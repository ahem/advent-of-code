use std::{cmp::Ordering, collections::HashMap};

use anyhow::anyhow;
use intcode::IntCode;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

impl TryFrom<i64> for Tile {
    type Error = anyhow::Error;
    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Tile::Empty),
            1 => Ok(Tile::Wall),
            2 => Ok(Tile::Block),
            3 => Ok(Tile::Paddle),
            4 => Ok(Tile::Ball),
            other => Err(anyhow!("invalid tile id {other}")),
        }
    }
}

pub fn main() -> anyhow::Result<()> {
    let program = read_input()?;
    let (_input, output, thread) = IntCode::new(&program).run_async()?;

    let mut screen: HashMap<(i64, i64), Tile> = HashMap::new();

    loop {
        if let Ok(x) = output.recv() {
            let y = output.recv()?;
            let tile = Tile::try_from(output.recv()?)?;
            screen.insert((x, y), tile);
        } else {
            break;
        }
    }

    thread.join().map_err(|err| anyhow!("{err:?}"))??;

    println!(
        "Part 1: {}",
        screen.iter().filter(|(_pos, x)| **x == Tile::Block).count()
    );

    let mut int_code = IntCode::new(&program);
    int_code.set_mem(0, 2);

    let (input, output, thread) = int_code.run_async()?;
    let mut screen: HashMap<(i64, i64), Tile> = HashMap::new();
    let mut score = None;
    input.send(0)?;

    loop {
        let x = match output.recv() {
            Ok(x) => x,
            Err(_) => break,
        };
        let y = output.recv()?;
        if x == -1 && y == 0 {
            score = Some(output.recv()?);
            continue;
        }

        let tile = Tile::try_from(output.recv()?)?;
        screen.insert((x, y), tile);

        let ball = screen.iter().find_map(|(pos, tile)| match tile {
            Tile::Ball => Some(pos),
            _ => None,
        });
        let paddle = screen.iter().find_map(|(pos, tile)| match tile {
            Tile::Paddle => Some(pos),
            _ => None,
        });

        if score.is_some() && matches!(tile, Tile::Ball) {
            let action = match (ball, paddle) {
                (Some(ball), Some(paddle)) => match ball.0.cmp(&paddle.0) {
                    Ordering::Less => -1,
                    Ordering::Equal => 0,
                    Ordering::Greater => 1,
                },
                _ => 0,
            };
            input.send(action)?;
            // println!("score {score:?}!! ball:{ball:?} paddle:{paddle:?}");
        }
    }

    thread.join().map_err(|err| anyhow!("{err:?}"))??;

    println!("Part 2: {}", score.unwrap());
    Ok(())
}
