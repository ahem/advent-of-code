use std::slice::ChunksExact;

use anyhow::anyhow;

fn layers(data: &[u8], width: usize, height: usize) -> ChunksExact<'_, u8> {
    data.chunks_exact(width * height)
}

fn count_digit(data: &[u8], digit: u8) -> usize {
    data.iter().filter(|x| **x == digit).count()
}

fn pixel(data: &[u8], width: usize, height: usize, x: usize, y: usize) -> u8 {
    layers(&data, width, height)
        .map(move |layer| layer[y * width + x])
        .rev()
        .reduce(|acc, x| match x {
            1 => 1,
            0 => 0,
            _ => acc,
        })
        .expect("layers aren't empty")
}

pub fn main() -> anyhow::Result<()> {
    let input = std::io::stdin()
        .lines()
        .next()
        .ok_or(anyhow!("invalid input"))??;
    let data = input
        .chars()
        .map(|c| u8::from_str_radix(&c.to_string(), 10))
        .collect::<Result<Vec<u8>, _>>()?;

    let width = 25;
    let height = 6;

    let layer = layers(&data, width, height)
        .min_by_key(|layer| count_digit(layer, 0))
        .ok_or(anyhow!("invalid data"))?;

    let part_1_result = count_digit(layer, 1) * count_digit(layer, 2);

    println!("Part 1: {part_1_result}");

    for y in 0..height {
        for x in 0..width {
            match pixel(&data, width, height, x, y) {
                0 => print!(" "),
                1 => print!("*"),
                _ => return Err(anyhow!("invalid pixel value")),
            }
        }
        print!("\n");
    }
    Ok(())
}
