use anyhow::anyhow;
use itertools::Itertools;
use std::{io::stdin, str::FromStr};

#[derive(Debug)]
struct ParsedLine(u32, u32);

impl FromStr for ParsedLine {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        let v = s
            .split_whitespace()
            .map(str::parse)
            .collect::<Result<Vec<u32>, _>>()?;
        if v.len() == 2 {
            Ok(ParsedLine(v[0], v[1]))
        } else {
            Err(anyhow!("too few values"))
        }
    }
}

fn main() -> anyhow::Result<()> {
    let input = stdin()
        .lines()
        .map(|s| s.unwrap().parse())
        .collect::<anyhow::Result<Vec<ParsedLine>>>()?;

    let left = input.iter().map(|x| x.0).sorted().collect_vec();
    let right = input.iter().map(|x| x.1).sorted().collect_vec();

    let total_differance: u32 = left.iter().zip(&right).map(|(a, b)| a.abs_diff(*b)).sum();
    println!("Part 1: {total_differance:?}");

    let right_counts = right.iter().counts();

    let similarity_score: u32 = left
        .iter()
        .map(|x| x * (*right_counts.get(x).unwrap_or(&0) as u32))
        .sum();
    println!("Part 2: {similarity_score:?}");

    Ok(())
}
