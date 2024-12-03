use itertools::Itertools;
use std::{io::stdin, ops::Deref, str::FromStr};

#[derive(Debug)]
struct ParsedLine(Vec<u32>);

impl FromStr for ParsedLine {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        let v = s
            .split_whitespace()
            .map(str::parse)
            .collect::<Result<Vec<u32>, _>>()?;
        Ok(ParsedLine(v))
    }
}

impl Deref for ParsedLine {
    type Target = Vec<u32>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

trait SkipNth<T>: Iterator<Item = T> + Sized {
    fn skip_nth(self, n: usize) -> impl Iterator<Item = T> {
        self.enumerate()
            .filter_map(move |(idx, b)| if idx == n { None } else { Some(b) })
    }
}

impl<T, IT> SkipNth<T> for IT where IT: Iterator<Item = T> + Sized {}

trait Dampen<T>: Iterator<Item = T> + Sized + Clone {
    fn dampen(self) -> impl Iterator<Item = impl Iterator<Item = T>> {
        (0..self.clone().count()).map(move |idx| self.clone().skip_nth(idx))
    }
}

impl<T, IT> Dampen<T> for IT where IT: Iterator<Item = T> + Sized + Clone {}

fn is_safe<'a, T: Iterator<Item = &'a u32> + Clone>(report: T) -> bool {
    let ascending = |(a, b)| b > a && b - a < 4;
    let descending = |(a, b)| b < a && a - b < 4;
    let mut iter = report.tuple_windows();
    iter.clone().all(ascending) || iter.all(descending)
}

fn main() -> anyhow::Result<()> {
    let input = stdin()
        .lines()
        .map(|s| s.unwrap().parse())
        .collect::<anyhow::Result<Vec<ParsedLine>>>()?;

    let part_1 = input.iter().filter(|report| is_safe(report.iter())).count();
    println!("Part 1: {part_1:?}",);

    let part_2 = input
        .iter()
        .filter(|report| {
            report.iter().dampen().any(|report| {
                let report = report.collect_vec();
                is_safe(report.iter().copied())
            })
        })
        .count();

    println!("Part 2: {part_2:?}",);

    Ok(())
}
