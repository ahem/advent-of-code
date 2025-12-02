use std::io::stdin;

#[derive(Debug, PartialEq)]
enum Rotation {
    Left(u32),
    Right(u32),
}

impl From<&str> for Rotation {
    fn from(value: &str) -> Self {
        let n = u32::from_str_radix(&value[1..], 10).unwrap();
        match value.chars().next() {
            Some('L') => Rotation::Left(n),
            Some('R') => Rotation::Right(n),
            _ => panic!("invalid data"),
        }
    }
}

fn part1(input: &[Rotation]) -> u32 {
    let mut acc = 50;
    let values: Vec<u32> = input
        .iter()
        .map(|rot| {
            acc = match rot {
                Rotation::Left(n) => (10000 + acc - n) % 100,
                Rotation::Right(n) => (acc + n) % 100,
            };
            acc
        })
        .collect();

    let zeroes = values.iter().filter(|x| **x == 0).count();
    zeroes as u32
}

fn part2(input: &[Rotation]) -> u32 {
    let mut acc: i32 = 50;
    let mut zeroes: u32 = 0;
    for rot in input {
        let (step, n) = match rot {
            Rotation::Left(n) => (-1, n),
            Rotation::Right(n) => (1, n),
        };
        for _ in 0..*n {
            acc += step;
            if acc > 99 {
                acc = 0;
            }
            if acc < 0 {
                acc = 99;
            }
            if acc == 0 {
                zeroes += 1;
            }
        }
        // println!("  {acc}");
    }
    zeroes
}

fn main() {
    let input: Vec<Rotation> = stdin()
        .lines()
        .map(|s| Rotation::from(s.unwrap().as_ref()))
        .collect();

    println!("part 1: {}", part1(&input));
    println!("part 2: {}", part2(&input));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parser() {
        assert_eq!(Rotation::from("L68"), Rotation::Left(68));
    }
}
