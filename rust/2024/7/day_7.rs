use anyhow::anyhow;
use itertools::Itertools;
use std::{io::stdin, str::FromStr};

#[derive(Debug)]
struct OperatorlessEquation {
    numbers: Vec<i64>,
    value: i64,
}

impl FromStr for OperatorlessEquation {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(':');
        let value = iter.next().ok_or(anyhow!("parse error"))?.parse()?;
        let numbers = iter
            .next()
            .ok_or(anyhow!("parse error"))?
            .split_whitespace()
            .map(|s| s.parse())
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self { value, numbers })
    }
}

#[derive(Debug, Clone, Copy)]
enum Operator {
    Add,
    Mul,
    Concat,
}

impl OperatorlessEquation {
    pub fn can_be_true<T, IT>(&self, valid_operators: T) -> bool
    where
        T: IntoIterator<Item = Operator, IntoIter = IT>,
        IT: Iterator<Item = Operator> + Clone,
    {
        let operator_possibilities =
            itertools::repeat_n(valid_operators.into_iter(), self.numbers.len() - 1)
                .multi_cartesian_product();

        operator_possibilities
            .map(|operators| {
                operators.iter().zip(self.numbers.iter().skip(1)).fold(
                    self.numbers[0],
                    |acc, (op, n)| match op {
                        Operator::Add => acc + (*n),
                        Operator::Mul => acc * (*n),
                        Operator::Concat => {
                            let digits_in_n = f64::log10(*n as f64).floor() as u32 + 1;
                            acc * i64::pow(10, digits_in_n) + n
                        }
                    },
                )
            })
            .any(|v| self.value == v)
    }
}

fn main() -> anyhow::Result<()> {
    let equations = stdin()
        .lines()
        .map(|s| s?.parse())
        .collect::<Result<Vec<OperatorlessEquation>, _>>()?;

    let part_1: i64 = equations
        .iter()
        .filter(|x| x.can_be_true([Operator::Add, Operator::Mul]))
        .map(|x| x.value)
        .sum();

    println!("part 1: {part_1}");

    let part_2: i64 = equations
        .iter()
        .filter(|x| x.can_be_true([Operator::Add, Operator::Mul, Operator::Concat]))
        .map(|x| x.value)
        .sum();

    println!("part 2: {part_2}");

    Ok(())
}
