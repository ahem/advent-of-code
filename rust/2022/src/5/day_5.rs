use std::collections::LinkedList;
use std::io::Read;
use std::str::FromStr;

use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug)]
pub struct ParseError;

#[derive(Debug)]
pub struct MoveError;

type Stacks = Vec<LinkedList<char>>;

#[derive(Debug)]
struct Instruction {
    quantity: usize,
    from: usize,
    to: usize,
}

impl Instruction {
    pub fn apply(&self, stacks: &mut Stacks) -> Result<(), MoveError> {
        for _ in 0..self.quantity {
            let item = stacks
                .get_mut(self.from)
                .and_then(|stack| stack.pop_back())
                .ok_or(MoveError)?;
            stacks.get_mut(self.to).ok_or(MoveError)?.push_back(item);
        }
        Ok(())
    }

    pub fn apply2(&self, stacks: &mut Stacks) -> Result<(), MoveError> {
        let from_stack = stacks.get_mut(self.from).ok_or(MoveError)?;
        let mut items = from_stack.split_off(from_stack.len() - self.quantity);
        let to_stack = stacks.get_mut(self.to).ok_or(MoveError)?;
        to_stack.append(&mut items);
        Ok(())
    }
}

impl FromStr for Instruction {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^move (\d+) from (\d+) to (\d+)$").unwrap();
        }

        let captures = RE.captures(s).ok_or(ParseError)?;
        let captures = captures.iter().skip(1);
        let mut captures = captures.filter_map(|c| c?.as_str().parse::<usize>().ok());
        let (quantity, from, to) = captures.next_tuple().ok_or(ParseError)?;
        Ok(Instruction {
            quantity,
            from: from - 1,
            to: to - 1,
        })
    }
}

fn parse_stacks(s: &str) -> Result<Stacks, ParseError> {
    let mut stacks: Vec<LinkedList<char>> = vec![];
    let mut lines = s.lines().rev();

    for _ in 0..(lines.next().ok_or(ParseError)?.len() / 4 + 1) {
        stacks.push(LinkedList::new());
    }

    for line in lines {
        for (i, c) in line.chars().skip(1).step_by(4).enumerate() {
            if c != ' ' {
                stacks[i].push_back(c);
            }
        }
    }

    Ok(stacks)
}

fn main() {
    let mut input = String::new();
    std::io::stdin().lock().read_to_string(&mut input).unwrap();

    let mut parts = input.splitn(2, "\n\n");
    let initial_stacks = parse_stacks(parts.next().unwrap()).unwrap();

    let instructions: Result<Vec<Instruction>, ParseError> = parts
        .next()
        .unwrap()
        .lines()
        .map(|l| l.parse::<Instruction>())
        .collect();
    let instructions = instructions.unwrap();

    let mut stacks = initial_stacks.clone();
    for instruction in instructions.iter() {
        instruction.apply(&mut stacks).unwrap();
    }

    let part_1_result: String = stacks.iter().filter_map(|stack| stack.back()).collect();
    println!("Part 1: {part_1_result}");

    let mut stacks = initial_stacks.clone();
    for instruction in instructions.iter() {
        instruction.apply2(&mut stacks).unwrap();
    }

    let part_2_result: String = stacks.iter().filter_map(|stack| stack.back()).collect();
    println!("Part 2: {part_2_result}");
}
