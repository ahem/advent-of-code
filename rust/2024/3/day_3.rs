use itertools::Itertools;
use regex::Regex;
use std::io::{stdin, Read};

#[derive(Debug, PartialEq)]
enum Token {
    Do,
    DoNot,
    Mul(u32, u32),
}

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();

    let re = Regex::new(r"do\(\)|don't\(\)|mul\((\d+),(\d+)\)").unwrap();
    let tokens = re
        .captures_iter(&input)
        .map(|m| match &m[0] {
            "do()" => Token::Do,
            "don't()" => Token::DoNot,
            _ => Token::Mul(m[1].parse::<u32>().unwrap(), m[2].parse::<u32>().unwrap()),
        })
        .collect_vec();

    let part_1: u32 = tokens
        .iter()
        .filter_map(|token| match token {
            Token::Mul(a, b) => Some(a * b),
            _ => None,
        })
        .sum();
    println!("part 1: {part_1}");

    let part_2: u32 = tokens
        .iter()
        .fold((0, true), |(acc, enabled), token| match token {
            Token::Do => (acc, true),
            Token::DoNot => (acc, false),
            Token::Mul(a, b) if enabled => (acc + a * b, true),
            Token::Mul(_, _) => (acc, false),
        })
        .0;
    println!("part 2: {part_2}");
}
