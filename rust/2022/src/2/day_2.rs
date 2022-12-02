use std::{io, io::prelude::*};

enum Shape {
    Rock,
    Paper,
    Scissors,
}

impl From<char> for Shape {
    fn from(c: char) -> Self {
        match c {
            'A' | 'X' => Shape::Rock,
            'B' | 'Y' => Shape::Paper,
            'C' | 'Z' => Shape::Scissors,
            _ => unreachable!(),
        }
    }
}

fn parse_line(s: String) -> (Shape, Shape) {
    (
        Shape::from(s.chars().nth(0).unwrap()),
        Shape::from(s.chars().nth(2).unwrap()),
    )
}

fn score(shapes: &(Shape, Shape)) -> u32 {
    match shapes {
        (Shape::Rock, Shape::Rock) => 3 + 1,
        (Shape::Rock, Shape::Paper) => 6 + 2,
        (Shape::Rock, Shape::Scissors) => 0 + 3,
        (Shape::Paper, Shape::Rock) => 0 + 1,
        (Shape::Paper, Shape::Paper) => 3 + 2,
        (Shape::Paper, Shape::Scissors) => 6 + 3,
        (Shape::Scissors, Shape::Rock) => 6 + 1,
        (Shape::Scissors, Shape::Paper) => 0 + 2,
        (Shape::Scissors, Shape::Scissors) => 3 + 3,
    }
}

fn main() {
    let lst: Vec<(Shape, Shape)> = io::stdin()
        .lock()
        .lines()
        .map(|s| parse_line(s.unwrap()))
        .collect();

    let score = lst.iter().fold(0, |acc, shapes| acc + score(shapes));
    println!("\nPart 1: {score}");
}
