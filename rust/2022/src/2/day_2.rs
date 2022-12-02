use std::{io, io::prelude::*};

enum Shape {
    Rock,
    Paper,
    Scissors,
}

fn score(shapes: (Shape, Shape)) -> u32 {
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

fn part_1_parse(s: &String) -> (Shape, Shape) {
    match s.trim() {
        "A X" => (Shape::Rock, Shape::Rock),
        "A Y" => (Shape::Rock, Shape::Paper),
        "A Z" => (Shape::Rock, Shape::Scissors),
        "B X" => (Shape::Paper, Shape::Rock),
        "B Y" => (Shape::Paper, Shape::Paper),
        "B Z" => (Shape::Paper, Shape::Scissors),
        "C X" => (Shape::Scissors, Shape::Rock),
        "C Y" => (Shape::Scissors, Shape::Paper),
        "C Z" => (Shape::Scissors, Shape::Scissors),
        _ => unreachable!(),
    }
}

fn part_2_parse(s: &String) -> (Shape, Shape) {
    match s.trim() {
        "A X" => (Shape::Rock, Shape::Scissors),
        "A Y" => (Shape::Rock, Shape::Rock),
        "A Z" => (Shape::Rock, Shape::Paper),
        "B X" => (Shape::Paper, Shape::Rock),
        "B Y" => (Shape::Paper, Shape::Paper),
        "B Z" => (Shape::Paper, Shape::Scissors),
        "C X" => (Shape::Scissors, Shape::Paper),
        "C Y" => (Shape::Scissors, Shape::Scissors),
        "C Z" => (Shape::Scissors, Shape::Rock),
        _ => unreachable!(),
    }
}

fn main() {
    let input: Vec<String> = io::stdin().lock().lines().map(|s| s.unwrap()).collect();

    let part_1_score = input
        .iter()
        .map(part_1_parse)
        .fold(0, |acc, shapes| acc + score(shapes));

    println!("\nPart 1: {part_1_score}");

    let part_2_score = input
        .iter()
        .map(part_2_parse)
        .fold(0, |acc, shapes| acc + score(shapes));

    println!("\nPart 2: {part_2_score}");
}
