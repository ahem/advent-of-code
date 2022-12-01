use std::{io, io::prelude::*};

fn main() {
    let mut lst = io::stdin().lock().lines().fold(vec![0], |mut acc, line| {
        match line.unwrap().parse::<u32>() {
            Ok(n) => *acc.last_mut().unwrap() += n,
            Err(_) => acc.push(0),
        }
        acc
    });
    lst.sort();

    let part_1_result = lst.last().unwrap();
    println!("part 1: {part_1_result}");

    let part_2_result = lst.iter().rev().take(3).sum::<u32>();
    println!("part 2: {part_2_result}");
}
