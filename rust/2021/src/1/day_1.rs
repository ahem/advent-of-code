use itertools::Itertools;
use std::{io, io::prelude::*};

fn main() {
    let input: Vec<u32> = io::stdin()
        .lock()
        .lines()
        .map(|x| x.unwrap().parse::<u32>().unwrap())
        .collect();

    let part_1_result = input
        .iter()
        .tuple_windows()
        .filter(|(&a, &b)| b > a)
        .count();
    println!("Part 1: {}", part_1_result);

    let part_2_result = input
        .iter()
        .tuple_windows()
        .filter(|(&a, &b, &c, &d)| (b + c + d) > (a + b + c))
        .count();
    println!("Part 2: {}", part_2_result)
}
