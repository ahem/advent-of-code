use std::{io, io::prelude::*};

fn main() {
    /*
    use itertools::Itertools;
    let lst = io::stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap().parse::<u32>())
        .group_by(|x| x.is_ok())
        .into_iter()
        .filter_map(|x| {
            if x.0 {
                Some(x.1.into_iter().map(|x| x.unwrap()).sum::<u32>())
            } else {
                None
            }
        })
        .collect_vec();

    let part_1_result = lst.iter().max().unwrap();
    println!("part 1: {part_1_result}");

    let part_2_result = lst.iter().sorted().rev().take(3).sum::<u32>();
    println!("part 2: {part_2_result}");
    */

    let mut lst = vec![];
    let mut sum = 0;
    for line in io::stdin().lock().lines() {
        match line.unwrap().parse::<u32>() {
            Ok(n) => sum += n,
            Err(_) => {
                lst.push(sum);
                sum = 0;
            }
        }
    }

    let part_1_result = lst.iter().max().unwrap();
    println!("part 1: {part_1_result}");

    lst.sort();
    let part_2_result = lst.iter().rev().take(3).sum::<u32>();
    println!("part 2: {part_2_result}");
}
