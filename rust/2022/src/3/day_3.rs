use std::{collections::HashSet, io, io::prelude::*};

use itertools::Itertools;

fn priority(c: char) -> u32 {
    if c >= 'a' && c <= 'z' {
        (c as u32 - 'a' as u32) + 1
    } else if c >= 'A' && c <= 'Z' {
        (c as u32 - 'A' as u32) + 27
    } else {
        panic!("invalid character")
    }
}

fn main() {
    let rucksacks: Vec<(HashSet<char>, HashSet<char>)> = io::stdin()
        .lock()
        .lines()
        .map(|s| {
            let s = s.unwrap();
            let x = s.len() / 2;
            let a = HashSet::from_iter(s[0..x].chars());
            let b = HashSet::from_iter(s[x..].chars());
            (a, b)
        })
        .collect();

    let part_1 = rucksacks
        .iter()
        .map(|sack| sack.0.intersection(&sack.1).last().unwrap())
        .map(|c| priority(*c))
        .sum::<u32>();

    println!("part 1: {part_1}");

    let mut sum = 0;
    for mut chunk in &rucksacks.iter().chunks(3) {
        let a = chunk.next().unwrap();
        let a: HashSet<char> = a.0.union(&a.1).copied().collect();
        let b = chunk.next().unwrap();
        let b: HashSet<char> = b.0.union(&b.1).copied().collect();
        let c = chunk.next().unwrap();
        let c: HashSet<char> = c.0.union(&c.1).copied().collect();

        let mut set: HashSet<char> = a.intersection(&b).copied().collect();
        set = set.intersection(&c).copied().collect();

        let chr = set.iter().last().unwrap();
        sum += priority(*chr);
    }

    println!("part 2: {sum}");
}
