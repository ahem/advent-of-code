use std::io::stdin;

fn fuel(m: i32) -> i32 {
    m / 3 - 2
}

fn accumulated_fuel(m: i32) -> i32 {
    match fuel(m) {
        r if r > 0 => r + accumulated_fuel(r),
        _ => 0,
    }
}

pub fn main() {
    let input: Vec<i32> = stdin()
        .lines()
        .map(|s| i32::from_str_radix(&s.unwrap(), 10).unwrap())
        .collect();

    let part_1 = input.iter().fold(0, |acc, m| acc + fuel(*m));
    println!("Part 1: {part_1}");

    let part_2 = input.iter().fold(0, |acc, m| acc + accumulated_fuel(*m));
    println!("Part 2: {part_2}");
}
