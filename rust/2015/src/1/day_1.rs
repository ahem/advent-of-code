use std::fs;

fn to_levels(s: &str) -> impl Iterator<Item = i32> + '_ {
    s.chars().scan(0, |state, c| {
        *state += match c {
            '(' => Some(1),
            ')' => Some(-1),
            _ => None,
        }?;
        Some(*state)
    })
}

fn main() {
    let input = fs::read_to_string("./src/1/input.txt").unwrap();

    let part_1_result = to_levels(&input).last().unwrap();
    println!("Part 1: {}", part_1_result);

    let part_2_result = to_levels(&input)
        .position(|n| n == -1)
        .map(|x| x + 1)
        .unwrap();

    println!("Part 2: {}", part_2_result);
}
