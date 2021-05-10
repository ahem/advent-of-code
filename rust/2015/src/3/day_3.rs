use std::collections::HashSet;
use std::fs;

fn positions(chars: impl Iterator<Item = char>) -> impl Iterator<Item = (i32, i32)> {
    return chars.scan((0, 0), |state, c| {
        *state = match c {
            'v' => Some((state.0, state.1 - 1)),
            '^' => Some((state.0, state.1 + 1)),
            '<' => Some((state.0 - 1, state.1)),
            '>' => Some((state.0 + 1, state.1)),
            _ => None,
        }?;
        Some(*state)
    });
}

fn main() {
    let input = fs::read_to_string("./src/3/input.txt").unwrap();

    let mut seen = HashSet::new();
    seen.insert((0, 0));

    for pos in positions(input.chars()) {
        seen.insert(pos);
    }

    println!("part 1: {}", seen.len());

    let mut seen = HashSet::new();
    seen.insert((0, 0));

    for santa_pos in positions(input.chars().step_by(2)) {
        seen.insert(santa_pos);
    }
    for robot_pos in positions(input.chars().skip(1).step_by(2)) {
        seen.insert(robot_pos);
    }

    println!("part 2: {}", seen.len());
}
