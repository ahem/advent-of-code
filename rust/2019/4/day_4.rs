use anyhow::anyhow;

fn digits(n: usize) -> [u8; 6] {
    let mut r = [0u8; 6];
    for i in 0..6 {
        r[i as usize] = ((n / usize::pow(10, 6 - (i + 1))) % 10) as u8;
    }
    return r;
}

fn is_never_decreasing(l: &[u8]) -> bool {
    l.chunk_by(|a, b| a <= b).count() == 1
}

fn has_identical_adjacent_digits(l: &[u8]) -> bool {
    l.chunk_by(|a, b| a == b).any(|x| x.len() >= 2)
}

fn has_exactly_two_adjacent_identical_digits(l: &[u8]) -> bool {
    l.chunk_by(|a, b| a == b).any(|x| x.len() == 2)
}

pub fn main() -> anyhow::Result<()> {
    let input = std::io::stdin()
        .lines()
        .next()
        .ok_or(anyhow!("invalid input"))??
        .split("-")
        .map(|s| usize::from_str_radix(s, 10))
        .collect::<Result<Vec<_>, _>>()?;

    if input.len() != 2 || input[0].ilog10() != 5 || input[1].ilog10() != 5 {
        return Err(anyhow!("invalid input"));
    }

    let range = input[0]..=input[1];

    let part_1_result = range
        .clone()
        .map(|n| digits(n))
        .filter(|d| is_never_decreasing(d))
        .filter(|d| has_identical_adjacent_digits(d))
        .count();

    println!("Part 1 result: {part_1_result}");

    let part_2_result = range
        .clone()
        .map(|n| digits(n))
        .filter(|d| is_never_decreasing(d))
        .filter(|d| has_exactly_two_adjacent_identical_digits(d))
        .count();

    println!("Part 2 result: {part_2_result}");

    Ok(())
}
