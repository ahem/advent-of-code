type TRange = std::ops::RangeInclusive<u32>;
type TPair = (TRange, TRange);

fn parse_line(s: &str) -> Option<TPair> {
    let mut iter = s.splitn(2, ",");
    Some((parse_pair(iter.next()?)?, parse_pair(iter.next()?)?))
}

fn parse_pair(s: &str) -> Option<TRange> {
    let mut iter = s.splitn(2, "-");
    let a = iter.next()?.parse::<u32>().ok()?;
    let b = iter.next()?.parse::<u32>().ok()?;
    Some(a..=b)
}

fn read_input() -> Vec<TPair> {
    std::io::stdin()
        .lines()
        .map(|s| parse_line(s.unwrap().as_str()).unwrap())
        .collect()
}

fn contains(a: &TRange, b: &TRange) -> bool {
    b.clone().all(|c| a.contains(&c))
}

fn overlaps(a: &TRange, b: &TRange) -> bool {
    b.clone().any(|c| a.contains(&c))
}

pub fn main() {
    let input = read_input();
    let part_1_result = input
        .iter()
        .filter(|(a, b)| contains(a, b) || contains(b, a))
        .count();
    println!("part 1: {part_1_result}");

    let part_2_result = input.iter().filter(|(a, b)| overlaps(a, b)).count();
    println!("part 2: {part_2_result}");
}
