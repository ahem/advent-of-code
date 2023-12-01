use regex::Regex;

fn read_input<T: std::io::BufRead>(f: T) -> Result<Vec<String>, std::io::Error> {
    f.lines().map(|s| s.map(|s| s.trim().to_owned())).collect()
}

fn digit_str_to_int(s: &str) -> Result<u32, std::num::ParseIntError> {
    let value = match s {
        "one" => 1,
        "two" => 2,
        "three" => 3,
        "four" => 4,
        "five" => 5,
        "six" => 6,
        "seven" => 7,
        "eight" => 8,
        "nine" => 9,
        s => u32::from_str_radix(s, 10)?,
    };
    Ok(value)
}

fn calibration_value(re: &Regex, s: &str) -> Option<u32> {
    let mut pos = 0;
    let mut digits = Vec::new();
    while let Some(m) = re.find_at(s, pos) {
        digits.push(digit_str_to_int(m.as_str()).unwrap());
        pos = m.start() + 1;
    }
    let first = digits.first()?;
    let last = digits.last()?;
    Some(first * 10 + last)
}

fn main() {
    let input = read_input(std::io::stdin().lock()).unwrap();
    let part_1_regex = Regex::new(r"\d").unwrap();
    let calibration_value_sum = input.iter().fold(0, |acc, s| {
        acc + calibration_value(&part_1_regex, s).unwrap()
    });
    println!("Part 1: {calibration_value_sum}");

    let part_2_regex = Regex::new(r"\d|one|two|three|four|five|six|seven|eight|nine").unwrap();
    let calibration_value_sum = input.iter().fold(0, |acc, s| {
        acc + calibration_value(&part_2_regex, s).unwrap()
    });
    println!("Part 2: {calibration_value_sum}");
}
