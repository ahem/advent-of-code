use regex::Regex;

fn search(input: &str, re: Regex) -> usize {
    (0..)
        .map(|n| md5::compute(format!("{}{}", input, n)))
        .map(|digest| format!("{:x}", digest))
        .position(|s| re.is_match(&s))
        .unwrap()
}

fn main() {
    let input = "iwrupvqb";

    let part_1_result = search(input, Regex::new(r"^0{5}").unwrap());
    println!("part 1 result: {}", part_1_result);

    let part_2_result = search(input, Regex::new(r"^0{6}").unwrap());
    println!("part 2 result: {}", part_2_result);
}
