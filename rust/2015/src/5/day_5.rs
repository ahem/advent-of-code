use lazy_static::lazy_static;
use std::fs;

fn has_duplicated_char(s: &str) -> bool {
    s.chars()
        .scan(None, |state, c| {
            let m = *state == Some(c);
            *state = Some(c);
            Some(m)
        })
        .any(|b| b)
}

fn has_three_vowels(s: &str) -> bool {
    lazy_static! {
        static ref VOWELS: regex::Regex = regex::Regex::new(r"[aeiou]").unwrap();
    }
    VOWELS.find_iter(&s).count() >= 3
}

fn has_no_illegal_pairs(s: &str) -> bool {
    lazy_static! {
        static ref ILLEGAL_PAIRS: regex::Regex = regex::Regex::new(r"ab|cd|pq|xy").unwrap();
    }
    !ILLEGAL_PAIRS.is_match(&s)
}

fn has_pair_twice(s: &str) -> bool {
    lazy_static! {
        static ref PAIRS: fancy_regex::Regex =
            fancy_regex::Regex::new(r"([a-z][a-z]).*\1").unwrap();
    }
    PAIRS.is_match(&s).unwrap()
}

fn has_repeat_with_one_between(s: &str) -> bool {
    lazy_static! {
        static ref REPEAT_WITH_ONE_BETWEEN: fancy_regex::Regex =
            fancy_regex::Regex::new(r"([a-z]).\1").unwrap();
    }
    REPEAT_WITH_ONE_BETWEEN.is_match(&s).unwrap()
}

fn main() {
    let input = fs::read_to_string("./src/5/input.txt").unwrap();
    let part_1_result = input
        .lines()
        .filter(|s| has_three_vowels(&s) && has_duplicated_char(&s) && has_no_illegal_pairs(&s))
        .count();

    println!("Part 1 result: {}", part_1_result);

    let part_2_result = input
        .lines()
        .filter(|s| has_pair_twice(&s) && has_repeat_with_one_between(&s))
        .count();

    println!("Part 2 result: {}", part_2_result);
}
