use std::borrow::Cow;

use regex::Captures;
use regex::Regex;

fn remove_qoutes(s: &str) -> &str {
    let len = s.len();
    &s[1..len - 1]
}

fn parse(s: &str) -> Cow<'_, str> {
    let re = Regex::new(r##"\\\\|\\"|\\x([[:xdigit:]]{2})"##).unwrap();
    let s = remove_qoutes(s);
    re.replace_all(s, |caps: &Captures| {
        let c = match &caps[0] {
            "\\\\" => '\\',
            "\\\"" => '"',
            _ => std::char::from_u32(u32::from_str_radix(&caps[1], 16).unwrap()).unwrap(),
        };
        c.to_string()
    })
}

fn main() {
    let input = std::fs::read_to_string("./src/8/input.txt").unwrap();
    let mut code_len = 0;
    let mut str_len = 0;

    for line in input.lines() {
        let parsed = parse(line);
        code_len += line.len();
        str_len += parsed.len();
        println!("{}  '{}'", line, parse(line))
    }

    println!("PART 1 RESULT: {}", code_len - str_len)
}
