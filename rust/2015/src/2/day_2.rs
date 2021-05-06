use regex::Regex;
use std::fs;

fn main() {
    let re = Regex::new(r"(\d+)x(\d+)x(\d+)").unwrap();
    let input = fs::read_to_string("./src/2/input.txt").unwrap();
    let values: Vec<(i32, i32, i32)> = re
        .captures_iter(&input)
        .map(|cap| {
            (
                cap[1].parse::<i32>().unwrap(),
                cap[2].parse::<i32>().unwrap(),
                cap[3].parse::<i32>().unwrap(),
            )
        })
        .collect();

    let part_1_result = values.iter().fold(0, |acc, (w, l, h)| {
        let a = l * w;
        let b = w * h;
        let c = h * l;
        return acc + 2 * a + 2 * b + 2 * c + a.min(b).min(c);
    });

    println!("part 1 result: {}", part_1_result);

    let part_2_result = values.iter().fold(0, |acc, (w, l, h)| {
        let a = w.min(l).min(h);
        let b = match a {
            _ if a == w => l.min(h),
            _ if a == l => w.min(h),
            _ => w.min(l),
        };
        return acc + 2 * a + 2 * b + w * l * h;
    });

    println!("part 2 result: {}", part_2_result);
}
