use std::collections::HashSet;

fn substrings(s: String, size: usize) -> impl Iterator<Item = String> {
    (0..(s.len() - size)).map(move |n| s[n..(n + size)].to_owned())
}

fn find_first_marker(s: &str, size: usize) -> Option<usize> {
    for (idx, s) in substrings(s.to_owned(), size).enumerate() {
        if HashSet::<char>::from_iter(s.chars()).len() == size {
            return Some(idx + size);
        }
    }
    None
}

fn main() {
    let line = std::io::stdin().lines().next().unwrap().unwrap();
    println!("part 1: {}", find_first_marker(line.as_str(), 4).unwrap());
    println!("part 2: {}", find_first_marker(line.as_str(), 14).unwrap());
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_find_first_marker() {
        use super::find_first_marker as f;
        assert_eq!(f("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4), Some(7));
        assert_eq!(f("bvwbjplbgvbhsrlpgdmjqwftvncz", 4), Some(5));
        assert_eq!(f("nppdvjthqldpwncqszvftbrmjlhg", 4), Some(6));
        assert_eq!(f("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4), Some(10));
        assert_eq!(f("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4), Some(11));

        assert_eq!(f("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14), Some(19));
        assert_eq!(f("bvwbjplbgvbhsrlpgdmjqwftvncz", 14), Some(23));
        assert_eq!(f("nppdvjthqldpwncqszvftbrmjlhg", 14), Some(23));
        assert_eq!(f("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14), Some(29));
        assert_eq!(f("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14), Some(26));
    }
}
