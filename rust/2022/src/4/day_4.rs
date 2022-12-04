mod assignment {
    pub struct Assignment {
        range: std::ops::RangeInclusive<u32>,
    }

    #[derive(Debug)]
    pub struct ParseError;

    impl Assignment {
        pub fn contains(&self, other: &Self) -> bool {
            self.range.clone().all(|c| other.range.contains(&c))
        }

        pub fn overlaps(&self, other: &Self) -> bool {
            self.range.clone().any(|c| other.range.contains(&c))
        }
    }

    impl std::str::FromStr for Assignment {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let mut iter = s
                .splitn(2, "-")
                .map(|s| s.parse::<u32>().or(Err(ParseError)));
            let a = iter.next().ok_or(ParseError)??;
            let b = iter.next().ok_or(ParseError)??;
            Ok(Assignment { range: a..=b })
        }
    }

    pub type AssignmentPair = (Assignment, Assignment);

    pub fn parse_pair(s: &str) -> Result<AssignmentPair, ParseError> {
        let mut iter = s.splitn(2, ",").map(|s| s.parse::<Assignment>());
        let a = iter.next().ok_or(ParseError)??;
        let b = iter.next().ok_or(ParseError)??;
        Ok((a, b))
    }
}

use assignment::AssignmentPair;

#[derive(Debug)]
enum Error {
    ParseError(assignment::ParseError),
    IOError(std::io::Error),
}

fn read_input() -> Result<Vec<AssignmentPair>, Error> {
    std::io::stdin()
        .lines()
        .map(|s| s.map_err(Error::IOError))
        .map(|s| assignment::parse_pair(s?.as_str()).map_err(Error::ParseError))
        .collect()
}

pub fn main() {
    let pairs = read_input().unwrap();
    let part_1_result = pairs
        .iter()
        .filter(|(a, b)| a.contains(b) || b.contains(a))
        .count();
    println!("part 1: {part_1_result}");

    let part_2_result = pairs.iter().filter(|(a, b)| a.overlaps(b)).count();
    println!("part 2: {part_2_result}");
}
