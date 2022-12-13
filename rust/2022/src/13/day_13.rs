#[derive(Debug, PartialEq, Clone, Ord, Eq)]
pub enum Item {
    List(Vec<Item>),
    Int(i32),
}

impl std::cmp::PartialOrd for Item {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Item::Int(a), Item::Int(b)) => a.partial_cmp(&b),
            (Item::Int(_), Item::List(_)) => Item::List(vec![self.clone()]).partial_cmp(other),
            (Item::List(_), Item::Int(_)) => self.partial_cmp(&Item::List(vec![other.clone()])),
            (Item::List(a), Item::List(b)) => a.partial_cmp(&b),
        }
    }
}

mod parser {
    use super::Item;
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::digit1;
    use nom::character::complete::multispace0;
    use nom::combinator::{map, map_res};
    use nom::multi::separated_list0;
    use nom::sequence::{delimited, separated_pair};
    use nom::IResult;

    fn int(input: &str) -> IResult<&str, Item> {
        map_res(digit1, |s: &str| s.parse::<i32>().map(Item::Int))(input)
    }

    fn list(input: &str) -> IResult<&str, Item> {
        map(
            delimited(tag("["), separated_list0(tag(","), item), tag("]")),
            Item::List,
        )(input)
    }

    fn item(input: &str) -> IResult<&str, Item> {
        alt((int, list))(input)
    }

    fn pair(input: &str) -> IResult<&str, (Item, Item)> {
        separated_pair(item, tag("\n"), item)(input)
    }

    pub fn parse_pairs(input: &str) -> IResult<&str, Vec<(Item, Item)>> {
        separated_list0(tag("\n\n"), pair)(input)
    }

    pub fn parse_list(input: &str) -> IResult<&str, Vec<Item>> {
        separated_list0(multispace0, item)(input)
    }
}

fn read_input() -> Result<String, std::io::Error> {
    use std::io::Read;
    let mut s = String::new();
    std::io::stdin().lock().read_to_string(&mut s)?;
    return Ok(s);
}

fn main() {
    let input = read_input().unwrap();

    let (_, pairs) = parser::parse_pairs(input.as_str()).unwrap();
    let part_1_result = pairs
        .iter()
        .enumerate()
        .filter_map(|(n, (a, b))| if a < b { Some(n + 1) } else { None })
        .sum::<usize>();

    println!("Part 1: {part_1_result}");

    let (_, mut list) = parser::parse_list(input.as_str()).unwrap();
    let (_, mut dividers) = parser::parse_pairs("[[2]]\n[[6]]").unwrap();
    let (start, end) = dividers.pop().unwrap();
    list.push(start.clone());
    list.push(end.clone());
    list.sort();

    let start_idx = list.iter().position(|o| *o == start).unwrap() + 1;
    let end_idx = list.iter().position(|o| *o == end).unwrap() + 1;
    let part_2_result = start_idx * end_idx;
    println!("Part 2: {start_idx} * {end_idx} = {part_2_result}");
}
