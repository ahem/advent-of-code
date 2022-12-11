use std::collections::VecDeque;

#[derive(Debug)]
enum Operation {
    Add(usize),
    Mul(usize),
    Square,
}

#[derive(Debug)]
pub struct Monkey {
    items: VecDeque<usize>,
    operation: Operation,
    test_divisible_by: usize,
    if_true_throw_to: usize,
    if_false_throw_to: usize,
    activity: usize,
}

mod parser {
    use std::collections::VecDeque;

    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{digit1, multispace0};
    use nom::combinator::{map, map_res};
    use nom::multi::{many0, separated_list1};
    use nom::sequence::{delimited, preceded, terminated};
    use nom::IResult;

    fn number(input: &str) -> IResult<&str, usize> {
        map_res(digit1, |s: &str| s.parse::<usize>())(input)
    }

    fn id(input: &str) -> IResult<&str, usize> {
        delimited(tag("Monkey "), number, tag(":"))(input)
    }

    fn starting_items(input: &str) -> IResult<&str, VecDeque<usize>> {
        map(
            preceded(tag("Starting items: "), separated_list1(tag(", "), number)),
            |lst: Vec<usize>| lst.into(),
        )(input)
    }

    fn test_divisible_by(input: &str) -> IResult<&str, usize> {
        preceded(tag("Test: divisible by "), number)(input)
    }

    fn operation(input: &str) -> IResult<&str, super::Operation> {
        let add = map(preceded(tag(" + "), number), super::Operation::Add);
        let mul = map(preceded(tag(" * "), number), super::Operation::Mul);
        let square = map(tag(" * old"), |_: &str| super::Operation::Square);
        preceded(tag("Operation: new = old"), alt((add, mul, square)))(input)
    }

    pub fn monkey(s: &str) -> IResult<&str, super::Monkey> {
        let (s, _id) = terminated(id, multispace0)(s)?;
        let (s, items) = terminated(starting_items, multispace0)(s)?;
        let (s, operation) = terminated(operation, multispace0)(s)?;
        let (s, test_divisible_by) = terminated(test_divisible_by, multispace0)(s)?;
        let (s, if_true_throw_to) = terminated(
            preceded(tag("If true: throw to monkey "), number),
            multispace0,
        )(s)?;
        let (s, if_false_throw_to) = terminated(
            preceded(tag("If false: throw to monkey "), number),
            multispace0,
        )(s)?;

        return Ok((
            s,
            super::Monkey {
                items,
                operation,
                test_divisible_by,
                if_true_throw_to,
                if_false_throw_to,
                activity: 0,
            },
        ));
    }

    #[derive(Debug)]
    pub enum ParseError<'a> {
        Unparsed(&'a str),
        Error(nom::Err<nom::error::Error<&'a str>>),
    }

    pub fn parse_monkeys(input: &str) -> Result<Vec<super::Monkey>, ParseError> {
        match many0(monkey)(input) {
            Ok(("", monkeys)) => Ok(monkeys),
            Ok((rest, _)) => Err(ParseError::Unparsed(rest)),
            Err(err) => Err(ParseError::Error(err)),
        }
    }
}

#[derive(Debug)]
struct InspectionResult {
    level: usize,
    to: usize,
}

impl Monkey {
    fn apply_operation(&self, item: usize) -> usize {
        match self.operation {
            Operation::Add(n) => item + n,
            Operation::Mul(n) => item * n,
            Operation::Square => item * item,
        }
    }

    fn inspect_next_item(&mut self, relief_factor: usize) -> Option<InspectionResult> {
        let n = self.items.pop_front()?;
        let level = self.apply_operation(n) / relief_factor;
        self.activity += 1;

        if level % self.test_divisible_by == 0 {
            Some(InspectionResult {
                to: self.if_true_throw_to,
                level,
            })
        } else {
            Some(InspectionResult {
                to: self.if_false_throw_to,
                level,
            })
        }
    }

    fn catch_item(&mut self, item: usize) {
        self.items.push_back(item);
    }
}

fn play_round(monkeys: &mut Vec<Monkey>, relief_factor: usize) {
    for n in 0..(monkeys.len()) {
        loop {
            match monkeys[n].inspect_next_item(relief_factor) {
                Some(r) => monkeys[r.to].catch_item(r.level),
                None => break,
            }
        }
    }
}

fn monkey_business(monkeys: &Vec<Monkey>) -> usize {
    let mut activies: Vec<usize> = monkeys.iter().map(|n| n.activity).collect();
    activies.sort();
    let n = activies.len();
    activies[n - 1] * activies[n - 2]
}

fn read_input() -> Result<String, std::io::Error> {
    use std::io::Read;
    let mut s = String::new();
    std::io::stdin().lock().read_to_string(&mut s)?;
    return Ok(s);
}

fn main() {
    let s = read_input().unwrap();
    let mut monkeys = parser::parse_monkeys(s.as_str()).unwrap();
    for _ in 0..20 {
        play_round(&mut monkeys, 3);
    }
    println!("part 1: {}", monkey_business(&monkeys));
}
