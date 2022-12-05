use std::io::Read;

#[derive(Debug)]
pub struct ParseError;

mod stacks {
    use std::collections::LinkedList;
    use std::str::FromStr;

    type Stack = LinkedList<char>;

    #[derive(Debug)]
    pub enum Error {
        IndexError,
        EmptyStack,
    }

    #[derive(Clone, Debug)]
    pub struct Stacks {
        stacks: Vec<Stack>,
    }

    impl Stacks {
        pub fn get_mut(&mut self, index: usize) -> Result<&mut Stack, Error> {
            self.stacks.get_mut(index).ok_or(Error::IndexError)
        }

        pub fn pop_item(&mut self, index: usize) -> Result<char, Error> {
            self.get_mut(index)?.pop_back().ok_or(Error::EmptyStack)
        }

        pub fn move_item(&mut self, from: usize, to: usize) -> Result<(), Error> {
            let item = self.pop_item(from)?;
            self.get_mut(to)?.push_back(item);
            Ok(())
        }

        pub fn move_items(&mut self, quantity: usize, from: usize, to: usize) -> Result<(), Error> {
            let from_stack = self.get_mut(from)?;
            let size = from_stack.len();
            if size < quantity {
                return Err(Error::EmptyStack);
            }
            let mut items = from_stack.split_off(size - quantity);
            self.get_mut(to)?.append(&mut items);
            Ok(())
        }

        pub fn top_row(&self) -> Result<String, Error> {
            self.stacks
                .iter()
                .map(|stack| stack.back().ok_or(Error::EmptyStack))
                .collect()
        }
    }

    impl FromStr for Stacks {
        type Err = super::ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let mut stacks: Vec<LinkedList<char>> = vec![];
            let mut lines = s.lines().rev();

            for _ in 0..(lines.next().ok_or(super::ParseError)?.len() / 4 + 1) {
                stacks.push(LinkedList::new());
            }

            for line in lines {
                for (i, c) in line.chars().skip(1).step_by(4).enumerate() {
                    if c != ' ' {
                        stacks[i].push_back(c);
                    }
                }
            }

            Ok(Stacks { stacks })
        }
    }
}

mod instruction {
    use lazy_static::lazy_static;
    use regex::Regex;
    use std::str::FromStr;

    #[derive(Debug)]
    pub struct Instruction {
        pub quantity: usize,
        pub from: usize,
        pub to: usize,
    }

    impl FromStr for Instruction {
        type Err = super::ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            lazy_static! {
                static ref RE: Regex = Regex::new(r"^move (\d+) from (\d+) to (\d+)$").unwrap();
            }

            let captures = RE.captures(s).ok_or(super::ParseError)?;
            let captures = captures.iter().skip(1);
            let mut captures = captures.filter_map(|c| c?.as_str().parse::<usize>().ok());
            let quantity = captures.next().ok_or(super::ParseError)?;
            let from = captures.next().ok_or(super::ParseError)?;
            let to = captures.next().ok_or(super::ParseError)?;

            Ok(Instruction {
                quantity,
                from: from.checked_sub(1).ok_or(super::ParseError)?,
                to: to.checked_sub(1).ok_or(super::ParseError)?,
            })
        }
    }
}

fn read_input() -> Result<String, std::io::Error> {
    let mut s = String::new();
    std::io::stdin().lock().read_to_string(&mut s)?;
    Ok(s)
}

fn parse(s: &str) -> Result<(stacks::Stacks, Vec<instruction::Instruction>), ParseError> {
    let mut iter = s.splitn(2, "\n\n");
    let stacks = iter.next().ok_or(ParseError)?.parse::<stacks::Stacks>()?;

    let instructions: Result<Vec<instruction::Instruction>, ParseError> = iter
        .next()
        .ok_or(ParseError)?
        .lines()
        .map(|l| l.parse::<instruction::Instruction>())
        .collect();

    Ok((stacks, instructions?))
}

fn main() {
    let input = read_input().unwrap();
    let (stacks, instructions) = parse(input.as_str()).unwrap();

    {
        let mut stacks = stacks.clone();
        instructions
            .iter()
            .try_for_each(|instruction::Instruction { quantity, from, to }| {
                (0..*quantity).try_for_each(|_| stacks.move_item(*from, *to))
            })
            .unwrap();
        println!("Part 1: {}", stacks.top_row().unwrap());
    }

    {
        let mut stacks = stacks.clone();
        instructions
            .iter()
            .try_for_each(|instruction::Instruction { quantity, from, to }| {
                stacks.move_items(*quantity, *from, *to)
            })
            .unwrap();
        println!("Part 2: {}", stacks.top_row().unwrap());
    }
}
