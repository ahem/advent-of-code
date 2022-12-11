use std::str::FromStr;

#[derive(Debug)]
enum Error {
    ParseError(String),
}

#[derive(Debug)]
enum Instruction {
    NoOp,
    AddX(i32),
}

impl FromStr for Instruction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.splitn(2, " ").map(|s| s.trim());
        match iter.next() {
            Some("noop") => Ok(Instruction::NoOp),
            Some("addx") => {
                let n = iter
                    .next()
                    .ok_or(Error::ParseError(String::from(
                        "expected integer, got <None>",
                    )))?
                    .parse::<i32>()
                    .map_err(|e| Error::ParseError(e.to_string()))?;
                Ok(Instruction::AddX(n))
            }
            Some(s) => Err(Error::ParseError(format!("Invalid insruction {s:?}"))),
            None => Err(Error::ParseError(String::from(
                "invalid instruction <None>",
            ))),
        }
    }
}

fn main() {
    let input = std::io::stdin()
        .lines()
        .map(|s| s.unwrap().parse())
        .collect::<Result<Vec<Instruction>, Error>>()
        .unwrap();

    let instructions: Vec<Instruction> = input
        .iter()
        .flat_map(|i| match i {
            Instruction::NoOp => vec![Instruction::NoOp],
            Instruction::AddX(n) => vec![Instruction::NoOp, Instruction::AddX(*n)],
        })
        .collect();

    let mut register = 1;
    let mut sum = 0;
    for (cycle, instruction) in instructions.iter().enumerate() {
        if [20, 60, 100, 140, 180, 220].contains(&(cycle + 1)) {
            sum += register * ((cycle + 1) as i32);
        }
        match instruction {
            Instruction::NoOp => (),
            Instruction::AddX(n) => {
                register += n;
            }
        }
    }

    println!("\npart 1: {sum}");

    register = 1;
    for (cycle, instruction) in instructions.iter().enumerate() {
        let pos: i32 = ((cycle) % 40) as i32;
        if register >= pos - 1 && register <= pos + 1 {
            print!("#");
        } else {
            print!(".");
        }
        if pos == 39 {
            print!("\n");
        }
        match instruction {
            Instruction::NoOp => (),
            Instruction::AddX(n) => {
                register += n;
            }
        }
    }
}
