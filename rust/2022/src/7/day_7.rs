mod parser {
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alphanumeric1, digit1, space1};
    use nom::combinator::{map, map_res, rest};
    use nom::sequence::{preceded, separated_pair};
    use nom::IResult;

    #[derive(Debug, PartialEq, Clone)]
    pub enum Command {
        ChangeDirRoot,
        ChangeDirUp,
        ChangeDirDown(String),
        List,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Output {
        Directory(String),
        File(String, u32),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Line {
        Command(Command),
        Output(Output),
    }

    #[derive(Debug)]
    pub enum ParseError {
        Error(String),
        Unparsed(String),
    }

    fn command(input: &str) -> IResult<&str, Command> {
        let chdir_arg = alt((tag("/"), tag(".."), alphanumeric1));
        let chdir = map(preceded(tag("cd "), chdir_arg), |s| match s {
            "/" => Command::ChangeDirRoot,
            ".." => Command::ChangeDirUp,
            s => Command::ChangeDirDown(s.to_owned()),
        });
        let ls = map(tag("ls"), |_| Command::List);
        preceded(tag("$ "), alt((chdir, ls)))(input)
    }

    fn output(input: &str) -> IResult<&str, Output> {
        let size = map_res(digit1, |s: &str| s.parse::<u32>());
        let file = map(separated_pair(size, space1, rest), |(size, name)| {
            Output::File(name.to_string(), size)
        });
        let dir = map(preceded(tag("dir "), alphanumeric1), |s: &str| {
            Output::Directory(s.to_string())
        });
        alt((dir, file))(input)
    }

    fn line(input: &str) -> IResult<&str, Line> {
        alt((map(command, Line::Command), map(output, Line::Output)))(input)
    }

    pub fn parse_line(input: &str) -> Result<Line, ParseError> {
        match line(input) {
            Ok(("", result)) => Ok(result),
            Ok((rest, _)) => Err(ParseError::Unparsed(rest.to_string())),
            Err(err) => Err(ParseError::Error(err.to_string())),
        }
    }
}

use parser::*;
use std::collections::HashMap;
use std::path::PathBuf;

fn main() {
    let parsed_lines = std::io::stdin()
        .lines()
        .map(|s| parser::parse_line(s.unwrap().as_str()))
        .collect::<Result<Vec<Line>, ParseError>>()
        .unwrap();

    let mut files = HashMap::<PathBuf, u32>::new();
    let root = "/".parse::<PathBuf>().unwrap();
    let mut path = root.clone();

    // build directory tree
    for line in parsed_lines {
        match line {
            Line::Command(Command::ChangeDirRoot) => path = root.clone(),
            Line::Command(Command::ChangeDirUp) => match path.pop() {
                false => panic!("already at root!"),
                true => (),
            },
            Line::Command(Command::ChangeDirDown(s)) => path.push(s),
            Line::Command(Command::List) => (),
            Line::Output(Output::Directory(_)) => (),
            Line::Output(Output::File(name, size)) => {
                let mut path = path.clone();
                path.push(name);
                files.insert(path, size);
            }
        }
    }

    // count directory sizes
    let mut directories = HashMap::<PathBuf, u32>::new();

    for (path, size) in files {
        let mut dir = path.parent().unwrap().to_owned();
        loop {
            if directories.contains_key(&dir) {
                *directories.get_mut(&dir).unwrap() += size;
            } else {
                directories.insert(dir.clone(), size);
            }
            if !dir.pop() {
                break;
            }
        }
    }

    let small_dirs_size = directories.values().filter(|x| **x < 100000).sum::<u32>();
    println!("part 1: {small_dirs_size}");

    let total_size = 70000000;
    let needed_space = 30000000;
    let used_space = directories.values().max().unwrap();
    let need_to_free = needed_space - (total_size - used_space);

    let size_of_selected_dir = directories
        .values()
        .filter(|x| **x >= need_to_free)
        .min()
        .unwrap();

    println!("part 2: {size_of_selected_dir}");
}
