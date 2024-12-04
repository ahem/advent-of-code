use std::{
    char,
    collections::HashMap,
    fmt::Display,
    io::{stdin, Read},
    str::FromStr,
};

#[derive(Debug)]
struct WordSearch {
    grid: HashMap<(i32, i32), char>,
}

impl FromStr for WordSearch {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lines = s.split_whitespace();
        let grid: HashMap<_, _> = lines
            .enumerate()
            .flat_map(|(y, line)| {
                line.chars()
                    .enumerate()
                    .map(move |(x, char)| ((x as i32, y as i32), char))
            })
            .collect();
        Ok(Self { grid })
    }
}

impl Display for WordSearch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (max_x, max_y) = self.grid.keys().max().unwrap().clone();
        for y in 0..=max_y {
            for x in 0..=max_x {
                write!(f, "{}", self.grid[&(x, y)])?;
            }
            if y < max_y {
                write!(f, "\n")?;
            }
        }
        Ok(())
    }
}

impl WordSearch {
    fn is_word(&self, s: &str, pos: &(i32, i32), direction: &(i32, i32)) -> bool {
        let (x, y) = pos;
        let (dx, dy) = direction;
        s.chars()
            .enumerate()
            .map(|(i, c)| ((x + dx * (i as i32), (y + dy * (i as i32))), c))
            .all(|(pos, c)| self.grid.get(&pos) == Some(&c))
    }

    pub fn count_words(&self, s: &str) -> usize {
        #[rustfmt::skip]
        let directions: [(i32, i32); 8] = [
            (-1,  1), (0,  1), (1,  1),
            (-1,  0),          (1,  0),
            (-1, -1), (0, -1), (1, -1),
        ];
        self.grid
            .keys()
            .flat_map(|pos| directions.map(|dir| ((*pos).clone(), dir.clone())))
            .filter(|(pos, dir)| self.is_word(s, pos, dir))
            .count()
    }

    #[rustfmt::skip]
    pub fn count_xmas(&self, s: &str) -> usize {
        self.grid
            .keys()
            .filter(|(x, y)| {
                (self.is_word(s, &(x - 1, y - 1), &(1,  1)) || self.is_word(s, &(x + 1, y + 1), &(-1, -1))) &&
                (self.is_word(s, &(x - 1, y + 1), &(1, -1)) || self.is_word(s, &(x + 1, y - 1), &(-1,  1)))
            })
            .count()
    }
}

fn main() -> anyhow::Result<()> {
    let mut input = String::new();
    stdin().read_to_string(&mut input)?;
    let word_search = WordSearch::from_str(&input)?;
    // println!("{word_search}");
    println!("part 1: {}", word_search.count_words("XMAS"));
    println!("part 2: {}", word_search.count_xmas("MAS"));
    Ok(())
}
