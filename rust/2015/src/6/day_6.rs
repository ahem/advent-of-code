use lazy_static::lazy_static;

type Grid<T> = [[T; 1000]; 1000];

struct Rect {
    x1: usize,
    y1: usize,
    x2: usize,
    y2: usize,
}

enum Instruction {
    TurnOn(Rect),
    TurnOff(Rect),
    Toggle(Rect),
}

fn parse(s: &str) -> Vec<Instruction> {
    lazy_static! {
        static ref PATTERN: regex::Regex =
            regex::Regex::new(r"(toggle|turn off|turn on) (\d+),(\d+) through (\d+),(\d+)")
                .unwrap();
    }
    PATTERN
        .captures_iter(&s)
        .map(|cap| {
            let x1 = cap[2].parse::<usize>().unwrap();
            let y1 = cap[3].parse::<usize>().unwrap();
            let x2 = cap[4].parse::<usize>().unwrap();
            let y2 = cap[5].parse::<usize>().unwrap();
            let rect = Rect { x1, y1, x2, y2 };
            match &cap[1] {
                "toggle" => Instruction::Toggle(rect),
                "turn on" => Instruction::TurnOn(rect),
                "turn off" => Instruction::TurnOff(rect),
                _ => panic!(),
            }
        })
        .collect()
}

fn update_rect<T>(grid: &mut Grid<T>, rect: &Rect, f: fn(&T) -> T) {
    for row in grid[rect.y1..rect.y2 + 1].iter_mut() {
        for cell in row[rect.x1..rect.x2 + 1].iter_mut() {
            *cell = f(cell);
        }
    }
}

fn fold_lights<Acc, T>(grid: &Grid<T>, initial: Acc, f: fn(Acc, &T) -> Acc) -> Acc {
    return grid
        .iter()
        .fold(initial, |acc, row| row.iter().fold(acc, f));
}

fn main() {
    let input = std::fs::read_to_string("./src/6/input.txt").unwrap();
    let instructions: Vec<Instruction> = parse(&input);

    let mut grid: Grid<bool> = [[false; 1000]; 1000];

    for instr in instructions.iter() {
        match &instr {
            Instruction::TurnOn(rect) => update_rect(&mut grid, &rect, |_| true),
            Instruction::TurnOff(rect) => update_rect(&mut grid, &rect, |_| false),
            Instruction::Toggle(rect) => update_rect(&mut grid, &rect, |&v| !v),
        }
    }
    let part_1_result = fold_lights(&grid, 0, |acc, &v| if v { acc + 1 } else { acc });
    println!("part 1: {}", part_1_result);

    let mut grid = [[0; 1000]; 1000];

    for instr in instructions.iter() {
        match &instr {
            Instruction::TurnOn(rect) => update_rect(&mut grid, &rect, |&v| v + 1),
            Instruction::TurnOff(rect) => {
                update_rect(&mut grid, &rect, |&v| if v > 0 { v - 1 } else { 0 })
            }
            Instruction::Toggle(rect) => update_rect(&mut grid, &rect, |&v| v + 2),
        }
    }
    let part_2_result = fold_lights(&grid, 0, |acc, &v| acc + v);
    println!("part 2: {}", part_2_result);
}
