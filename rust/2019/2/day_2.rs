use intcode::IntCode;

fn read_input() -> anyhow::Result<Vec<i64>> {
    std::io::stdin()
        .lines()
        .next()
        .ok_or(anyhow::Error::msg("missing input"))??
        .split(',')
        .map(|s| i64::from_str_radix(s, 10))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| anyhow::Error::from(err))
}

pub fn main() -> anyhow::Result<()> {
    let input = read_input()?;
    let mut int_code = IntCode::new(&input);

    int_code.set_mem(1, 12);
    int_code.set_mem(2, 2);
    int_code.run()?;

    println!("Part 1: {}", int_code.get_mem(0));

    for noun in 0..99 {
        for verb in 0..99 {
            int_code = IntCode::new(&input);
            int_code.set_mem(1, noun);
            int_code.set_mem(2, verb);
            int_code.run()?;

            if int_code.get_mem(0) == 19690720 {
                println!("Part 2: {} ({}, {})", 100 * noun + verb, noun, verb);
            }
        }
    }

    Ok(())
}
