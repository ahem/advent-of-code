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

    int_code.send_input(1)?;
    int_code.run()?;

    let mut output = Vec::new();
    while let Ok(val) = int_code.read_output() {
        output.push(val);
    }

    if output.len() > 1 {
        eprintln!("Errors in test: {output:?}");
    } else {
        println!("Part 1 result: {}", output[0]);
    }

    let mut int_code = IntCode::new(&input);
    int_code.send_input(2)?;
    int_code.run()?;
    println!("Part 2 result: {}", int_code.read_output()?);

    Ok(())
}
