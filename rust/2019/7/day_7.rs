use anyhow::anyhow;
use intcode::IntCode;
use itertools::Itertools;

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

pub fn amplifiers(program: &[i64], phases: &[u8]) -> anyhow::Result<i64> {
    let mut amps = std::iter::repeat_with(|| IntCode::new(program))
        .take(5)
        .collect::<Vec<_>>();

    let mut res = 0;
    for (i, amp) in amps.iter_mut().enumerate() {
        amp.send_input(phases[i] as i64)?;
        amp.send_input(res)?;
        amp.run()?;
        res = amp.read_output()?;
    }
    return Ok(res);
}

pub fn feedback_amplifiers(program: &[i64], phases: &[u8]) -> anyhow::Result<i64> {
    let mut amps = std::iter::repeat_with(|| IntCode::new(program).run_async())
        .take(5)
        .collect::<Result<Vec<_>, _>>()?;

    for (i, amp) in amps.iter_mut().enumerate() {
        amp.0.send(phases[i] as i64)?;
    }

    let mut res = 0;

    loop {
        for amp in amps.iter() {
            amp.0.send(res)?;
            res = amp.1.recv()?;
        }

        if amps.iter().all(|x| x.2.is_finished()) {
            for amp in amps {
                amp.2.join().map_err(|err| anyhow!("{err:?}"))??;
            }
            break;
        }
    }

    Ok(res)
}

pub fn main() -> anyhow::Result<()> {
    let input = read_input()?;

    let part_1_result = (0u8..=4u8)
        .permutations(5)
        .map(|phases| amplifiers(&input, &phases))
        .fold(anyhow::Ok(0), |acc, x| Ok(std::cmp::max(acc?, x?)))?;

    println!("Part 1: {part_1_result}");

    let part_2_result = (5u8..=9u8)
        .permutations(5)
        .map(|phases| feedback_amplifiers(&input, &phases))
        .fold(anyhow::Ok(0), |acc, x| Ok(std::cmp::max(acc?, x?)))?;

    println!("Part 2: {part_2_result}");

    Ok(())
}
