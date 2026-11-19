use anyhow::anyhow;
use std::collections::HashMap;

fn parse_line(s: &str) -> anyhow::Result<(String, String)> {
    let mut x = s.split(")");
    let object = x.next().ok_or(anyhow!("invalid input"))?.to_owned();
    let parent = x.next().ok_or(anyhow!("invalid input"))?.to_owned();
    Ok((parent, object))
}

fn orbits<'a, 'b>(
    orbit_map: &'a HashMap<String, String>,
    object: &'b str,
) -> impl Iterator<Item = &'a str>
where
    'b: 'a,
{
    std::iter::successors(Some(object), |object| Some(orbit_map.get(*object)?))
}

fn main() -> anyhow::Result<()> {
    let orbit_map = std::io::stdin()
        .lines()
        .map(|s| parse_line(&s?))
        .collect::<Result<HashMap<String, String>, _>>()?;

    let total_orbits: usize = orbit_map
        .keys()
        .map(|object| orbits(&orbit_map, object).count())
        .sum();

    println!("part 1: {total_orbits}");

    let orbit_chain = orbits(&orbit_map, &orbit_map["YOU"]).collect::<Vec<_>>();

    for (i, object) in orbits(&orbit_map, &orbit_map["SAN"]).enumerate() {
        if let Some(steps) = orbit_chain.iter().position(|x| object == *x) {
            println!("Part 2: {}", i + steps);
            break;
        }
    }

    Ok(())
}
