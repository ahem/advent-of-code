use anyhow::anyhow;
use itertools::Itertools;
use std::{collections::HashSet, hash::Hash};

fn parse_line(s: &str) -> anyhow::Result<(i64, i64, i64)> {
    let s = s.strip_prefix("<").ok_or(anyhow!("parse error"))?;
    let s = s.strip_suffix(">").ok_or(anyhow!("parse error"))?;
    let values = s
        .split(", ")
        .flat_map(|s| s.split('=').nth(1))
        .map(|s| i64::from_str_radix(s, 10))
        .collect::<Result<Vec<i64>, _>>()?;
    Ok((values[0], values[1], values[2]))
}

#[derive(Clone)]
struct Moon {
    pos: (i64, i64, i64),
    velocity: (i64, i64, i64),
}

impl std::fmt::Display for Moon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "pos=<x={}, y={}, z={}>, vel=<x={}, y={}, z={}>",
            self.pos.0, self.pos.1, self.pos.2, self.velocity.0, self.velocity.1, self.velocity.2,
        )
    }
}

impl Moon {
    fn apply_gravity(&self, moons: &[Moon]) -> Moon {
        let mut velocity = self.velocity;
        for other in moons {
            velocity.0 += (other.pos.0 - self.pos.0).signum();
            velocity.1 += (other.pos.1 - self.pos.1).signum();
            velocity.2 += (other.pos.2 - self.pos.2).signum();
        }
        Moon { velocity, ..*self }
    }

    fn apply_velocity(&self) -> Moon {
        let pos = (
            self.pos.0 + self.velocity.0,
            self.pos.1 + self.velocity.1,
            self.pos.2 + self.velocity.2,
        );
        Moon { pos, ..*self }
    }

    fn pot(&self) -> u64 {
        u64::try_from(self.pos.0.abs() + self.pos.1.abs() + self.pos.2.abs())
            .expect("no integer overflow")
    }

    fn kin(&self) -> u64 {
        u64::try_from(self.velocity.0.abs() + self.velocity.1.abs() + self.velocity.2.abs())
            .expect("no integer overflow")
    }

    fn energy(&self) -> u64 {
        self.pot() * self.kin()
    }
}

fn step(moons: &[Moon]) -> Vec<Moon> {
    moons
        .iter()
        .map(|m| m.apply_gravity(&moons))
        .map(|m| m.apply_velocity())
        .collect()
}

fn steps(moons: &[Moon]) -> impl Iterator<Item = Vec<Moon>> {
    let initial = moons.to_vec();
    std::iter::successors(Some(initial), |moons| Some(step(moons)))
}

fn series_length<T: Eq + Hash>(moons: &[Moon], f: fn(&Moon) -> T) -> u32 {
    let mut set = HashSet::<Vec<T>>::new();

    for state in steps(moons) {
        let key: Vec<_> = state.into_iter().map(|x| f(&x)).collect();
        if set.contains(&key) {
            return set.len() as u32;
        }
        set.insert(key);
    }

    unreachable!()
}

fn is_prime(n: u32) -> bool {
    !(2..=n.isqrt()).any(|x| n % x == 0)
}

fn primes() -> impl Iterator<Item = u32> {
    std::iter::successors(Some(2), |n| Some(n + 1)).filter(|n| is_prime(*n))
}

fn prime_factors(n: u32) -> impl Iterator<Item = (u32, u32)> {
    primes()
        .take_while(move |x| *x < n)
        .filter(move |x| n % x == 0)
        .map(move |x| {
            let factor = (1..n).take_while(|p| n % x.pow(*p) == 0).last().unwrap();
            (x, factor)
        })
}

// find smallest common multiple with the prime factorization method. Takes the prime factors
// of all inputs, takes the highest factor for each prime and returns the multiple of those.
fn least_common_multiple(nums: &[u32]) -> u64 {
    nums.iter()
        .flat_map(|x| prime_factors(*x))
        .sorted_by_key(|x| x.0)
        .chunk_by(|x| x.0)
        .into_iter()
        .map(|(_, chunk)| chunk.sorted_by_key(|x| x.1).last().unwrap())
        .fold(1u64, |acc, (x, p)| acc * (x as u64).pow(p))
}

fn main() -> anyhow::Result<()> {
    let input = std::io::stdin()
        .lines()
        .map(|s| match s {
            Ok(s) => parse_line(&s),
            Err(err) => Err(anyhow!("{err}")),
        })
        .collect::<Result<Vec<_>, _>>()?;

    let moons: Vec<Moon> = input
        .iter()
        .map(|(x, y, z)| Moon {
            pos: (*x, *y, *z),
            velocity: (0, 0, 0),
        })
        .collect();

    let energy: u64 = steps(&moons)
        .nth(1000)
        .unwrap()
        .iter()
        .map(|m| m.energy())
        .sum();

    println!("Part 1: {energy}");

    let x_len = series_length(&moons, |m| (m.pos.0, m.velocity.0));
    let y_len = series_length(&moons, |m| (m.pos.1, m.velocity.1));
    let z_len = series_length(&moons, |m| (m.pos.2, m.velocity.2));

    println!("{x_len} {:?}", prime_factors(x_len).collect::<Vec<_>>());
    println!("{y_len} {:?}", prime_factors(y_len).collect::<Vec<_>>());
    println!("{z_len} {:?}", prime_factors(z_len).collect::<Vec<_>>());

    println!("part 2: {}", least_common_multiple(&[x_len, y_len, z_len]));

    Ok(())
}
