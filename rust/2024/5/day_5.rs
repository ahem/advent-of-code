use anyhow::anyhow;
use itertools::Itertools;
use regex::Regex;
use std::{
    cell::LazyCell,
    io::{stdin, Read},
    str::FromStr,
};

#[derive(Debug, Clone)]
struct PageOrderingRule {
    page: u32,
    must_be_before: u32,
}

const PAGE_ORDERING_RULE_REGEX: LazyCell<Regex> =
    LazyCell::new(|| Regex::new(r"^(\d+)\|(\d+)$").unwrap());

impl FromStr for PageOrderingRule {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match PAGE_ORDERING_RULE_REGEX.captures(s) {
            Some(captures) => Ok(Self {
                page: captures[1].parse::<u32>()?,
                must_be_before: captures[2].parse::<u32>()?,
            }),
            None => Err(anyhow::anyhow!("error parsing: {s}")),
        }
    }
}

#[derive(Debug)]
struct PageUpdates {
    pages: Vec<u32>,
}

impl FromStr for PageUpdates {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pages = s
            .split(",")
            .map(|n| n.parse::<u32>())
            .collect::<Result<Vec<u32>, _>>()?;

        Ok(Self { pages })
    }
}

impl PageUpdates {
    fn is_correct<'a>(&'a self, rules: impl IntoIterator<Item = &'a PageOrderingRule>) -> bool {
        for rule in rules.into_iter() {
            if !(self.pages.contains(&rule.page) && self.pages.contains(&rule.must_be_before)) {
                continue;
            }
            let page_pos = self.pages.iter().position(|x| *x == rule.page);
            let must_be_before_pos = self.pages.iter().position(|x| *x == rule.must_be_before);
            if page_pos > must_be_before_pos {
                return false;
            }
        }
        true
    }

    fn middle_page(&self) -> u32 {
        self.pages[self.pages.len() / 2]
    }

    fn sort<'a>(&'a self, rules: impl IntoIterator<Item = &'a PageOrderingRule>) -> Self {
        let mut pages = self.pages.clone();
        for r in rules.into_iter() {
            if !(pages.contains(&r.page) && pages.contains(&r.must_be_before)) {
                continue;
            }
            let page_pos = pages.iter().position(|x| *x == r.page).unwrap();
            let must_be_before_pos = pages.iter().position(|x| *x == r.must_be_before).unwrap();
            if page_pos > must_be_before_pos {
                pages.remove(page_pos);
                pages.insert(must_be_before_pos, r.page);
            }
        }
        Self { pages }
    }
}

fn read_input() -> anyhow::Result<(Vec<PageOrderingRule>, Vec<PageUpdates>)> {
    let mut buf = String::new();
    stdin().read_to_string(&mut buf)?;
    let mut bits = buf.split("\n\n");
    let page_ordering_rules = bits
        .next()
        .ok_or(anyhow!("parse error"))?
        .split_whitespace()
        .map(|s| s.parse::<PageOrderingRule>())
        .collect::<Result<Vec<PageOrderingRule>, _>>()?;

    let page_updates = bits
        .next()
        .ok_or(anyhow!("parse error"))?
        .split_whitespace()
        .map(|s| s.parse::<PageUpdates>())
        .collect::<Result<Vec<PageUpdates>, _>>()?;

    Ok((page_ordering_rules, page_updates))
}

fn main() -> anyhow::Result<()> {
    let (rules, page_updates) = read_input()?;

    let (correct, incorrect): (Vec<_>, Vec<_>) =
        page_updates.iter().partition(|x| x.is_correct(&rules));

    let part_1: u32 = correct.iter().map(|x| x.middle_page()).sum();
    println!("part 1: {part_1}",);

    let rules = {
        let mut t = rules.clone();
        t.sort_by_key(|x| (x.page, x.must_be_before));
        t
    };

    let corrected = incorrect.iter().map(|x| x.sort(&rules)).collect_vec();
    assert!(corrected.iter().all(|x| x.is_correct(&rules)));

    let part_2: u32 = corrected.iter().map(|x| x.middle_page()).sum();
    println!("part 2: {part_2}",);

    Ok(())
}
