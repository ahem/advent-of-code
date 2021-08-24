extern crate pest;
#[macro_use]
extern crate pest_derive;

use crate::pest::Parser;
use std::fs;

#[derive(Parser)]
#[grammar = "./7/grammar.pest"] // relative to src
pub struct BitwiseLogicGateParser;

fn main() {
    let unparsed_input = fs::read_to_string("./src/7/input.txt").expect("cannot read input");
    let input = BitwiseLogicGateParser::parse(Rule::file, &unparsed_input)
        .expect("parse error")
        .next()
        .unwrap();

    for instruction in input.into_inner() {
        match instruction.as_rule() {
            Rule::instruction => {
                let mut inner_rules = instruction.into_inner();
                let source = inner_rules.next().unwrap();
                let target = inner_rules.next().unwrap();

                match source.as_rule() {
                    Rule::rshift_expr => {}
                    Rule::lshift_expr => unimplemented!("lshift_expr"),
                    Rule::or_expr => unimplemented!("or_expr"),
                    Rule::and_expr => unimplemented!("and_expr"),
                    Rule::not_expr => unimplemented!("not_expr"),
                    Rule::wire => unimplemented!("wire"),
                    Rule::number => unimplemented!("number"),
                    _ => unreachable!(),
                };

                println!("{:?} -> {:?}", source.as_rule(), target.as_str());
            }
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }
}
