extern crate pest;
#[macro_use]
extern crate pest_derive;

use crate::pest::Parser;
use std::fs;

#[derive(Parser)]
#[grammar = "./7/grammar.pest"] // relative to src
pub struct BitwiseLogicGateParser;

#[derive(Debug, Clone)]
enum Value {
    Identifier(String),
    Number(u16),
}

impl Into<Expression> for Value {
    fn into(self) -> Expression {
        match self {
            Value::Identifier(i) => Expression::Identifier(i),
            Value::Number(n) => Expression::Number(n),
        }
    }
}

#[derive(Debug, Clone)]
enum Expression {
    And(Value, Value),
    Or(Value, Value),
    LShift(Value, Value),
    RShift(Value, Value),
    Not(Value),
    Identifier(String),
    Number(u16),
}

#[derive(Debug)]
struct Statements(std::collections::HashMap<String, Expression>);

fn parse_value(pair: pest::iterators::Pair<Rule>) -> Value {
    assert!(pair.as_rule() == Rule::value);
    let token = pair.into_inner().next().unwrap();
    match token.as_rule() {
        Rule::ident => Value::Identifier(token.as_str().to_owned()),
        Rule::number => Value::Number(token.as_str().parse::<u16>().unwrap()),
        _ => unreachable!(),
    }
}

fn parse_unary(pair: pest::iterators::Pair<Rule>) -> Expression {
    assert!(pair.as_rule() == Rule::unary_expr);
    let mut tokens = pair.into_inner();
    let operator = tokens.next().unwrap().as_str();
    let value = parse_value(tokens.next().unwrap());
    assert!(operator == "NOT");
    return Expression::Not(value);
}

fn parse_binary(pair: pest::iterators::Pair<Rule>) -> Expression {
    assert!(pair.as_rule() == Rule::binary_expr);
    let mut tokens = pair.into_inner();
    let left = parse_value(tokens.next().unwrap());
    let operator = tokens.next().unwrap().as_str();
    let right = parse_value(tokens.next().unwrap());
    return match operator {
        "OR" => Expression::Or(left, right),
        "AND" => Expression::And(left, right),
        "LSHIFT" => Expression::LShift(left, right),
        "RSHIFT" => Expression::RShift(left, right),
        _ => unreachable!("invalid operator!"),
    };
}

fn parse_expression(pair: pest::iterators::Pair<Rule>) -> Expression {
    assert!(pair.as_rule() == Rule::expr);
    let inner = pair.into_inner().next().unwrap();
    return match inner.as_rule() {
        Rule::unary_expr => parse_unary(inner),
        Rule::binary_expr => parse_binary(inner),
        Rule::value => parse_value(inner).into(),
        _ => unreachable!("unexpected expression type"),
    };
}

fn eval_expr(rules: &std::collections::HashMap<String, Expression>, expr: Expression) -> u16 {
    match expr {
        Expression::Number(x) => return x,
        Expression::Identifier(id) => {
            println!("{}", id);
            return eval_expr(&rules, rules.get(&id).unwrap().to_owned());
        }
        Expression::LShift(left, right) => {
            eval_expr(rules, left.into()) << eval_expr(rules, right.into())
        }
        Expression::RShift(left, right) => {
            eval_expr(rules, left.into()) >> eval_expr(rules, right.into())
        }
        Expression::And(left, right) => {
            eval_expr(rules, left.into()) & eval_expr(rules, right.into())
        }
        Expression::Or(left, right) => {
            eval_expr(rules, left.into()) | eval_expr(rules, right.into())
        }
        Expression::Not(val) => !eval_expr(rules, val.into()),
    }
}

fn main() {
    let unparsed_input = fs::read_to_string("./src/7/input.txt").expect("cannot read input");
    let input = BitwiseLogicGateParser::parse(Rule::file, &unparsed_input)
        .expect("parse error")
        .next()
        .unwrap();

    let mut statements: std::collections::HashMap<String, Expression> =
        std::collections::HashMap::new();

    for statement in input.into_inner() {
        match statement.as_rule() {
            Rule::statement => {
                let mut tokens = statement.into_inner();
                let expr = parse_expression(tokens.next().unwrap());
                let identifier = tokens.next().unwrap().as_str().to_owned();
                statements.insert(identifier, expr);
            }
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }
    let result = eval_expr(&statements, statements.get("a").unwrap().to_owned());
    println!("result part 1: {}", result);
}
