extern crate pest;
#[macro_use]
extern crate pest_derive;

use crate::pest::Parser;
use std::collections::HashMap;
use std::fs;

#[derive(Parser)]
#[grammar = "./7/grammar.pest"] // relative to src
pub struct BitwiseLogicGateParser;

#[derive(Debug, Clone)]
enum Value {
    Identifier(String),
    Number(u16),
}

impl Value {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Value {
        assert!(pair.as_rule() == Rule::value);
        let token = pair.into_inner().next().unwrap();
        match token.as_rule() {
            Rule::ident => Value::Identifier(token.as_str().to_owned()),
            Rule::number => Value::Number(token.as_str().parse::<u16>().unwrap()),
            _ => unreachable!(),
        }
    }
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

impl Expression {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Expression {
        assert!(pair.as_rule() == Rule::expr);
        let inner = pair.into_inner().next().unwrap();
        return match inner.as_rule() {
            Rule::unary_expr => {
                let mut tokens = inner.into_inner();
                let operator = tokens.next().unwrap().as_str();
                let value = Value::parse(tokens.next().unwrap());
                assert!(operator == "NOT");
                return Expression::Not(value);
            }
            Rule::binary_expr => {
                let mut tokens = inner.into_inner();
                let left = Value::parse(tokens.next().unwrap());
                let operator = tokens.next().unwrap().as_str();
                let right = Value::parse(tokens.next().unwrap());
                return match operator {
                    "OR" => Expression::Or(left, right),
                    "AND" => Expression::And(left, right),
                    "LSHIFT" => Expression::LShift(left, right),
                    "RSHIFT" => Expression::RShift(left, right),
                    _ => unreachable!("invalid operator!"),
                };
            }
            Rule::value => Value::parse(inner).into(),
            _ => unreachable!("unexpected expression type"),
        };
    }
}

#[derive(Debug)]
struct Statements(HashMap<String, Expression>);

impl Statements {
    fn parse(pair: pest::iterators::Pair<Rule>) -> Statements {
        let mut statements: HashMap<String, Expression> = HashMap::new();

        for statement in pair.into_inner() {
            match statement.as_rule() {
                Rule::statement => {
                    let mut tokens = statement.into_inner();
                    let expr = Expression::parse(tokens.next().unwrap());
                    let identifier = tokens.next().unwrap().as_str().to_owned();
                    statements.insert(identifier, expr);
                }
                Rule::EOI => (),
                _ => unreachable!(),
            }
        }

        return Statements(statements);
    }

    fn eval_id(&self, id: &str, cache: &mut HashMap<String, u16>) -> u16 {
        let Statements(rules) = self;
        let expr = rules.get(id).unwrap().to_owned();
        return self.eval(expr, cache);
    }

    fn eval(&self, expr: Expression, cache: &mut HashMap<String, u16>) -> u16 {
        match expr {
            Expression::Number(x) => return x,
            Expression::Identifier(id) => match cache.get(&id) {
                Some(x) => *x,
                None => {
                    let Statements(rules) = self;
                    let expr = rules.get(&id).unwrap().to_owned();
                    let x = self.eval(expr, cache);
                    cache.insert(id, x);
                    x
                }
            },
            Expression::LShift(left, right) => {
                self.eval(left.into(), cache) << self.eval(right.into(), cache)
            }
            Expression::RShift(left, right) => {
                self.eval(left.into(), cache) >> self.eval(right.into(), cache)
            }
            Expression::And(left, right) => {
                self.eval(left.into(), cache) & self.eval(right.into(), cache)
            }
            Expression::Or(left, right) => {
                self.eval(left.into(), cache) | self.eval(right.into(), cache)
            }
            Expression::Not(val) => !self.eval(val.into(), cache),
        }
    }
}

fn main() {
    let unparsed_input = fs::read_to_string("./src/7/input.txt").expect("cannot read input");
    let input = BitwiseLogicGateParser::parse(Rule::file, &unparsed_input)
        .expect("parse error")
        .next()
        .unwrap();

    let statements = Statements::parse(input);
    let mut cache: HashMap<String, u16> = HashMap::new();
    let result = statements.eval_id("a", &mut cache);
    println!("result part 1: {}", result);

    let mut cache: HashMap<String, u16> = HashMap::new();
    cache.insert("b".to_owned(), result);
    let result = statements.eval_id("a", &mut cache);
    println!("result part 2: {}", result);
}
