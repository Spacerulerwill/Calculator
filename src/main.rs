mod expr;
mod function;
mod parser;
mod tokenizer;
mod value;
mod variable;

use clap::Parser as ClapParser;
use function::BUILTIN_SIN;
use num_complex::Complex64;
use parser::{Parser, ParserError};
use std::{
    collections::HashMap,
    f64::consts::{E, PI, TAU},
    io::{self, Write},
};
use tokenizer::{Tokenizer, TokenizerError};
use value::Value;
use variable::Variable;

const DEFAULT_TAB_SIZE: u8 = 4;

#[derive(ClapParser)]
#[command(name = "calculator")]
#[command(version = "0.0")]
#[command(about = "Evaluate a mathematical expression", long_about = None)]
struct Args {
    #[arg()]
    expression: Option<String>,

    #[arg(short, long, default_value_t = DEFAULT_TAB_SIZE)]
    tabsize: u8,
}

fn main() {
    let args = Args::parse();

    let variables = HashMap::from([
        (
            "i",
            Variable {
                constant: true,
                value: Value::Number(Complex64::new(0.0, 1.0)),
            },
        ),
        (
            "e",
            Variable {
                constant: true,
                value: Value::Number(Complex64::from(E)),
            },
        ),
        (
            "pi",
            Variable {
                constant: true,
                value: Value::Number(Complex64::from(PI)),
            },
        ),
        (
            "tau",
            Variable {
                constant: true,
                value: Value::Number(Complex64::from(TAU)),
            },
        ),
        (
            "sin",
            Variable {
                constant: true,
                value: BUILTIN_SIN,
            },
        ),
    ]);

    if let Some(expression) = args.expression {
        process_expression(&expression.trim(), &variables, args.tabsize);
    } else {
        start_repl(args.tabsize, &variables);
    }
}

fn process_expression(expression: &str, variables: &HashMap<&str, Variable>, tabsize: u8) {
    let tokens = match Tokenizer::tokenize(expression, tabsize) {
        Ok(tokenizer) => tokenizer.tokens,
        Err(err) => {
            match err {
                TokenizerError::BadChar(char, char_pos) => {
                    eprintln!("Position {char_pos} :: Unexpected or invalid character: '{char}'");
                }
            }
            return;
        }
    };

    let expr = match Parser::parse(tokens) {
        Ok(expr) => expr,
        Err(err) => {
            match err {
                ParserError::ExpectedExpression { found } => {
                    if let Some(found) = found {
                        eprintln!(
                            "Position {} :: Expected expression next but found '{}'",
                            found.col,
                            found.kind.get_lexeme()
                        );
                    } else {
                        eprintln!("Expected expression next but found EOF");
                    }
                }
                ParserError::ExpectedToken { expected, found } => {
                    if let Some(found) = found {
                        eprintln!(
                            "Column {} :: Expected {:?} but found '{}'",
                            found.col,
                            expected,
                            found.kind.get_lexeme()
                        );
                    } else {
                        eprintln!("Expected {} but found EOF", expected.get_lexeme());
                    }
                }
                ParserError::ExpectedEOF { found } => {
                    eprintln!("Expected EOF but found '{}'", found.kind.get_lexeme())
                }
            }
            return;
        }
    };

    let result = match expr.evaluate(variables) {
        Ok(result) => result,
        Err(err) => {
            eprintln!("Column {} :: {}", err.col, err.message);
            return;
        }
    };

    println!("{}", &result);
}

fn start_repl(tabsize: u8, variables: &HashMap<&str, Variable>) {
    let mut input = String::new();

    println!("Enter mathematical expressions to evaluate. Type 'exit' to quit.");

    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to flush stdout");

        input.clear();
        if io::stdin().read_line(&mut input).is_err() {
            eprintln!("Failed to read input");
            continue;
        }

        let trimmed_input = input.trim();
        if trimmed_input.eq_ignore_ascii_case("exit") {
            break;
        }

        process_expression(trimmed_input, variables, tabsize);
    }
}
