mod expr;
mod parser;
mod tokenizer;
mod variable;

use clap::Parser as ClapParser;
use num_complex::{Complex, Complex64};
use parser::{Parser, ParserError};
use variable::Variable;
use std::{collections::HashMap, f64::consts::{E, PI}, io::{self, Write}};
use tokenizer::{Tokenizer, TokenizerError};

const DEFAULT_TAB_SIZE: u8 = 4;

#[derive(ClapParser)]
#[command(name = "calculator")]
#[command(version = "0.0")]
#[command(about = "Evaluate a mathematical expression", long_about = None)]
struct Args {
    #[arg()]
    expression: Option<String>,

    #[arg(short, long, default_value_t = DEFAULT_TAB_SIZE)]
    tabsize: u8
}

fn main() {
    let args = Args::parse();

    let variables = HashMap::from([
        ("e", Variable {constant: true, value: Complex64::new(E, 0.0)}),
        ("pi", Variable {constant: true, value: Complex::new(PI, 0.0)})
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
                    eprintln!("Position {char_pos} :: Unexpected or invalid character '{char}'");
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

    if result.im == 0.0 {
        println!("{} = {}", expression, &result.re);
    } else if result.re == 0.0 {
        println!("{} = {}i", expression, &result.im);
    }
    else {
        println!("{} = {} + {}i", expression, &result.re, &result.im);
    }
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
