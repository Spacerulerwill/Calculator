mod expr;
mod parser;
mod tokenizer;

use clap::Parser as ClapParser;
use parser::{Parser, ParserError};
use std::io::{self, Write};
use tokenizer::{Tokenizer, TokenizerError};

const DEFAULT_TAB_SIZE: u8 = 4;
pub const DEFAULT_PRECISION: u32 = 64;

#[derive(ClapParser)]
#[command(name = "calculator")]
#[command(version = "0.0")]
#[command(about = "Evaluate a mathematical expression", long_about = None)]
struct Args {
    #[arg()]
    expression: Option<String>,

    #[arg(short, long, default_value_t = DEFAULT_TAB_SIZE)]
    tabsize: u8,

    #[arg(short, long, default_value_t = DEFAULT_PRECISION)]
    precision: u32,
}

fn main() {
    let args = Args::parse();

    if let Some(expression) = args.expression {
        process_expression(&expression.trim(), args.tabsize, args.precision);
    } else {
        start_repl(args.tabsize, args.precision);
    }
}

fn process_expression(expression: &str, tabsize: u8, precision: u32) {
    let tokens = match Tokenizer::tokenize(expression, tabsize, precision) {
        Ok(tokenizer) => tokenizer.tokens,
        Err(err) => {
            match err {
                TokenizerError::BadChar(char, char_pos) => {
                    eprintln!("Position {char_pos} :: Invalid character '{char}'");
                }
            }
            return;
        }
    };

    let expr = match Parser::parse(tokens, precision) {
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

    let result = match expr.evaluate(precision) {
        Ok(result) => result,
        Err(err) => {
            eprintln!("Column {} :: {}", err.col, err.message);
            return;
        }
    };

    if result.imag().is_zero() {
        println!("{} = {}", expression, &result.real());
    } else {
        println!("{} = {} + {}i", expression, &result.real(), &result.imag());
    }
}

fn start_repl(tabsize: u8, precision: u32) {
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

        process_expression(trimmed_input, tabsize, precision);
    }
}
