mod expr;
mod parser;
mod tokenizer;

use parser::{Parser, ParserError};
use clap::Parser as ClapParser;
use tokenizer::{Tokenizer, TokenizerError};


const DEFAULT_TAB_SIZE: u8 = 4;
pub const DEFAULT_PRECISION: u32 = 64;

#[derive(ClapParser)]
#[command(name = "calculator")]
#[command(version = "0.0")]
#[command(about = "Evaluate a mathematical expression", long_about = None)]
struct Args {
    #[arg()]
    expression: String,

    #[arg(short, long, default_value_t = DEFAULT_TAB_SIZE)]
    tabsize: u8,

    #[arg(short, long, default_value_t = DEFAULT_PRECISION)]
    precision: u32
}

fn main() {
    let args = Args::parse();
    let tokens = match Tokenizer::tokenize(&args.expression, args.tabsize, args.precision) {
        Ok(tokenizer) => tokenizer.tokens,
        Err(err) => {
            match err {
                TokenizerError::BadChar(char, char_pos) => {
                    eprintln!("Position {char_pos} :: Invalid character '{char}'")
                }
            }
            return;
        }
    };

    let result = match Parser::parse(tokens) {
        Ok(expr) => expr.evaluate(),
        Err(err) => {
            match err {
                ParserError::ExpectedExpression { found } => {
                    if let Some(found) = found {
                        eprintln!(
                            "Position {} :: Expected expression but found '{}'",
                            found.col,
                            found.kind.get_lexeme()
                        )
                    } else {
                        eprintln!("Expected expression but found EOF")
                    }
                }
                ParserError::ExpectedToken { expected, found } => {
                    if let Some(found) = found {
                        eprintln!(
                            "Position {} :: Expected {:?} but found '{}'",
                            found.col,
                            expected,
                            found.kind.get_lexeme()
                        )
                    } else {
                        eprintln!("Expected {} but found EOF", expected.get_lexeme())
                    }
                }
            }
            return;
        }
    };
    println!("{} = {}", &args.expression, result);
}
