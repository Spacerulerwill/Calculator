mod expr;
mod parser;
mod tokenizer;

use parser::{Parser, ParserError};
use std::env;
use tokenizer::{Tokenizer, TokenizerError};

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() == 0 {
        eprintln!("Calculator requires expression argument");
        return;
    }
    let expression = args.join("");
    let tokens = match Tokenizer::tokenize(&expression) {
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
    println!("{} = {}", expression, result);
}
