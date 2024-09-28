mod builtin_math;

use builtin_math::get_constants;
use clap::Parser as ClapParser;
use common::{parser::Parser, tokenizer::Tokenizer, variable::VariableMap};
use rustyline::{error::ReadlineError, DefaultEditor};
use std::fs;

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

    #[arg(short, long, value_name = "FILE")]
    file: Option<String>,
}

fn main() {
    let args = Args::parse();
    let mut variables = get_constants();

    if let Some(filepath) = args.file {
        match fs::read_to_string(&filepath) {
            Ok(contents) => process_text(contents + "\n", &mut variables, args.tabsize),
            Err(err) => {
                eprintln!("Can't find file '{filepath}': {err}");
                return;
            }
        }
    }

    if let Some(expression) = args.expression {
        process_text(expression + "\n", &mut variables, args.tabsize);
    } else {
        start_repl(args.tabsize, &mut variables);
    }
}

fn process_text<'a>(expression: String, variables: &mut VariableMap<'a>, tabsize: u8) {
    let tokens = match Tokenizer::tokenize(&expression, tabsize) {
        Ok(tokenizer) => tokenizer.tokens,
        Err(err) => {
            println!("{}", &err);
            return;
        }
    };

    let statements = match Parser::parse(tokens) {
        Ok(expr) => expr,
        Err(err) => {
            println!("{}", &err);
            return;
        }
    };

    for statement in statements {
        if let Err(err) = statement.interpret(variables) {
            println!("{err}")
        }
    }
}

fn start_repl<'a>(tabsize: u8, variables: &mut VariableMap<'a>) {
    println!("Enter mathematical expressions to evaluate. Type 'exit' to quit.");

    let mut rl = DefaultEditor::new().expect("Failed to create readline");

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                if line.trim().eq_ignore_ascii_case("exit") {
                    break;
                }
                rl.add_history_entry(line.clone())
                    .expect("Failed to add to history");
                process_text(line + "\n", variables, tabsize);
            }
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    println!("Exiting, Goodbye!")
}
