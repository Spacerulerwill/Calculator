mod builtin_math;

use builtin_math::get_constants;
use clap::Parser as ClapParser;
use common::{parser::Parser, tokenizer::Tokenizer, variable::VariableMap};
use std::{
    fs,
    io::{self, Write},
};

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
            Ok(contents) => process_text(contents, &mut variables, args.tabsize),
            Err(err) => {
                eprintln!("Can't find file '{filepath}': {err}");
                return;
            }
        }
    }

    if let Some(expression) = args.expression {
        process_text(expression, &mut variables, args.tabsize);
    } else {
        start_repl(args.tabsize, &mut variables);
    }
}

fn process_text<'a>(mut expression: String, variables: &mut VariableMap<'a>, tabsize: u8) {
    if !expression.ends_with('\n') {
        expression.push('\n');
    }

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

    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to flush stdout");

        let mut input = String::new();

        if io::stdin().read_line(&mut input).is_err() {
            eprintln!("Failed to read input");
            continue;
        }

        if input.trim().eq_ignore_ascii_case("exit") {
            break;
        }

        process_text(input, variables, tabsize);
    }
}
