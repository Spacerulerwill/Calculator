mod builtin_math;

use builtin_math::get_constants;
use clap::Parser as ClapParser;
use common::{
    parser::Parser,
    tokenizer::Tokenizer,
    variable::VariableMap,
};
use std::io::{self, Write};

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
    let mut variables = get_constants();
    if let Some(expression) = args.expression {
        process_expression(&expression.trim(), &mut variables, args.tabsize);
    } else {
        start_repl(args.tabsize, &mut variables);
    }
}

fn process_expression<'a>(
    expression: &str,
    variables: &mut VariableMap<'a>,
    tabsize: u8,
) {
    let tokens = match Tokenizer::tokenize(expression, tabsize) {
        Ok(tokenizer) => tokenizer.tokens,
        Err(err) => {
            println!("{}", &err);
            return
        }
    };

    let statement = match Parser::parse(tokens) {
        Ok(expr) => expr,
        Err(err) => {
            println!("{}", &err);
            return
        }
    };

    if let Err(err) = statement.interpret(variables) {
        println!("{err}")
    }
}

fn start_repl<'a>(tabsize: u8, variables: &mut VariableMap<'a>) {
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
