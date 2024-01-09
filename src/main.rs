mod calculator;
use calculator::tokeniser::{tokenise, TokeniserError};
use calculator::rpn::infix_to_rpn;
use calculator::evaluate::{evaluate_rpn, EvaluationError};
use std::io::stdin;
use std::io::Write;

fn main() {
    println!("Input 'q' to quit");
    let args = std::env::args();

    if args.len() == 1 {
        loop {
            let mut input: String = String::new();
            print!("Input expression: ");
            std::io::stdout().flush().expect("Failed to flush stdout buffer");
            stdin().read_line(&mut input).expect("Failed to read user input");
            if input.trim() == "q" {
                break;
            }
            calculate(input);
        }
    }
    else {
        let collected_args: Vec<String> = args.collect();
        let res: String = collected_args[1..].join(" ");    
        calculate(res);
    }
}

fn calculate(mut input: String) {
    let tokens = tokenise(&mut input);
    match tokens {
        Ok(_) => {
            let mut rpn = infix_to_rpn(&tokens.unwrap());
            let result = evaluate_rpn(&mut rpn);
            match result {
                Ok(_) => println!("Result: {}", result.unwrap()),
                Err(e) => {
                    match e {
                        EvaluationError::UndefinedOperation(msg) => println!("Undefined operation: {}", msg),
                        EvaluationError::IntegerOverflow => println!("Overflow occured whilst performing operations"),
                        EvaluationError::InvalidFunctionDomain(function, domain) => println!("The domain for the function \"{}\" is {}", function, domain),
                    }
                }
            }
        },
        Err(e) => {
            match e {
                TokeniserError::BadToken(c) => println!("Bad token: {}", c),
                TokeniserError::MismatchedParenthesis => println!("Mismatched parenthesis found."),
                TokeniserError::InvalidConsecutiveTokens(c1, c2) => println!("Invalid consecutive tokens {} and {}", c1, c2),
                TokeniserError::InvalidNumberOfOperands(c1, count) => println!("Operator {} requires {} operands", c1, count),
                TokeniserError::InvalidFunctionOrConstant(function) => println!("The function or constant \"{}\" does not exist!", function),
            }
        } 
    }
}