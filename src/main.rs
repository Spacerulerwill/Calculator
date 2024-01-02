mod calculator;
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
            if let Some(c) = input.chars().next() {
                if c == 'q' {
                    break;
                }
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

fn calculate(input: String) {
    let tokens = calculator::tokenise(input);
    dbg!(&tokens);
    match tokens {
        Ok(_) => {
            let mut rpn = calculator::infix_to_rpn(&tokens.unwrap());
            let result: Result<calculator::Signed, calculator::CalculatonError> = calculator::evaluate_rpn(&mut rpn);

            match result {
                Ok(_) => println!("Result: {}", result.unwrap()),
                Err(e) => {
                    match e {
                        calculator::CalculatonError::UndefinedOperation(msg) => println!("Undefined operation: {}", msg),
                    }
                },
            }

        },
        Err(e) => {
            match e {
                calculator::ParserError::BadToken(c) => println!("Bad token: {}", c),
                calculator::ParserError::MismatchedParenthesis => println!("Mismatched parenthesis found."),
            }
        } 
    }
}
