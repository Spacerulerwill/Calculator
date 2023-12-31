mod calculator;
use std::io::stdin;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let res: String = args[1..].join(" ");    

    if res.len() == 0 {
        loop {
            let mut input: String = String::new();
            stdin().read_line(&mut input).expect("Failed to read user input");
            calculate(input);
        }
    }
    else {
        calculate(res);
    }
}

fn calculate(input: String) {
    let tokens = calculator::tokenise(input);
    match tokens {
        Ok(_) => {
            let mut rpn = calculator::infix_to_rpn(&tokens.unwrap());
            let result: Result<u32, calculator::CalculatonError> = calculator::evaluate_rpn(&mut rpn);

            match result {
                Ok(_) => println!("{}", result.unwrap()),
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
