mod calculator;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let res: String = args[1..].join(" ");    
    let tokens = calculator::parse_and_tokenise(res);

    match tokens {
        Ok(_) => {
            let mut rpn = calculator::infix_to_rpn(&tokens.unwrap());
            let result = calculator::evaluate_rpn(&mut rpn);
            println!("{}", result);
        },
        Err(e) => {
            match e {
                calculator::Error::BadToken(c) => println!("Bad token: {}", c),
                calculator::Error::MismatchedParenthesis => println!("Mismatched parenthesis found."),
            }
        } 
    }
}
