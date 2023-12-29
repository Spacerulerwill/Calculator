mod calculator;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let res: String = args[1..].join(" ");    
    let tokens = calculator::Calculator::parse(res);

    match tokens {
        Ok(_) => {
            let mut rpn = calculator::Calculator::rpn(&tokens.unwrap());
            let result = calculator::Calculator::evaluate(&mut rpn);
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
