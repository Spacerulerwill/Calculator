#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Parenthesis {
    OPEN,
    CLOSED
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Number(u32),
    Op(Operator),
    Parenthesis(Parenthesis)
}

pub struct Calculator {}

#[derive(Debug)]
pub enum Error {
    BadToken(char),
    MismatchedParenthesis,
}

impl Calculator {
    pub fn parse<T: AsRef<str>>(expr: T) -> Result<Vec<Token>, Error> {
        let expr = expr.as_ref();
        let chars = expr.chars();
        let mut tokens: Vec<Token> = Vec::new();
        let mut parens: Vec<Parenthesis> = Vec::new();
        for c in chars {
            match c {
                '0'..='9' => match tokens.last_mut() {
                    Some(Token::Number(n)) => {
                        *n = *n * 10 + (c as u32 - 48);
                    }
                    _ => {
                        let digit = c as u32 - 48;
                        tokens.push(Token::Number(digit));
                    }
                }
                '(' => {
                    tokens.push(Token::Parenthesis(Parenthesis::OPEN));
                    parens.push(Parenthesis::OPEN)
                },
                ')' => {
                    if let Some(p) = parens.pop() {
                        if p != Parenthesis::OPEN {
                            return Err(Error::MismatchedParenthesis);
                        }
                    } else {
                        return Err(Error::MismatchedParenthesis);
                    }

                    tokens.push(Token::Parenthesis(Parenthesis::CLOSED));
                },
                '+' => tokens.push(Token::Op(Operator::Add)),
                '-' => tokens.push(Token::Op(Operator::Sub)),
                '*' => tokens.push(Token::Op(Operator::Mul)),
                '/' => tokens.push(Token::Op(Operator::Div)),
                ' ' => {},
                '\n' => {}
                _ => return Err(Error::BadToken(c))
            }
        }

        if parens.len() > 0 {
            return Err(Error::MismatchedParenthesis);
        }

        return Ok(tokens);
    }
}