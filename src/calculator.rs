use std::{collections::VecDeque, clone};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Parenthesis {
    OPEN,
    CLOSED
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Number(u32),
    Op(Operator),
    Parenthesis(Parenthesis)
}

#[derive(Debug)]
pub enum Error {
    BadToken(char),
    MismatchedParenthesis,
}

pub fn tokenise<T: AsRef<str>>(expr: T) -> Result<Vec<Token>, Error> {
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
            ' ' | '\n' => {},
            _ => return Err(Error::BadToken(c))
        }
    }

    if parens.len() > 0 {
        return Err(Error::MismatchedParenthesis);
    }

    return Ok(tokens);
}

pub fn infix_to_rpn(tokens: &Vec<Token>) -> VecDeque<Token> {
    let mut queue: VecDeque<Token> = VecDeque::new();
    let mut operation_stack: Vec<Token> = Vec::new();
    
    for token in tokens {
        match token {
            Token::Number(_) => queue.push_back(token.clone()),
            Token::Op(_) => {
                while !operation_stack.is_empty() {
                    let next_op = operation_stack.last().unwrap().clone();
                    if next_op >= *token && next_op != Token::Parenthesis(Parenthesis::OPEN){
                        queue.push_back(operation_stack.pop().unwrap());
                    } else {
                        break;
                    }
                }
                operation_stack.push(token.clone());
            },
            Token::Parenthesis(Parenthesis::OPEN) => operation_stack.push(token.clone()),
            Token::Parenthesis(Parenthesis::CLOSED) => {
                while !operation_stack.is_empty() {
                    let next_op = operation_stack.last().unwrap().clone();
                    if next_op != Token::Parenthesis(Parenthesis::OPEN) {
                        queue.push_back(operation_stack.pop().unwrap());
                    } else {
                        break;
                    }
                }
                operation_stack.pop();
            },
        }
    }

    while operation_stack.len() > 0 {
        let op = operation_stack.pop().unwrap();
        if op != Token::Parenthesis(Parenthesis::OPEN){
            queue.push_back(op);
        }
    }

    return queue
}

pub fn evaluate_rpn(rpn: &mut VecDeque<Token>) -> u32 {
    let mut stack: Vec<u32> = Vec::new();
    while !rpn.is_empty() {
        let token = rpn.pop_front();
        match token.unwrap() {
            Token::Number(num) => stack.push(num),
            Token::Op(op) => {
                let y = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                match op {
                    Operator::Add => {
                        stack.push(x + y);
                    },
                    Operator::Sub => {
                        stack.push(x - y);
                    },
                    Operator::Mul => {
                        stack.push(x * y);
                    },
                    Operator::Div => {
                        stack.push(x / y);
                    },
                }
            },
            Token::Parenthesis(_) => panic!("There should be no parenthesis in expression evaluation stage!"),
        }
    }
    return *stack.first().unwrap();
}