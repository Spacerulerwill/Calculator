use std::{collections::VecDeque, fmt::Binary};

pub type Signed = i128;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOp {
    Negate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Parenthesis {
    OPEN,
    CLOSED
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Number(Signed),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Parenthesis(Parenthesis)
}

#[derive(Debug)]
pub enum ParserError {
    BadToken(char),
    MismatchedParenthesis,
}

#[derive(Debug)]
pub enum CalculatonError {
    UndefinedOperation(String),
    IntegerOverflow
}

fn get_binary_operator_precedence(op: BinaryOp) -> Signed {
    return match op {
        BinaryOp::Add | BinaryOp::Sub => 0,
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 1,
        BinaryOp::Exp  => 2,
    }
}

pub fn tokenise<T: AsRef<str>>(expr: T) -> Result<Vec<Token>, ParserError> {
    let expr = expr.as_ref();
    let chars = expr.chars();
    let mut tokens: Vec<Token> = Vec::new();
    let mut parens: Vec<Parenthesis> = Vec::new();
    for c in chars {
        match c {
            '0'..='9' => match tokens.last_mut() {
                Some(Token::Number(n)) => {
                    if *n < 0 {
                        *n = *n * 10 - (c as Signed - 48);
                    } else {
                        *n = *n * 10 + (c as Signed - 48);
                    }
                },
                Some(Token::UnaryOp(UnaryOp::Negate)) => {
                    let mut digit = c as Signed - 48;

                    loop {
                        tokens.pop();
                        digit *= -1;
                        
                        if let Some(last) = tokens.last() {
                            if last.clone() != Token::UnaryOp(UnaryOp::Negate) {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    tokens.push(Token::Number(digit));

                }
                _ => {
                    let digit = c as Signed - 48;
                    tokens.push(Token::Number(digit));
                }
            }
            '(' => {
                 if let Some(token) = tokens.last().clone() {
                    match token {
                        Token::Number(_) => {
                            tokens.push(Token::BinaryOp(BinaryOp::Mul));
                        },
                        Token::BinaryOp(_) => {},
                        Token::UnaryOp(_) => {},
                        Token::Parenthesis(p) => {
                            if p.clone() == Parenthesis::CLOSED {
                                tokens.push(Token::BinaryOp(BinaryOp::Mul));
                            }
                        },
                    }
                }
                tokens.push(Token::Parenthesis(Parenthesis::OPEN));
                parens.push(Parenthesis::OPEN)
            },
            ')' => {
                if let Some(p) = parens.pop() {
                    if p != Parenthesis::OPEN {
                        return Err(ParserError::MismatchedParenthesis);
                    }
                } else {
                    return Err(ParserError::MismatchedParenthesis);
                }

                tokens.push(Token::Parenthesis(Parenthesis::CLOSED));
            },
            '+' => tokens.push(Token::BinaryOp(BinaryOp::Add)),
            '-' => {
                if let Some(token) = tokens.last().clone() {
                    match token.clone() {
                        Token::BinaryOp(BinaryOp::Add) | Token::BinaryOp(BinaryOp::Sub) 
                        | Token::BinaryOp(BinaryOp::Mul) | Token::BinaryOp(BinaryOp::Div) 
                        | Token::BinaryOp(BinaryOp::Mod) | Token::BinaryOp(BinaryOp::Exp) 
                        | Token::UnaryOp(UnaryOp::Negate)
                        | Token::Parenthesis(Parenthesis::OPEN) => {
                            tokens.push(Token::UnaryOp(UnaryOp::Negate))
                        }
                        _ => {
                            tokens.push(Token::BinaryOp(BinaryOp::Sub))
                        }
                    }
                } else {
                    tokens.push(Token::UnaryOp(UnaryOp::Negate))
                }
            },
            '*' => tokens.push(Token::BinaryOp(BinaryOp::Mul)),
            '/' => tokens.push(Token::BinaryOp(BinaryOp::Div)),
            '^' => tokens.push(Token::BinaryOp(BinaryOp::Exp)),
            '%' => tokens.push(Token::BinaryOp(BinaryOp::Mod)),
            ' ' | '\n' | '\r' | '\t' => {},
            _ => return Err(ParserError::BadToken(c))
        }
    }

    if parens.len() > 0 {
        return Err(ParserError::MismatchedParenthesis);
    }

    return Ok(tokens);
}

pub fn infix_to_rpn(tokens: &Vec<Token>) -> VecDeque<Token> {
    let mut queue: VecDeque<Token> = VecDeque::new();
    let mut operation_stack: Vec<Token> = Vec::new();
    
    for token in tokens {
        match token {
            Token::Number(_) => queue.push_back(token.clone()),
            Token::BinaryOp(op1) => {
                let op1_precedence = get_binary_operator_precedence(op1.clone());
                while !operation_stack.is_empty() {
                    let next_op = operation_stack.last().unwrap().clone();
                    if let Token::BinaryOp(op2) = next_op.clone() {
                        let op2_precedence = get_binary_operator_precedence(op2);
                        if op2_precedence >= op1_precedence && next_op != Token::Parenthesis(Parenthesis::OPEN){
                            queue.push_back(operation_stack.pop().unwrap());
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                operation_stack.push(token.clone());
            },
            Token::UnaryOp(_) => {
                while !operation_stack.is_empty() {
                    let next_op = operation_stack.last().unwrap().clone();
                    if let Token::UnaryOp(_) = next_op.clone() {
                        if next_op != Token::Parenthesis(Parenthesis::OPEN){
                            queue.push_back(operation_stack.pop().unwrap());
                        } else {
                            break;
                        }
                    }             
                    else {
                        break;
                    }
                }
                operation_stack.push(token.clone());
            }
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

pub fn evaluate_rpn(rpn: &mut VecDeque<Token>) -> Result<Signed, CalculatonError> {
    let mut stack: Vec<Signed> = Vec::new();
    while !rpn.is_empty() {
        let token = rpn.pop_front();
        match token.unwrap() {
            Token::Number(num) => stack.push(num),
            Token::BinaryOp(op) => {
                let y = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                match op {
                    BinaryOp::Add => {
                        match x.checked_add(y) {
                            Some(num) => stack.push(num),
                            None => return Err(CalculatonError::IntegerOverflow),
                        }
                    },
                    BinaryOp::Sub => {
                        match x.checked_sub(y) {
                            Some(num) => stack.push(num),
                            None => return Err(CalculatonError::IntegerOverflow)
                        }
                    },
                    BinaryOp::Mul => {
                        match x.checked_mul(y) {
                            Some(num) => stack.push(num),
                            None => return Err(CalculatonError::IntegerOverflow)
                        }
                    },
                    BinaryOp::Div => {
                        if y == 0 {
                            return Err(CalculatonError::UndefinedOperation("Cannot divide by 0".to_string()))
                        }
                        match x.checked_div(y) {
                            Some(num) => stack.push(num),
                            None => return Err(CalculatonError::IntegerOverflow)
                        }
                    },
                    BinaryOp::Exp => {
                        if x == 0 && y == 0 {
                            return Err(CalculatonError::UndefinedOperation("0^0".to_string()))
                        }
                        match x.checked_pow(y as u32) {
                            Some(num) => stack.push(num),
                            None => return Err(CalculatonError::IntegerOverflow)
                        }
                    },
                    BinaryOp::Mod => {
                        if y == 0 {
                            return Err(CalculatonError::UndefinedOperation("Cannot modulo by 0".to_string()))
                        }
                        match x.checked_rem(y) {
                            Some(num) => stack.push(num),
                            None => return Err(CalculatonError::IntegerOverflow)
                        }
                    },
                }
            },
            Token::UnaryOp(op) =>  {
                match op {
                    UnaryOp::Negate => {
                        let x = stack.pop().unwrap();
                        stack.push(x * -1);
                    }
                }
            },
            Token::Parenthesis(_) => panic!("There should be no parenthesis in expression evaluation stage! There must be a bug in the infix to rpn conversion..."),
        }
    }

    if stack.len() == 0 {
        return Ok(0)
    };
    return Ok(*stack.first().unwrap());
}