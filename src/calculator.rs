use std::collections::VecDeque;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Parenthesis {
    OPEN,
    CLOSED
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Number(u32),
    Op(Operator),
    Parenthesis(Parenthesis)
}

#[derive(Debug, Clone)]
pub enum ParserError {
    BadToken(char),
    MismatchedParenthesis,
}

#[derive(Debug)]
pub enum CalculatonError {
    UndefinedOperation(String)
}

fn get_operator_precedence(op: Operator) -> i32 {
    return match op {
        Operator::Add | Operator::Sub => 0,
        Operator::Mul | Operator::Div | Operator::Mod => 1,
        Operator::Exp => 2,
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
                        return Err(ParserError::MismatchedParenthesis);
                    }
                } else {
                    return Err(ParserError::MismatchedParenthesis);
                }

                tokens.push(Token::Parenthesis(Parenthesis::CLOSED));
            },
            '+' => tokens.push(Token::Op(Operator::Add)),
            '-' => tokens.push(Token::Op(Operator::Sub)),
            '*' => tokens.push(Token::Op(Operator::Mul)),
            '/' => tokens.push(Token::Op(Operator::Div)),
            '^' => tokens.push(Token::Op(Operator::Exp)),
            '%' => tokens.push(Token::Op(Operator::Mod)),
            ' ' | '\n' | '\r' => {},
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
            Token::Op(op1) => {
                let op1_precedence = get_operator_precedence(op1.clone());
                while !operation_stack.is_empty() {
                    let next_op = operation_stack.last().unwrap().clone();
                    if let Token::Op(op2) = next_op.clone() {
                        let op2_precedence = get_operator_precedence(op2);
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

pub fn evaluate_rpn(rpn: &mut VecDeque<Token>) -> Result<u32, CalculatonError> {
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
                        if y == 0 {
                            return Err(CalculatonError::UndefinedOperation("Cannot divide by 0".to_string()))
                        }
                        stack.push(x / y);
                    },
                    Operator::Exp => {
                        if x == 0 && y == 0 {
                            return Err(CalculatonError::UndefinedOperation("0^0".to_string()))
                        }
                        stack.push(x.pow(y));
                    },
                    Operator::Mod => {
                        if y == 0 {
                            return Err(CalculatonError::UndefinedOperation("Cannot modulo by 0".to_string()))
                        }
                        stack.push(x % y);
                    },
                }
            },
            Token::Parenthesis(_) => panic!("There should be no parenthesis in expression evaluation stage! There must be a bug in the infix to rpn conversion..."),
        }
    }
    return Ok(*stack.first().unwrap());
}