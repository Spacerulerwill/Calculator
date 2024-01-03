use std::collections::VecDeque;

pub type Signed = i128;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOp {
    Add ,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
}

static OPERATOR_CHARS: [char; 6] = ['+', '-', '*', '/', '%', '^'];

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOp {
    Negate,
    Factorial
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
    InvalidConsecutiveTokens(char, char),
    InvalidNumberOfOperands(char, i32),
    OperandOverflow
}

#[derive(Debug)]
pub enum CalculatonError {
    UndefinedOperation(String),
    IntegerOverflow
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Associativity {
    LEFT,
    RIGHT
}

fn factorial(num: Signed) -> Option<Signed> {
    let mut result: Signed = 1;
    for i in 2..=num {
        if let Some(num) = result.checked_mul(i) {
            result = num;
        } else {
            return None;
        }
    }
    return Some(result);
}

fn get_binary_operator_precedence(op: BinaryOp) -> Signed {
    return match op {
        BinaryOp::Add | BinaryOp::Sub => 0,
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 1,
        BinaryOp::Exp  => 2,
    }
}

fn get_binary_operator_associativity(op: BinaryOp) -> Associativity {
    return match op {
        BinaryOp::Exp => Associativity::RIGHT,
        _ => Associativity::LEFT 
    }
}

fn is_consecutive_tokens_valid(current_token_char: char, all_tokens: &Vec<Token>) -> Result<(), ParserError> {
    if let Some(op) = all_tokens.last() {
        match op {
            Token::BinaryOp(opchar) => {
                return Err(ParserError::InvalidConsecutiveTokens(OPERATOR_CHARS[*opchar as usize], current_token_char));
            },
            _ => {Ok(())}
        }
    } else {
        return Err(ParserError::InvalidNumberOfOperands(current_token_char, 2));
    }
}

pub fn tokenise<T: AsRef<str>>(expr: T) -> Result<Vec<Token>, ParserError> {
    let expr = expr.as_ref();
    let chars = expr.chars();
    let mut tokens: Vec<Token> = Vec::new();
    let mut parens: Vec<Parenthesis> = Vec::new();
    let mut unary_minus = 0;

    for c in chars {
        match c {
            '0'..='9' => match tokens.last_mut() {
                Some(Token::Number(n)) => {
                    match (*n).checked_mul(10) {
                        Some(result) => *n = result,
                        None => return Err(ParserError::OperandOverflow),
                    }
                    match (*n).checked_add(c as Signed - 48) {
                        Some(result) => *n = result,
                        None => return Err(ParserError::OperandOverflow)
                    }
                },
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
                        Token::UnaryOp(UnaryOp::Negate) => {
                            tokens.pop();
                            tokens.push(Token::Parenthesis(Parenthesis::OPEN));
                            unary_minus += 1;
                        },
                        Token::UnaryOp(UnaryOp::Factorial) => {

                        }
                        Token::Parenthesis(p) => {
                            if p.clone() == Parenthesis::CLOSED {
                                tokens.push(Token::BinaryOp(BinaryOp::Mul));
                            }
                        },
                    }
                }
                tokens.push(Token::Parenthesis(Parenthesis::OPEN));
                parens.push(Parenthesis::OPEN);
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
                if unary_minus > 0 {
                    unary_minus -= 1;
                    tokens.push(Token::BinaryOp(BinaryOp::Mul));
                    tokens.push(Token::Number(-1));
                    tokens.push(Token::Parenthesis(Parenthesis::CLOSED));
                }
            },
            '+' => {
                let res = is_consecutive_tokens_valid(c, &tokens);
                if let Err(err) = res { return Err(err)}
                tokens.push(Token::BinaryOp(BinaryOp::Add))
            },
            '-' => {
                if let Some(token) = tokens.last().clone() {
                    match token.clone() {
                        Token::BinaryOp(BinaryOp::Add) | Token::BinaryOp(BinaryOp::Sub) 
                        | Token::BinaryOp(BinaryOp::Mul) | Token::BinaryOp(BinaryOp::Div) 
                        | Token::BinaryOp(BinaryOp::Mod) | Token::BinaryOp(BinaryOp::Exp) 
                        | Token::UnaryOp(UnaryOp::Negate)
                        | Token::Parenthesis(Parenthesis::OPEN) => {
                            tokens.push(Token::UnaryOp(UnaryOp::Negate));
                        }
                        _ => {
                            tokens.push(Token::BinaryOp(BinaryOp::Sub));
                        }
                    }
                } else {
                    tokens.push(Token::UnaryOp(UnaryOp::Negate))
                }
            },
            '*' => {
                let res = is_consecutive_tokens_valid(c, &tokens);
                if let Err(err) = res { return Err(err)}
                tokens.push(Token::BinaryOp(BinaryOp::Mul));
            },
            '/' => {
                let res = is_consecutive_tokens_valid(c, &tokens);
                if let Err(err) = res { return Err(err)}
                tokens.push(Token::BinaryOp(BinaryOp::Div));
            },
            '^' => {
                let res = is_consecutive_tokens_valid(c, &tokens);
                if let Err(err) = res { return Err(err)}
                tokens.push(Token::BinaryOp(BinaryOp::Exp));
            },
            '%' => {
                let res = is_consecutive_tokens_valid(c, &tokens);
                if let Err(err) = res { return Err(err)}
                tokens.push(Token::BinaryOp(BinaryOp::Mod));
            },
            '!' => {
                tokens.push(Token::UnaryOp(UnaryOp::Factorial));
            }
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
            Token::Number(_) => {
                queue.push_back(*token);
            },
            Token::BinaryOp(op1) => 'binaryop: {
                if operation_stack.is_empty() || operation_stack.last().unwrap().clone() == Token::Parenthesis(Parenthesis::OPEN) {
                    operation_stack.push(*token);
                    break 'binaryop;
                }

                let op1_precedence = get_binary_operator_precedence(*op1);
                let op1_associativity = get_binary_operator_associativity(*op1);
                let mut next_op = operation_stack.last().unwrap();
                if let Token::BinaryOp(op2) = next_op {
                    let op2_precedence = get_binary_operator_precedence(*op2);
                    if op1_precedence > op2_precedence || (op1_precedence == op2_precedence && op1_associativity == Associativity::RIGHT) {
                        operation_stack.push(*token);
                        break 'binaryop;
                    }
                } else {
                    break 'binaryop;
                }

                while !operation_stack.is_empty() {
                    next_op = operation_stack.last().unwrap();
                    if let Token::BinaryOp(op2) = next_op.clone() {
                        let op2_precedence = get_binary_operator_precedence(op2);
                        if op1_precedence < op2_precedence || (op1_precedence == op2_precedence && op1_associativity == Associativity::LEFT) {
                            queue.push_back(operation_stack.pop().unwrap());
                        } else {
                            break 'binaryop;
                        }
                    }
                    else {
                        break 'binaryop;
                    }
                }
                operation_stack.push(*token);
            },
            Token::UnaryOp(UnaryOp::Factorial) => {
                queue.push_back(*token);
            }
            Token::UnaryOp(_) => {},
            Token::Parenthesis(Parenthesis::OPEN) => operation_stack.push(*token),
            Token::Parenthesis(Parenthesis::CLOSED) => {
                while !operation_stack.is_empty() {
                    let next_op = operation_stack.last().unwrap().clone();
                    if next_op != Token::Parenthesis(Parenthesis::OPEN) {
                        queue.push_back(operation_stack.pop().unwrap());
                    } else {
                        operation_stack.pop();
                        break;     
                    }
                }
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
            Token::UnaryOp(op) => {
                let x = stack.pop().unwrap();
                match op {
                    UnaryOp::Factorial => {
                        match factorial(x) {
                            Some(result) => {
                                stack.push(result)
                            },
                            None => return Err(CalculatonError::IntegerOverflow),
                        }
                    }
                    UnaryOp::Negate => panic!("There should be no unary minus in expression evaluation stage! There must be a bug in the infix to rpn conversion...")
                }
            }
            Token::Parenthesis(_) => panic!("There should be no parenthesis in expression evaluation stage! There must be a bug in the infix to rpn conversion..."),
        }
    }

    if stack.len() == 0 {
        return Ok(0)
    };
    return Ok(*stack.first().unwrap());
}