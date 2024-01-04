use std::{collections::VecDeque, thread::current, env::current_exe};

pub type FloatType = f64;
pub type IntType = i128;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
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
pub enum Associativity {
    LEFT,
    RIGHT
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum UnaryOp {
    Negate,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Parenthesis {
    OPEN,
    CLOSED
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Number {
    Integer(IntType),
    Float(FloatType),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Token {
    Number(Number),
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
}

#[derive(Debug)]
pub enum CalculatonError {
    UndefinedOperation(String),
    IntegerOverflow
}

fn get_binary_operator_precedence(op: BinaryOp) -> IntType {
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

// determine if the previous operator can be next to the current operator
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

fn tokenise_operator(c: &char, tokens: &mut Vec<Token>, parens: &mut Vec<Parenthesis>) -> Result<(), ParserError> {
    match c {
        '(' => {
            if let Some(token) = tokens.last().clone() {
                match token {
                    Token::Number(_) => {
                        tokens.push(Token::BinaryOp(BinaryOp::Mul));
                    },
                    Token::Parenthesis(p) => {
                        if p.clone() == Parenthesis::CLOSED {
                            tokens.push(Token::BinaryOp(BinaryOp::Mul));
                        }
                    },
                    _ => {}
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
        },
        '+' => {
            let res = is_consecutive_tokens_valid(*c, &tokens);
            if let Err(err) = res { return Err(err)}
            tokens.push(Token::BinaryOp(BinaryOp::Add));
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
        }
        '*' => {
            let res = is_consecutive_tokens_valid(*c, &tokens);
            if let Err(err) = res { return Err(err)}
            tokens.push(Token::BinaryOp(BinaryOp::Mul));
        }
        '/' => {
            let res = is_consecutive_tokens_valid(*c, &tokens);
            if let Err(err) = res { return Err(err)}
            tokens.push(Token::BinaryOp(BinaryOp::Div));
        }
        '^' => {
            let res = is_consecutive_tokens_valid(*c, &tokens);
            if let Err(err) = res { return Err(err)}
            tokens.push(Token::BinaryOp(BinaryOp::Exp));
        }
        '%' => {
            let res = is_consecutive_tokens_valid(*c, &tokens);
            if let Err(err) = res { return Err(err)}
            tokens.push(Token::BinaryOp(BinaryOp::Mod));
        }
        ' ' | '\n' | '\r' | '\t' => {},
        _ => return Err(ParserError::BadToken(*c))
    } 
    Ok(())
}

pub fn tokenise(expr: &String) -> Result<Vec<Token>, ParserError> {
    let expr = expr.trim();
    let mut chars = expr.chars().peekable();
    let mut tokens: Vec<Token> = Vec::new();
    let mut parens: Vec<Parenthesis> = Vec::new();

    while let Some(c) = chars.next() {
        match c {
            '0'..='9' => {
                // First step, move forwards until we find a non digit character
                let mut number_string = String::new();
                let mut terminator_char: char = '\0'; 
                number_string.push(c);

                while let Some(d) = chars.peek() {
                    if d.is_digit(10) {
                        number_string.push(*d);
                        chars.next();

                    } else {
                        // the character we stopped on
                        terminator_char = *d;
                        break;
                    }
                };

                // if the character we stopped on is a decimal point, then the number will be a float
                if terminator_char == '.' {
                    // once again iterate forwards until we find a non digit character
                    chars.next();
                    number_string.push('.');
                    while let Some(d) = chars.peek() {
                        if d.is_digit(10) {
                            number_string.push(*d);
                            chars.next();
                        } else {
                            // the character we stopped
                            break;
                        }
                    }
                    
                    // conver string to number float
                    let float_val: FloatType = match number_string.parse() {
                        Ok(v) => v,
                        Err(_) => panic!("Error converting number string to FloatType")
                    };

                    // push token
                    tokens.push(Token::Number(Number::Float(float_val)));
                } else {
                    // convert string to number integer
                    let int_val: IntType = match number_string.parse() {
                        Ok(v) => v,
                        Err(_) => panic!("Error converting number string to IntType")
                    };

                    // push token
                    tokens.push(Token::Number(Number::Integer(int_val)));
                }
            }
            // for any other token (not a number) we process it in a seperate function
            _ => {
                if let Err(err) = tokenise_operator(&c, &mut tokens, &mut parens) {
                    return Err(err);
                }
            }
        }
    }
    
    if parens.len() > 0 {
        return Err(ParserError::MismatchedParenthesis);
    }

    Ok(tokens)
}

pub fn infix_to_rpn(tokens: &Vec<Token>) -> VecDeque<Token> {
    let mut queue: VecDeque<Token> = VecDeque::new();
    let mut operation_stack: Vec<Token> = Vec::new();
    let mut token_iter = tokens.iter().peekable();
    let mut parenthesis_unary_stacks: Vec<Vec<Token>> = Vec::new();

    while let Some(token) = token_iter.next() {
        match token {
            Token::Number(Number::Integer(_)) | Token::Number(Number::Float(_)) => {
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
            Token::UnaryOp(op) => {
                let mut unary_stack: Vec<Token> = Vec::new();
                let mut terminator_token = Token::UnaryOp(UnaryOp::Negate); // CANT THINK OF A BETTER DEFAULTs

                unary_stack.push(Token::UnaryOp(*op));
                while let Some(token) = token_iter.peek() {
                    if let Token::UnaryOp(op) = token {
                        unary_stack.push(**token);
                        token_iter.next();
                    } else {
                        terminator_token = **token;
                        break;
                    }
                };

                match terminator_token {
                    Token::Parenthesis(Parenthesis::OPEN) => {
                        parenthesis_unary_stacks.push(unary_stack);
                    },
                    Token::Number(Number::Float(f)) => {
                        token_iter.next();
                        queue.push_back(Token::Number(Number::Float(f)));
                        while !unary_stack.is_empty() {
                            queue.push_back(unary_stack.pop().unwrap());
                        }
                    },
                    Token::Number(Number::Integer(i)) => {
                        token_iter.next();
                        queue.push_back(Token::Number(Number::Integer(i)));
                        while !unary_stack.is_empty() {
                            queue.push_back(unary_stack.pop().unwrap());
                        }
                    },
                    _ => panic!("Unary op terminated on a token that was not a number or an open parenthesis. This should never happen! BUG!")
                }
            },
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

                if parenthesis_unary_stacks.len() != 0 {
                    let stack = parenthesis_unary_stacks.pop().unwrap();
                    for op in stack {
                        queue.push_back(op);
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

pub fn evaluate_rpn(rpn: &mut VecDeque<Token>) -> Result<FloatType, CalculatonError> {
    let mut stack: Vec<Number> = Vec::new();
    while !rpn.is_empty() {
        let token = rpn.pop_front();
        match token.unwrap() {
            Token::Number(num) => stack.push(num),
            Token::BinaryOp(op) => {
                let y = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                match op {
                    BinaryOp::Add => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                match i_x.checked_add(i_y) {
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(CalculatonError::IntegerOverflow)
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType) + f_y)),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x + (i_y as FloatType))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x + f_y)),
                        }
                    }
                    BinaryOp::Sub => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                match i_x.checked_sub(i_y) {
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(CalculatonError::IntegerOverflow)
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType) - f_y)),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x - (i_y as FloatType))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x - f_y)),
                        }
                    }
                    BinaryOp::Mul => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                match i_x.checked_mul(i_y) {
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(CalculatonError::IntegerOverflow)
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType) * f_y)),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x * (i_y as FloatType))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x * f_y)),
                        }
                    },
                    BinaryOp::Div => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                if i_y == 0 { return Err(CalculatonError::UndefinedOperation("Cannot divide by 0".to_string())); }
                                match i_x.checked_div(i_y) {
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(CalculatonError::IntegerOverflow)
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType) / f_y)),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x / (i_y as FloatType))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x / f_y)),
                        }
                    }
                    BinaryOp::Exp => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                match i_x.checked_pow(i_y as u32) {
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(CalculatonError::IntegerOverflow),
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType).powf(f_y as f64))),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x.powf(i_y as f64))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x.powf(f_y as f64))),
                        }
                    }
                    BinaryOp::Mod => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                if i_y == 0 { return Err(CalculatonError::UndefinedOperation("Cannot mod by 0".to_string())); }
                                match i_x.checked_rem(i_y){
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(CalculatonError::IntegerOverflow),
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType) % f_y)),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x % (i_y as FloatType))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x % f_y)),
                        }
                    },
                }
            },
            Token::UnaryOp(op) => {
                let x = stack.pop().unwrap();
                match op {
                    UnaryOp::Negate => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Integer((i_x * -1))),
                            Number::Float(f_x) => stack.push(Number::Float(f_x * -1.0)),
                        }
                    },
                }
            },
            Token::Parenthesis(_) => panic!("There should be no parenthesis in expression evaluation stage! There must be a bug in the infix to rpn conversion..."),
        }
    }

    if stack.len() == 0 {
        return Ok(0.0)
    };

    return match stack.last().unwrap() {
        Number::Integer(i) => {
            Ok(*i as FloatType)
        },
        Number::Float(f) => {
            Ok(*f)
        },
    };
}