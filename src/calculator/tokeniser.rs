use std::iter::Peekable;
use std::str::Chars;

use crate::calculator::tokens::{Token, BinaryOp, UnaryOp, Parenthesis, Number, FloatType, IntType, KEYWORDS};

#[derive(Debug)]
pub enum TokeniserError {
    BadToken(char),
    MismatchedParenthesis,
    InvalidConsecutiveTokens(String, String),
    InvalidNumberOfOperands(String, i32),
    InvalidFunctionOrConstant(String),
}

pub fn tokenise(expr: &String) -> Result<Vec<Token>, TokeniserError> {
    let expr = expr.trim();
    let mut chars = expr.chars().peekable();
    let mut tokens: Vec<Token> = Vec::new();
    let mut parens: Vec<Parenthesis> = Vec::new();
    
    if expr.len() == 0 { return Ok(tokens); }

    // get tokens
    while let Some(c) = chars.next() {
        match c {
            '0'..='9' => tokenise_digits(c, &mut tokens,&mut chars),
            'a'..='z' => tokenise_letters(c, &mut tokens, &mut chars)?,
            '(' => tokenise_open_parenthesis(&mut tokens, &mut parens),
            ')' => tokenise_closed_parenthesis(&mut tokens, &mut parens)?,
            '+' => tokens.push(Token::BinaryOp(BinaryOp::ADD)),
            '-' => tokenise_minus(&mut tokens),
            '*' => tokens.push(Token::BinaryOp(BinaryOp::MUL)),
            '/' => tokens.push(Token::BinaryOp(BinaryOp::DIV)),
            '^' => tokens.push(Token::BinaryOp(BinaryOp::EXP)),
            '%' => tokens.push(Token::BinaryOp(BinaryOp::MOD)),
            ' ' | '\n' | '\r' | '\t' => continue,
            _ => return Err(TokeniserError::BadToken(c)),
        }
    }

    if parens.len() > 0 { return Err(TokeniserError::MismatchedParenthesis); }
    check_consecutive_tokens_valid(&tokens)?;
    Ok(tokens)
}

fn check_consecutive_tokens_valid(tokens: &Vec<Token>) -> Result<(), TokeniserError>{
    // iterate through tokens checking consecutive tokens are in a valid order
    let mut iter = tokens.iter().rev().peekable();
    while let Some(second_token) = iter.next() {
        if let Some(first_token) = iter.peek() {
            match second_token {
                Token::Number(second_num) | Token::Constant(second_num) => {
                    match first_token {
                        Token::Number(first_num) | Token::Constant(first_num) => return Err(TokeniserError::InvalidConsecutiveTokens(first_num.to_string(), second_num.to_string())),
                        Token::Parenthesis(Parenthesis::CLOSED) => return Err(TokeniserError::InvalidConsecutiveTokens(")".to_string(), second_num.to_string())),
                        _ => {}
                    }
                },
                Token::BinaryOp(second_binary_op) => {
                    match first_token {
                        Token::BinaryOp(first_binary_op) => return Err(TokeniserError::InvalidConsecutiveTokens(first_binary_op.to_string(), second_binary_op.to_string())),
                        Token::UnaryOp(first_unary_op) => return Err(TokeniserError::InvalidConsecutiveTokens(first_unary_op.to_string(), second_binary_op.to_string())),
                        Token::Parenthesis(Parenthesis::OPEN) => return Err(TokeniserError::InvalidConsecutiveTokens("(".to_string(), second_binary_op.to_string())),
                        _ => {}
                    }
                },
                Token::Parenthesis(Parenthesis::CLOSED) => {
                    match first_token {
                        Token::BinaryOp(first_binary_op) => return Err(TokeniserError::InvalidConsecutiveTokens(first_binary_op.to_string(), ")".to_string())),
                        Token::UnaryOp(first_unary_op) => return Err(TokeniserError::InvalidConsecutiveTokens(first_unary_op.to_string(),")".to_string())),
                        Token::Parenthesis(Parenthesis::OPEN) => return Err(TokeniserError::InvalidConsecutiveTokens("(".to_string(), ")".to_string())),
                        _ => {}
                    }
                },
                _ => {}
            }
        }
    }  

    // check first token 
    match tokens.first() {
        Some(token) => {
            match token {
                Token::BinaryOp(binary_op) => {
                    return Err(TokeniserError::InvalidNumberOfOperands(binary_op.to_string(), 2))
                }
                _ => {}
            }
        },
        None => {},
    } 

    // check last
    match tokens.last() {
        Some(token) => {
            match token {
                Token::BinaryOp(binary_op) => {
                    return Err(TokeniserError::InvalidNumberOfOperands(binary_op.to_string(), 2))
                }
                Token::UnaryOp(unary_op) => {
                    return Err(TokeniserError::InvalidNumberOfOperands(unary_op.to_string(), 1))
                },
                _ => {}
            }
        },
        None => {},
    }
    Ok(())
}

fn tokenise_digits(current_char: char, tokens: &mut Vec<Token>, chars: &mut Peekable<Chars<'_>>) {
    // First step, move forwards until we find a non digit character
    let mut number_string = String::new();
    let mut terminator_char: char = '\0'; 
    number_string.push(current_char);

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

fn tokenise_letters(current_char: char, tokens: &mut Vec<Token>, chars: &mut Peekable<Chars<'_>>) -> Result<(), TokeniserError> {
    let mut keyword = String::new();
    keyword.push(current_char);

    while let Some(d) = chars.peek() {
        if d.is_alphabetic() {
            keyword.push(*d);
            chars.next();

        } else {
            break;
        }
    };
    
    match KEYWORDS.get(&keyword) {
        Some(token) => {
            if let Some(c) = tokens.last().clone() {
                match c {
                    Token::Constant(_) | Token::Number(_) | Token::Parenthesis(Parenthesis::CLOSED) => {
                        tokens.push(Token::BinaryOp(BinaryOp::MUL));
                    },
                    _ => {}

                }
            }                  
            tokens.push(*token);
            Ok(())
        },
        None => return Err(TokeniserError::InvalidFunctionOrConstant(keyword)),
    }
}

fn tokenise_open_parenthesis(tokens: &mut Vec<Token>, parens: &mut Vec<Parenthesis>) {
    if let Some(token) = tokens.last() {
        match token {
            Token::Number(_) | Token::Constant(_) => {
                tokens.push(Token::BinaryOp(BinaryOp::MUL));
            },
            Token::Parenthesis(p) => {
                if p.clone() == Parenthesis::CLOSED {
                    tokens.push(Token::BinaryOp(BinaryOp::MUL));
                }
            },
            _ => {}
        }
    }

    tokens.push(Token::Parenthesis(Parenthesis::OPEN));
    parens.push(Parenthesis::OPEN);
}

fn tokenise_closed_parenthesis(tokens: &mut Vec<Token>, parens: &mut Vec<Parenthesis>) -> Result<(), TokeniserError>{
    if let Some(p) = parens.pop() {
        if p != Parenthesis::OPEN {
            return Err(TokeniserError::MismatchedParenthesis);
        }
    } else {
        return Err(TokeniserError::MismatchedParenthesis);
    }
    tokens.push(Token::Parenthesis(Parenthesis::CLOSED));
    Ok(())
}

fn tokenise_minus(tokens: &mut Vec<Token>) {
    if let Some(token) = tokens.last().clone() {
        match token.clone() {
            | Token::BinaryOp(_)
            | Token::UnaryOp(_)
            | Token::Parenthesis(Parenthesis::OPEN) => {
                tokens.push(Token::UnaryOp(UnaryOp::NEGATE));
            }
            _ => {
                tokens.push(Token::BinaryOp(BinaryOp::SUB));
            }
        }
    } else {
        tokens.push(Token::UnaryOp(UnaryOp::NEGATE))
    }
}