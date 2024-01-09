use std::collections::VecDeque;
use std::vec::Vec;

use crate::calculator::tokens::{Token, FloatType, BinaryOp, UnaryOp, Number};

#[derive(Debug)]
pub enum EvaluationError {
    UndefinedOperation(String),
    IntegerOverflow,
    InvalidFunctionDomain(String, String)
}

pub fn evaluate_rpn(rpn: &mut VecDeque<Token>) -> Result<FloatType, EvaluationError> {
    let mut stack: Vec<Number> = Vec::new();
    while !rpn.is_empty() {
        let token = rpn.pop_front();
        match token.unwrap() {
            Token::Number(num) | Token::Constant(num) => stack.push(num),
            Token::BinaryOp(op) => {
                let y = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                match op {
                    BinaryOp::ADD => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                match i_x.checked_add(i_y) {
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(EvaluationError::IntegerOverflow)
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType) + f_y)),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x + (i_y as FloatType))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x + f_y)),
                        }
                    }
                    BinaryOp::SUB => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                match i_x.checked_sub(i_y) {
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(EvaluationError::IntegerOverflow)
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType) - f_y)),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x - (i_y as FloatType))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x - f_y)),
                        }
                    }
                    BinaryOp::MUL => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                match i_x.checked_mul(i_y) {
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(EvaluationError::IntegerOverflow)
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType) * f_y)),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x * (i_y as FloatType))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x * f_y)),
                        }
                    },
                    BinaryOp::DIV => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                if i_y == 0 { return Err(EvaluationError::UndefinedOperation("Cannot divide by 0".to_string())); }
                                match i_x.checked_div(i_y) {
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(EvaluationError::IntegerOverflow)
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType) / f_y)),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x / (i_y as FloatType))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x / f_y)),
                        }
                    }
                    BinaryOp::EXP => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                match i_x.checked_pow(i_y as u32) {
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(EvaluationError::IntegerOverflow),
                                }
                            },
                            (Number::Integer(i_x), Number::Float(f_y)) => stack.push(Number::Float((i_x as FloatType).powf(f_y as f64))),
                            (Number::Float(f_x), Number::Integer(i_y)) => stack.push(Number::Float(f_x.powf(i_y as f64))),
                            (Number::Float(f_x), Number::Float(f_y)) => stack.push(Number::Float(f_x.powf(f_y as f64))),
                        }
                    }
                    BinaryOp::MOD => {
                        match (x,y) {
                            (Number::Integer(i_x), Number::Integer(i_y)) => {
                                if i_y == 0 { return Err(EvaluationError::UndefinedOperation("Cannot mod by 0".to_string())); }
                                match i_x.checked_rem(i_y){
                                    Some(num) => stack.push(Number::Integer(num)),
                                    None => return Err(EvaluationError::IntegerOverflow),
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
                    UnaryOp::NEGATE => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Integer(i_x * -1)),
                            Number::Float(f_x) => stack.push(Number::Float(f_x * -1.0)),
                        }
                    },
                    UnaryOp::ABS => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Integer(i_x.abs())),
                            Number::Float(f_x) => stack.push(Number::Float(f_x.abs())),
                        }
                    },
                    UnaryOp::SIN => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float((i_x as FloatType).sin())),
                            Number::Float(f_x) => stack.push(Number::Float(f_x.sin())),
                        }
                    },
                    UnaryOp::COS => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float((i_x as FloatType).cos())),
                            Number::Float(f_x) => stack.push(Number::Float(f_x.cos())),
                        }
                    } ,
                    UnaryOp::TAN => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float((i_x as FloatType).tan())),
                            Number::Float(f_x) => stack.push(Number::Float(f_x.tan())),
                        }
                    }
                    UnaryOp::ARCSIN => {
                        match x {
                            Number::Integer(i_x) => {
                                if i_x > 1 || i_x < -1 { return Err(EvaluationError::InvalidFunctionDomain("asin".to_string(), "-1 <= x <= 1".to_string()))}
                                stack.push(Number::Float((i_x as FloatType).asin()))
                            },
                            Number::Float(f_x) => {
                                if f_x > 1.0 || f_x < -1.0 { return Err(EvaluationError::InvalidFunctionDomain("asin".to_string(), "-1 <= x <= 1".to_string()))}
                                stack.push(Number::Float(f_x.tan()))
                            },
                        }
                    }
                    UnaryOp::ARCCOS => {
                        match x {
                            Number::Integer(i_x) => {
                                if i_x > 1 || i_x < -1 { return Err(EvaluationError::InvalidFunctionDomain("acos".to_string(), "-1 <= x <= 1".to_string()))}
                                stack.push(Number::Float((i_x as FloatType).acos()))
                            },
                            Number::Float(f_x) => {
                                if f_x > 1.0 || f_x < -1.0 { return Err(EvaluationError::InvalidFunctionDomain("acos".to_string(), "-1 <= x <= 1".to_string()))}
                                stack.push(Number::Float(f_x.tan()))
                            },
                        }
                    },
                    UnaryOp::ARCTAN => {
                        match x {
                            Number::Integer(i_x) => {
                                if i_x > 1 || i_x < -1 { return Err(EvaluationError::InvalidFunctionDomain("atan".to_string(), "-1 <= x <= 1".to_string()))}
                                stack.push(Number::Float((i_x as FloatType).atan()))
                            },
                            Number::Float(f_x) => {
                                if f_x > 1.0 || f_x < -1.0 { return Err(EvaluationError::InvalidFunctionDomain("atan".to_string(), "-1 <= x <= 1".to_string()))}
                                stack.push(Number::Float(f_x.tan()))
                            },
                        }
                    },
                    UnaryOp::COSEC => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float(1.0 / ((i_x as FloatType)).sin())),
                            Number::Float(f_x) => stack.push(Number::Float(1.0 / f_x.sin())),
                        }
                    },
                    UnaryOp::SEC => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float(1.0 / ((i_x as FloatType)).cos())),
                            Number::Float(f_x) => stack.push(Number::Float(1.0 / f_x.cos())),
                        }
                    },
                    UnaryOp::COT => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float(1.0 / ((i_x as FloatType)).tan())),
                            Number::Float(f_x) => stack.push(Number::Float(1.0 / f_x.tan())),
                        }
                    },
                    UnaryOp::SINH => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float((i_x as FloatType).sinh())),
                            Number::Float(f_x) => stack.push(Number::Float(f_x.sinh())),
                        }
                    },
                    UnaryOp::COSH => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float((i_x as FloatType).cosh())),
                            Number::Float(f_x) => stack.push(Number::Float(f_x.cosh())),
                        }
                    },
                    UnaryOp::TANH => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float((i_x as FloatType).tanh())),
                            Number::Float(f_x) => stack.push(Number::Float(f_x.tanh())),
                        }
                    },
                    UnaryOp::COSECH => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float(1.0 / ((i_x as FloatType)).sinh())),
                            Number::Float(f_x) => stack.push(Number::Float(1.0 / f_x.sin())),
                        }
                    },
                    UnaryOp::SECH => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float(1.0 / ((i_x as FloatType)).cosh())),
                            Number::Float(f_x) => stack.push(Number::Float(1.0 / f_x.cos())),
                        }
                    },
                    UnaryOp::COTH => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float(1.0 / ((i_x as FloatType)).tanh())),
                            Number::Float(f_x) => stack.push(Number::Float(1.0 / f_x.tan())),
                        }
                    },
                    UnaryOp::ARSINH => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float((i_x as FloatType).asinh())),
                            Number::Float(f_x) => stack.push(Number::Float(f_x.asinh())),
                        }
                    },
                    UnaryOp::ARCOSH => {
                        match x {
                            Number::Integer(i_x) => {
                                if i_x < 0 { return Err(EvaluationError::InvalidFunctionDomain("arcosh".to_string(), "x >= 0".to_string()))}
                                stack.push(Number::Float((i_x as FloatType).acosh()));
                            },
                            Number::Float(f_x) => {
                                if f_x < 0.0 { return Err(EvaluationError::InvalidFunctionDomain("artanh".to_string(), "x >= 0".to_string()))}
                                stack.push(Number::Float(f_x.acosh()));
                            }
                        }
                    },
                    UnaryOp::ARTANH => {
                        match x {
                            Number::Integer(i_x) => {
                                if i_x < - 1 || i_x > 1 { return Err(EvaluationError::InvalidFunctionDomain("artanh".to_string(), "-1 <= x <= 1".to_string()))}
                                stack.push(Number::Float((i_x as FloatType).atanh()));
                            },
                            Number::Float(f_x) => {
                                if f_x < - 1.0 || f_x > 1.0 { return Err(EvaluationError::InvalidFunctionDomain("artanh".to_string(), "-1 <= x <= 1".to_string()))}
                                stack.push(Number::Float(f_x.atanh()));
                            }
                        }
                    },
                    UnaryOp::RAD => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float((i_x as FloatType).to_radians())),
                            Number::Float(f_x) => stack.push(Number::Float(f_x.to_radians())),
                        }
                    },
                    UnaryOp::DEG => {
                        match x {
                            Number::Integer(i_x) => stack.push(Number::Float((i_x as FloatType).to_degrees())),
                            Number::Float(f_x) => stack.push(Number::Float(f_x.to_degrees())),
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