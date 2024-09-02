use std::collections::HashMap;

use crate::{
    tokenizer::{Token, TokenKind},
    value::Value,
    variable::Variable,
};
use num_complex::{Complex64, ComplexFloat};

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Absolute {
        expr: Box<Expr>,
    },
    Number {
        number: Complex64,
    },
    Identifier {
        name: Token,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
}

#[derive(Debug)]
pub struct EvaluationError {
    pub col: usize,
    pub message: String,
}

impl Expr {
    pub fn evaluate(self, variables: &HashMap<&str, Variable>) -> Result<Value, EvaluationError> {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.evaluate(variables)?;
                let right = right.evaluate(variables)?;
                match operator.kind {
                    TokenKind::Plus => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => {
                            Ok(Value::Number(left + right))
                        }
                        _ => todo!(),
                    },
                    TokenKind::Minus => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => {
                            Ok(Value::Number(left - right))
                        }
                        _ => todo!(),
                    },
                    TokenKind::Star => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => {
                            Ok(Value::Number(left * right))
                        }
                        _ => todo!(),
                    },
                    TokenKind::Slash => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => {
                            Ok(Value::Number(left / right))
                        }
                        _ => todo!(),
                    },
                    TokenKind::Caret => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => {
                            Ok(Value::Number(left.powc(right)))
                        }
                        _ => todo!(),
                    },
                    TokenKind::Percent => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => {
                            Ok(Value::Number(left % right))
                        }
                        _ => todo!(),
                    },
                    kind => panic!("Invalid token kind for binary operation: {:?}", kind),
                }
            }
            Expr::Unary { operator, right } => {
                let right = right.evaluate(variables)?;
                match operator.kind {
                    TokenKind::Minus => match right {
                        Value::Number(right) => {
                            Ok(Value::Number(right * Complex64::new(-1.0, 0.0)))
                        }
                        _ => todo!(),
                    },
                    TokenKind::Bang => match right {
                        Value::Number(right) => {
                            let real_part = right.re;
                            if right.im == 0.0 {
                                if real_part.fract() == 0.0 && real_part >= 0.0 {
                                    Ok(Value::Number(factorial(real_part as u64).into()))
                                } else {
                                    todo!()
                                }
                            } else {
                                todo!()
                            }
                        }
                        _ => todo!(),
                    },
                    kind => panic!("Invalid token kind for unary operation: {:?}", kind),
                }
            }
            Expr::Grouping { expr } => Ok(expr.evaluate(variables)?),
            Expr::Absolute { expr } => {
                let result = expr.evaluate(variables)?;
                match result {
                    Value::Number(result) => Ok(Value::Number(result.norm_sqr().into())),
                    _ => todo!(),
                }
            }
            Expr::Number { number } => Ok(Value::Number(number)),
            Expr::Identifier { name } => {
                if let Some(variable) = variables.get(name.kind.get_lexeme().as_str()) {
                    Ok(variable.value.clone())
                } else {
                    Ok(Value::Number(Complex64::from(0.0)))
                }
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => todo!(),
        }
    }
}

// Computes factorial of a non-negative integer
fn factorial(n: u64) -> f64 {
    (1..=n).map(|x| x as f64).product()
}
