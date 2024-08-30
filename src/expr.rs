use std::collections::HashMap;

use num_complex::Complex64;
use crate::{tokenizer::{Token, TokenKind}, variable::Variable};

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
    }
}

#[derive(Debug)]
pub struct EvaluationError {
    pub col: usize,
    pub message: String
}

impl Expr {
    pub fn evaluate(self, variables: &HashMap<&str, Variable>) -> Result<Complex64, EvaluationError> {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.evaluate(variables)?;
                let right = right.evaluate(variables)?;
                match operator.kind {
                    TokenKind::Plus => Ok(left + right),
                    TokenKind::Minus => Ok(left - right),
                    TokenKind::Star => Ok(left * right),
                    TokenKind::Slash => Ok(left / right),
                    TokenKind::Caret => Ok(left.powc(right)),
                    TokenKind::Percent => Ok(left % right),
                    kind => panic!("Invalid token kind for binary operation: {:?}", kind),
                }
            }
            Expr::Unary { operator, right } => {
                let right = right.evaluate(variables)?;
                match operator.kind {
                    TokenKind::Minus => Ok(right * Complex64::new(-1.0, 0.0)),
                    TokenKind::Plus => Ok(right),
                    TokenKind::Bang => {
                        let real_part = right.re;
                        if right.im == 0.0 {
                            if real_part.fract() == 0.0 && real_part >= 0.0 {
                                Ok(factorial(real_part as u64).into())
                            } else {
                                Err(EvaluationError { col: operator.col, message: String::from("Factorial only defined for non-negative real integers") })
                            }
                        } else {
                            Err(EvaluationError { col: operator.col, message: String::from("Factorial only defined for non-negative real integers") })
                        }
                    }
                    kind => panic!("Invalid token kind for unary operation: {:?}", kind),
                }
            }
            Expr::Grouping { expr } => Ok(expr.evaluate(variables)?),
            Expr::Absolute { expr } => Ok(expr.evaluate(variables)?.norm_sqr().into()),
            Expr::Number { number } => Ok(number),
            Expr::Identifier { name } => { 
                if let Some(variable) = variables.get(name.kind.get_lexeme().as_str()) {
                    Ok(variable.value.clone())
                } else {
                    Ok(Complex64::from(0.0))
                }
            }
        }
    }
}

// Computes factorial of a non-negative integer
fn factorial(n: u64) -> f64 {
    (1..=n).map(|x| x as f64).product()
}
