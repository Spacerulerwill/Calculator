use rug::{ops::Pow, Complex, Float, Integer};

use crate::tokenizer::{Token, TokenKind};

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
        number: Complex,
    },
    Identifier {
        name: String,
    }
}

#[derive(Debug)]
pub struct EvaluationError {
    pub col: usize,
    pub message: String
}

impl Expr {
    pub fn evaluate(self, precision: u32) -> Result<Complex, EvaluationError> {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.evaluate(precision)?;
                let right = right.evaluate(precision)?;
                match operator.kind {
                    TokenKind::Plus => Ok(left + right),
                    TokenKind::Minus => Ok(left - right),
                    TokenKind::Star => Ok(left * right),
                    TokenKind::Slash => Ok(left / right),
                    TokenKind::Caret => Ok(left.pow(right)),
                    TokenKind::Percent => {
                        if left.imag().is_zero() && right.imag().is_zero() {
                            return Ok(Complex::with_val(precision, left.real() % right.real()));
                        } else {
                            Err(EvaluationError { col: operator.col, message: String::from("Modulus operation is only supported for real numbers") })
                        }
                    }
                    kind => panic!("Invalid token kind for binary operation: {:?}", kind),
                }
            }
            Expr::Unary { operator, right } => {
                let right = right.evaluate(precision)?;
                match operator.kind {
                    TokenKind::Minus => Ok(right * -1),
                    TokenKind::Plus => Ok(right),
                    TokenKind::Bang => {
                        if right.imag().is_zero() {
                            let value = right.real();
                            if value.is_integer() {
                                let integer_val = value.to_integer().unwrap();
                                Ok(Complex::with_val(
                                    precision,
                                    (Expr::factorial_int(integer_val, precision), 0),
                                ))
                            } else {
                                let f = Float::with_val(precision, value + 1.0);
                                Ok(Complex::with_val(precision, (Float::gamma(f), 0)))
                            }
                        } else {
                            Err(EvaluationError { col: operator.col, message: String::from("Gamma function not supported for imaginary numbers") })
                        }
                    }
                    kind => panic!("Invalid token kind for unary operation: {:?}", kind),
                }
            }
            Expr::Grouping { expr } => Ok(expr.evaluate(precision)?),
            Expr::Absolute { expr } => Ok(expr.evaluate(precision)?.abs()),
            Expr::Number { number } => Ok(number),
            Expr::Identifier { name } => Ok(Complex::with_val(precision, (0, 0)))
        }
    }

    fn factorial_int(n: Integer, precision: u32) -> Float {
        let mut result = Float::with_val(precision, 1);
        let mut i = Integer::from(1);

        while i <= n {
            result *= Float::with_val(precision, i.clone());
            i += 1;
        }

        result
    }
}
