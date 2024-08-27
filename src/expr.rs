use rug::{ops::Pow, Float, Integer};

use crate::tokenizer::TokenKind;

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: TokenKind,
        right: Box<Expr>,
    },
    Unary {
        operator: TokenKind,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Absolute {
        expr: Box<Expr>,
    },
    Number {
        number: Float,
    },
}

impl Expr {
    pub fn evaluate(self, precision: u32) -> Float {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => match operator {
                TokenKind::Plus => left.evaluate(precision) + right.evaluate(precision),
                TokenKind::Minus => left.evaluate(precision) - right.evaluate(precision),
                TokenKind::Star => left.evaluate(precision) * right.evaluate(precision),
                TokenKind::Slash => left.evaluate(precision) / right.evaluate(precision),
                TokenKind::Caret => left.evaluate(precision).pow(right.evaluate(precision)),
                kind => panic!("Invalid token kind for binary operation: {:?}", kind),
            },
            Expr::Unary { operator, right } => match operator {
                TokenKind::Minus => right.evaluate(precision) * -1,
                TokenKind::Plus => right.evaluate(precision),
                TokenKind::Bang => {
                    let value = right.evaluate(precision);
                    if value.is_integer() {
                        let integer_val = value.to_integer().unwrap();
                        Expr::factorial_int(integer_val)
                    } else {
                        let f = Float::with_val(precision, value + 1.0);
                        Float::gamma(f)
                    }
                }
                kind => panic!("Invalid token kind for unary operation: {:?}", kind),
            },
            Expr::Grouping { expr } => expr.evaluate(precision),
            Expr::Absolute { expr } => expr.evaluate(precision).abs(),
            Expr::Number { number } => number,
        }
    }

    fn factorial_int(n: Integer) -> Float {
        let mut result = Float::with_val(53, 1);
        let mut i = Integer::from(1);

        while i <= n {
            result *= Float::with_val(53, i.clone());
            i += 1;
        }

        result
    }
}
