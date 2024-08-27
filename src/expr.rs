use rug::{ops::Pow, Float};

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
    Number {
        number: Float,
    },
}

impl Expr {
    pub fn evaluate(self) -> Float {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => match operator {
                TokenKind::Plus => left.evaluate() + right.evaluate(),
                TokenKind::Minus => left.evaluate() - right.evaluate(),
                TokenKind::Star => left.evaluate() * right.evaluate(),
                TokenKind::Slash => left.evaluate() / right.evaluate(),
                TokenKind::Caret => left.evaluate().pow(right.evaluate()),
                kind => panic!("Invalid token kind for binary operation: {:?}", kind),
            },
            Expr::Unary { operator, right } => match operator {
                TokenKind::Minus => right.evaluate() * -1,
                TokenKind::Plus => right.evaluate(),
                kind => panic!("Invalid token kind for unary operation: {:?}", kind),
            },
            Expr::Grouping { expr } => expr.evaluate(),
            Expr::Number { number } => number,
        }
    }
}
