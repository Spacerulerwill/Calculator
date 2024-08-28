/*
expression → term ";" ;
term → factor ( ( "-" | "+" ) factor )* ;
factor → exponent ( ( "/" | "*" | "%" ) exponent )* ;
exponent → unary ( "^" unary )* ;
unary →  "-" unary | factorial ;
factorial → primary "!" | primary ;
primary → NUMBER | "i" | NUMBER "i" | IDENTIFIER | "(" expression ")" | "|" expression "|" ;*/

use std::{iter::Peekable, vec::IntoIter};

use crate::{
    expr::Expr,
    tokenizer::{Token, TokenKind},
};
use rug::Complex;

#[derive(Debug)]
pub enum ParserError {
    ExpectedExpression {
        found: Option<Token>,
    },
    ExpectedToken {
        expected: TokenKind,
        found: Option<Token>,
    },
}

#[derive(Debug)]
pub struct Parser {
    iter: Peekable<IntoIter<Token>>,
    precision: u32,
}

impl Parser {
    pub fn parse(tokens: Vec<Token>, precision: u32) -> Result<Expr, ParserError> {
        let mut parser = Parser {
            iter: tokens.into_iter().peekable(),
            precision: precision,
        };

        parser.expression()
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.term()
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;

        while let Some(token) = self.iter.peek() {
            match token.kind {
                TokenKind::Minus | TokenKind::Plus => {
                    let token = self.iter.next().unwrap();
                    let right = self.factor()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator: token,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.exponent()?;
        while let Some(token) = self.iter.peek() {
            match token.kind {
                TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
                    let token = self.iter.next().unwrap();
                    let right = self.exponent()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator: token,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn exponent(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let Some(token) = self.iter.peek() {
            if token.kind == TokenKind::Caret {
                let token = self.iter.next().unwrap();
                let right = self.exponent()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    operator: token,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.iter.peek() {
            match token.kind {
                TokenKind::Minus => {
                    let token = self.iter.next().unwrap();
                    let right = self.unary()?;
                    return Ok(Expr::Unary {
                        operator: token,
                        right: Box::new(right),
                    });
                }
                _ => {}
            }
        }
        self.factorial()
    }

    fn factorial(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;

        while let Some(token) = self.iter.peek() {
            if token.kind == TokenKind::Bang {
                let token = self.iter.next().unwrap();
                expr = Expr::Unary {
                    operator: token,
                    right: Box::new(expr),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.iter.next() {
            match token.kind {
                TokenKind::Number(num) => {
                    // Is there an ImaginryUnit following?
                    if let Some(Token {
                        kind: TokenKind::ImaginaryUnit,
                        ..
                    }) = self.iter.peek()
                    {
                        self.iter.next();
                        return Ok(Expr::Number {
                            number: num * Complex::with_val(self.precision, (0, 1)),
                        });
                    } else {
                        return Ok(Expr::Number {
                            number: num,
                        });
                    }
                },
                TokenKind::ImaginaryUnit => return Ok(Expr::Number { number: Complex::with_val(self.precision, (0, 1)) }),
                TokenKind::Identifier(identifier) => return Ok(Expr::Identifier { name: identifier }),
                TokenKind::LeftParen => {
                    let expr = self.expression()?;
                    self.consume(TokenKind::RightParen)?;
                    return Ok(Expr::Grouping {
                        expr: Box::new(expr),
                    })
                },
                TokenKind::Pipe => {
                    let expr = self.expression()?;
                    self.consume(TokenKind::Pipe)?;
                    return Ok(Expr::Absolute {
                        expr: Box::new(expr),
                    });
                },
                _ => return Err(ParserError::ExpectedExpression { found: Some(token)}),
            }
        } else {
            return Err(ParserError::ExpectedExpression { found: None });
        }
    }

    fn consume(&mut self, expected_kind: TokenKind) -> Result<Token, ParserError> {
        if let Some(token) = self.iter.next() {
            if token.kind == expected_kind {
                Ok(token)
            } else {
                Err(ParserError::ExpectedToken {
                    expected: expected_kind,
                    found: Some(token),
                })
            }
        } else {
            Err(ParserError::ExpectedToken {
                expected: expected_kind,
                found: None,
            })
        }
    }
}
