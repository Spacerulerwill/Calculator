/*
expression → term ";" ;
term → factor ( ( "-" | "+" ) factor )* ;
factor → exponent ( ( "/" | "*" ) exponent )* ;
exponent → unary ( "^" unary )* ;
unary → ( "-" | "+" ) unary | factorial ;
factorial → primary "!" | primary ;
primary → NUMBER | "(" expression ")" | "|" expression "|";
*/

use std::{iter::Peekable, slice::Iter};

use crate::{
    expr::Expr,
    tokenizer::{Token, TokenKind},
};

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
pub struct Parser<'a> {
    iter: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn parse(tokens: Vec<Token>) -> Result<Expr, ParserError> {
        let mut parser = Parser {
            iter: tokens.iter().peekable(),
        };

        parser.expression()
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.term()
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;

        while let Some(&token) = self.iter.peek() {
            match token.kind {
                TokenKind::Minus | TokenKind::Plus => {
                    self.iter.next();
                    let operator: TokenKind = token.kind.clone();
                    let right = self.factor()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator,
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
        while let Some(&token) = self.iter.peek() {
            match token.kind {
                TokenKind::Star | TokenKind::Slash => {
                    self.iter.next();
                    let operator: TokenKind = token.kind.clone();
                    let right = self.exponent()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator,
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

        while let Some(&token) = self.iter.peek() {
            if token.kind == TokenKind::Caret {
                self.iter.next();
                let right = self.exponent()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    operator: TokenKind::Caret,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(&token) = self.iter.peek() {
            match token.kind {
                TokenKind::Plus | TokenKind::Minus => {
                    self.iter.next();
                    let operator = token.kind.clone();
                    let right = self.unary()?;
                    return Ok(Expr::Unary {
                        operator: operator,
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

        while let Some(&token) = self.iter.peek() {
            if token.kind == TokenKind::Bang {
                self.iter.next();
                expr = Expr::Unary {
                    operator: TokenKind::Bang,
                    right: Box::new(expr),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        // Is it a number?
        if let Some(Token {
            kind: TokenKind::Number(num),
            ..
        }) = self.iter.peek()
        {
            self.iter.next();
            return Ok(Expr::Number {
                number: num.clone(),
            });
        }

        // Left parenthesis is a start of grouping - parse expression and consume a right parenthesis
        if let Some(Token {
            kind: TokenKind::LeftParen,
            ..
        }) = self.iter.peek()
        {
            self.iter.next();
            let expr = self.expression()?;
            self.consume(TokenKind::RightParen)?;
            return Ok(Expr::Grouping {
                expr: Box::new(expr),
            });
        }

        // Pipe is start of absolute grouping - parse expression and another pipe
        if let Some(Token {
            kind: TokenKind::Pipe,
            ..
        }) = self.iter.peek()
        {
            self.iter.next();
            let expr = self.expression()?;
            self.consume(TokenKind::Pipe)?;
            return Ok(Expr::Absolute { expr: Box::new(expr) })
        }

        // We expected an expression, but did not find one
        if let Some(&token) = self.iter.peek() {
            return Err(ParserError::ExpectedExpression {
                found: Some(token.clone()),
            });
        } else {
            return Err(ParserError::ExpectedExpression { found: None });
        }
    }

    fn consume(&mut self, expected_kind: TokenKind) -> Result<Token, ParserError> {
        // Peek at the next token
        if let Some(&token) = self.iter.peek() {
            // Check if the token matches the expected kind
            if token.kind == expected_kind {
                // Consume the token by advancing the iterator
                self.iter.next();
                Ok(token.clone())
            } else {
                // Create and return an error indicating the mismatch
                Err(ParserError::ExpectedToken {
                    expected: expected_kind,
                    found: Some(token.clone()),
                })
            }
        } else {
            // No token found when one was expected
            Err(ParserError::ExpectedToken {
                expected: expected_kind,
                found: None,
            })
        }
    }
}
