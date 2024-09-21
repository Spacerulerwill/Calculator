/*
<statement> ::= <expression_statement>
              | <assignment>
              | <function_declaration>

<expression_statement> ::= <expression> "\n"
<assignment> ::= <IDENTIFIER> "=" <expression> "\n"

<function_declaration> ::= <call> "=" <expression> "\n"

<expression> ::= <term>

<term> ::= <factor> ( ( "-" | "+" ) <factor> )*

<factor> ::= <exponent> ( ( "/" | "*" | "%" ) <exponent> )*

<exponent> ::= <unary> ( "^" <unary> )*

<unary> ::= ( "-" | "√" ) <unary> | <factorial>

<factorial> ::= <call> ( "!" )*

<call> ::= <primary> ( "(" <arguments>? ")" )*

<primary> ::= <NUMBER>
              | <IDENTIFIER>
              | "(" <expression> ")"
              | "|" <expression> "|"
              | "⌈" <expression> "⌉"
              | "⌊" <expression> "⌋"

// helper rules
<arguments> ::= <expression> ( "," <expression> )*
*/

use std::{fmt, iter::Peekable, vec::IntoIter};

use crate::{
    expr::{Expr, GroupingKind}, function::UserDefinedFunctionArgType, stmt::Statement, tokenizer::{Token, TokenKind}
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
    ExpectedEOF {
        found: Token,
    },
    InvalidAssignmentTarget {
        equal: Token,
    },
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::ExpectedExpression { found } => {
                if let Some(found) = found {
                    write!(
                        f,
                        "Position {} :: Expected expression next but found '{}'",
                        found.col,
                        &found.lexeme
                    )
                } else {
                    write!(f, "Expected expression next but found EOF")
                }
            }
            ParserError::ExpectedToken { expected, found } => {
                if let Some(found) = found {
                    write!(
                        f,
                        "Column {} :: Expected {:?} but found '{}'",
                        found.col,
                        expected,
                        found.lexeme
                    )
                } else {
                    write!(f, "Expected {:?} but found EOF", expected)
                }
            }
            ParserError::ExpectedEOF { found } => write!(f, "Expected EOF but found '{}'", &found.lexeme),
            ParserError::InvalidAssignmentTarget { equal } => write!(f, "Column {} :: Invalid assignment target", equal.col)

        }
    }
}

#[derive(Debug)]
pub struct Parser {
    iter: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Vec<Statement>, ParserError> {
        let mut parser = Parser {
            iter: tokens.into_iter().peekable(),
        };
        let mut statements = Vec::new();
        while parser.iter.peek().is_some() {
            statements.push(parser.statement()?);
        }
        Ok(statements)
    }

    fn statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;
    
        match expr.clone() {
            Expr::Identifier { name } => {
                if let Some(statement) = self.try_assignment_statement(name)? {
                    return Ok(statement);
                }
            }
            Expr::Call { callee, paren: _, arguments } => {
                if let Some(statement) = self.try_function_assignment_statement(callee, arguments)? {
                    return Ok(statement);
                }
            }
            _ => {}
        }

        // Expression statement
        self.consume(TokenKind::Newline)?;
        return Ok(Statement::Expression(expr))
    }
    

    fn try_assignment_statement(&mut self, name: Token) -> Result<Option<Statement>, ParserError> {
        if self.check(TokenKind::Equal) {
            self.iter.next();
            let right = self.expression()?;
            self.consume(TokenKind::Newline)?;
            return Ok(Some(Statement::Assignment { identifier: name, expr: right }))
        }
        Ok(None)
    }

    fn try_function_assignment_statement(&mut self, callee: Box<Expr>, args: Vec<Expr>) -> Result<Option<Statement>, ParserError> {
        if self.check(TokenKind::Equal) {
            let equal = self.iter.next().unwrap();
            let function_body = self.expression()?;
            self.consume(TokenKind::Newline)?;
            if let Expr::Identifier { name } = *callee {
                let mut signature = Vec::with_capacity(args.len());
                for arg in args {
                    match arg {
                        Expr::Identifier { name } => signature.push(UserDefinedFunctionArgType::Identifier(name.lexeme)),
                        Expr::Number { number } => signature.push(UserDefinedFunctionArgType::Number(number)),
                        _ => return Err(ParserError::InvalidAssignmentTarget { equal: equal }),
                    }
                }
                return Ok(Some(Statement::FunctionDeclaration { name: name, signature: signature, expr: function_body }))
            }
        } 
        Ok(None)
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
                TokenKind::Minus | TokenKind::Sqrt => {
                    let token = self.iter.next().unwrap();
                    let operand = self.unary()?;
                    return Ok(Expr::Unary {
                        operator: token,
                        operand: Box::new(operand),
                    });
                }
                _ => {}
            }
        }
        self.factorial()
    }

    fn factorial(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.call()?;

        while let Some(token) = self.iter.peek() {
            if token.kind == TokenKind::Bang {
                let token = self.iter.next().unwrap();
                expr = Expr::Unary {
                    operator: token,
                    operand: Box::new(expr),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;
        while self.check(TokenKind::LeftParen) {
            let token = self.iter.next().unwrap();
            expr = self.finish_call(expr, token)?;
        }
        Ok(expr)
    }

    fn consume_comma_seperated_arguments(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut arguments = Vec::new();
        if !self.check(TokenKind::RightParen) {
            loop {
                arguments.push(self.expression()?);
                if self.check(TokenKind::Comma) {
                    self.iter.next();
                } else {
                    break;
                }
            }
        }
        self.consume(TokenKind::RightParen)?;
        Ok(arguments)
    }

    fn finish_call(&mut self, callee: Expr, left_paren: Token) -> Result<Expr, ParserError> {
        let arguments = self.consume_comma_seperated_arguments()?;
        Ok(Expr::Call {
            callee: Box::new(callee),
            paren: left_paren,
            arguments: arguments,
        })
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.iter.next() {
            match token.kind {
                TokenKind::Number(num) => return Ok(Expr::Number { number: num }),
                TokenKind::Identifier(_) => return Ok(Expr::Identifier { name: token }),
                TokenKind::LeftParen => {
                    let expr = self.expression()?;
                    self.consume(TokenKind::RightParen)?;
                    return Ok(Expr::Grouping {
                        paren: token,
                        kind: GroupingKind::Grouping,
                        expr: Box::new(expr),
                    });
                }
                TokenKind::Pipe => {
                    let expr = self.expression()?;
                    self.consume(TokenKind::Pipe)?;
                    return Ok(Expr::Grouping {
                        paren: token,
                        kind: GroupingKind::Absolute,
                        expr: Box::new(expr),
                    });
                }
                TokenKind::LeftCeiling => {
                    let expr = self.expression()?;
                    self.consume(TokenKind::RightCeiling)?;
                    return Ok(Expr::Grouping {
                        paren: token,
                        kind: GroupingKind::Ceil,
                        expr: Box::new(expr),
                    });
                }
                TokenKind::LeftFloor => {
                    let expr = self.expression()?;
                    self.consume(TokenKind::RightFloor)?;
                    return Ok(Expr::Grouping {
                        paren: token,
                        kind: GroupingKind::Floor,
                        expr: Box::new(expr),
                    });
                }
                _ => return Err(ParserError::ExpectedExpression { found: Some(token) }),
            }
        } else {
            return Err(ParserError::ExpectedExpression { found: None });
        }
    }

    fn consume(&mut self, expected_kind: TokenKind) -> Result<Token, ParserError> {
        if let Some(token) = self.iter.peek() {
            if token.kind == expected_kind {
                let token = self.iter.next().unwrap();
                Ok(token)
            } else {
                Err(ParserError::ExpectedToken {
                    expected: expected_kind,
                    found: Some(token.clone()),
                })
            }
        } else {
            Err(ParserError::ExpectedToken {
                expected: expected_kind,
                found: None,
            })
        }
    }

    fn check(&mut self, expected_kind: TokenKind) -> bool {
        match self.iter.peek() {
            Some(token) => token.kind == expected_kind,
            None => false,
        }
    }
}
