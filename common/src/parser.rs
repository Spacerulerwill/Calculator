/*
<program>  ::= ( <statement>? <delimeter> )*

<delimeter> ::= "\n" | ";"
<statement> ::= <expression_statement>
              | <delete_statement>
              | <assignment>
              | <function_declaration>

<expression_statement> ::= <expression>

<delete_statement> ::= "delete" (<IDENTIFIER> | <call>)

<assignment> ::= <IDENTIFIER> "=" <expression>

<function_declaration> ::= <call> "=" <expression>

<expression> ::= <term>

<term> ::= <factor> ( ( "-" | "+" ) <factor> )*

<factor> ::= <dot> ( ( "/" | "*" | "%" | "dot" ) <dot> )*

<dot> ::= <cross> ( "dot" <cross> )*

<cross> ::= <exponent> ( "cross" <exponent>)*

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
              | "[" <arguments> "]"

// helper rules
<arguments> ::= <expression> ( "," <expression> )*
*/

use std::{fmt, iter::Peekable, vec::IntoIter};

use crate::{
    expr::{Expr, GroupingKind},
    function::Signature,
    stmt::Statement,
    tokenizer::{Token, TokenKind},
};

#[derive(Debug)]
pub enum ParserError {
    ExpectedExpression {
        found: Option<Token>,
    },
    ExpectedDelimeter {
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
    CannotDelete(Token, Expr),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::ExpectedExpression { found } => {
                if let Some(found) = found {
                    write!(
                        f,
                        "Line {}, Position {} :: Expected expression next but found '{}'",
                        found.line, found.col, &found.lexeme
                    )
                } else {
                    write!(f, "Expected expression next but found EOF")
                }
            }
            ParserError::ExpectedDelimeter { found } => {
                if let Some(found) = found {
                    write!(
                        f,
                        "Line {}, Position {} :: Expected demileter (semicolon or newline) next but found '{}'",
                        found.line,
                        found.col,
                        &found.lexeme
                    )
                } else {
                    write!(f, "Expected delimeter (semicolon or newline) but found EOF")
                }
            }
            ParserError::ExpectedToken { expected, found } => {
                if let Some(found) = found {
                    write!(
                        f,
                        "Line {}, Column {} :: Expected {:?} but found '{}'",
                        found.line, found.col, expected, found.lexeme
                    )
                } else {
                    write!(f, "Expected {:?} but found EOF", expected)
                }
            }
            ParserError::ExpectedEOF { found } => {
                write!(f, "Expected EOF but found '{}'", &found.lexeme)
            }
            ParserError::InvalidAssignmentTarget { equal } => {
                write!(
                    f,
                    "Line {}, Column {} :: Invalid assignment target",
                    equal.line, equal.col
                )
            }
            ParserError::CannotDelete(token, expr) => write!(
                f,
                "Line {}, Column {} :: Cannot delete {}",
                token.line,
                token.col,
                expr.get_type_string()
            ),
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
        while let Some(token) = parser.iter.peek() {
            match token.kind {
                TokenKind::Newline | TokenKind::Semicolon => {
                    parser.iter.next();
                }
                _ => statements.push(parser.statement()?),
            }
        }
        Ok(statements)
    }

    fn statement(&mut self) -> Result<Statement, ParserError> {
        if self.check(TokenKind::Delete) {
            return self.delete_statement();
        }

        let expr = self.expression()?;
        match expr.clone() {
            Expr::Identifier { name } => {
                if let Some(statement) = self.assignment(name)? {
                    return Ok(statement);
                }
            }
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                if let Some(statement) = self.function_declaration(*callee, arguments)? {
                    return Ok(statement);
                }
            }
            _ => {}
        }

        self.expression_statement(expr)
    }

    fn expression_statement(&mut self, expr: Expr) -> Result<Statement, ParserError> {
        self.consume_line_delimeter()?;
        Ok(Statement::ExpressionStatement(expr))
    }

    fn delete_statement(&mut self) -> Result<Statement, ParserError> {
        let delete = self.iter.next().unwrap();
        let expr = self.expression()?;
        match expr.clone() {
            Expr::Identifier { name } => {
                self.consume_line_delimeter()?;
                return Ok(Statement::DeleteVariable(name));
            }
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                self.consume_line_delimeter()?;
                match Signature::from_call_expression(*callee, arguments) {
                    Ok((name, signature)) => {
                        return Ok(Statement::DeleteFunctionSignature {
                            name: name,
                            signature: signature,
                        })
                    }
                    Err(_) => return Err(ParserError::CannotDelete(delete, expr)),
                }
            }
            expr => return Err(ParserError::CannotDelete(delete, expr)),
        }
    }

    fn assignment(&mut self, name: Token) -> Result<Option<Statement>, ParserError> {
        if self.check(TokenKind::Equal) {
            self.iter.next();
            let right = self.expression()?;
            self.consume_line_delimeter()?;
            return Ok(Some(Statement::Assignment {
                identifier: name,
                expr: right,
            }));
        }
        Ok(None)
    }

    fn function_declaration(
        &mut self,
        callee: Expr,
        arguments: Vec<Expr>,
    ) -> Result<Option<Statement>, ParserError> {
        if self.check(TokenKind::Equal) {
            let equal = self.iter.next().unwrap();
            let function_body = self.expression()?;
            self.consume_line_delimeter()?;
            let (name, signature) = match Signature::from_call_expression(callee, arguments) {
                Ok(result) => result,
                Err(_) => return Err(ParserError::InvalidAssignmentTarget { equal: equal }),
            };
            return Ok(Some(Statement::FunctionDeclaration {
                name: name,
                signature: signature,
                expr: function_body,
            }));
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
        let mut expr = self.dot()?;
        while let Some(token) = self.iter.peek() {
            match token.kind {
                TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
                    let token = self.iter.next().unwrap();
                    let right = self.dot()?;
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

    fn dot(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.cross()?;
        while let Some(token) = self.iter.peek() {
            match token.kind {
                TokenKind::Dot => {
                    let token = self.iter.next().unwrap();
                    let right = self.cross()?;
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

    fn cross(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.exponent()?;
        while let Some(token) = self.iter.peek() {
            match token.kind {
                TokenKind::Cross => {
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
        Ok(arguments)
    }

    fn finish_call(&mut self, callee: Expr, left_paren: Token) -> Result<Expr, ParserError> {
        let arguments = self.consume_comma_seperated_arguments()?;
        self.consume(TokenKind::RightParen)?;
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
                TokenKind::LeftBracket => {
                    let experssions = self.consume_comma_seperated_arguments()?;
                    self.consume(TokenKind::RightBracket)?;
                    return Ok(Expr::Vector(experssions));
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

    fn consume_line_delimeter(&mut self) -> Result<Token, ParserError> {
        if let Some(token) = self.iter.peek() {
            match token.kind {
                TokenKind::Newline | TokenKind::Semicolon => {
                    let token = self.iter.next().unwrap();
                    Ok(token)
                }
                _ => Err(ParserError::ExpectedDelimeter {
                    found: Some(token.clone()),
                }),
            }
        } else {
            Err(ParserError::ExpectedDelimeter { found: None })
        }
    }

    fn check(&mut self, expected_kind: TokenKind) -> bool {
        match self.iter.peek() {
            Some(token) => token.kind == expected_kind,
            None => false,
        }
    }
}
