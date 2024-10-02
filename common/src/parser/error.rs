use crate::{
    expr::Expr,
    tokenizer::{Token, TokenKind},
};
use std::fmt;

#[derive(Debug)]
pub enum ParserError {
    ExpectedExpression(Box<ExpectedExpression>),
    ExpectedDelimeter(Box<ExpectedDelimeter>),
    ExpectedToken(Box<ExpectedToken>),
    ExpectedEOF(Box<ExpectedEOF>),
    InvalidAssignmentTarget(Box<InvalidAssignmentTarget>),
    CannotDelete(Box<CannotDelete>),
    InconsistentMatrixRowLength(Box<InconsistentMatrixRowLength>),
}

impl std::error::Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::ExpectedExpression(err) => {
                if let Some(found) = &err.found {
                    write!(
                        f,
                        "Line {}, Position {} :: Expected expression next but found '{}'",
                        found.line, found.col, &found.lexeme
                    )
                } else {
                    write!(f, "Expected expression next but found EOF")
                }
            }
            ParserError::ExpectedDelimeter(err) => {
                if let Some(found) = &err.found {
                    write!(
                        f,
                        "Line {}, Position {} :: Expected delimeter (semicolon or newline) next but found '{}'",
                        found.line,
                        found.col,
                        &found.lexeme
                    )
                } else {
                    write!(f, "Expected delimeter (semicolon or newline) but found EOF")
                }
            }
            ParserError::ExpectedToken(err) => {
                if let Some(found) = &err.found {
                    write!(
                        f,
                        "Line {}, Column {} :: Expected {:?} but found '{}'",
                        found.line, found.col, err.expected, &found.lexeme
                    )
                } else {
                    write!(f, "Expected {:?} but found EOF", err.expected)
                }
            }
            ParserError::ExpectedEOF(err) => {
                write!(f, "Expected EOF but found '{}'", &err.found.lexeme)
            }
            ParserError::InvalidAssignmentTarget(err) => {
                write!(
                    f,
                    "Line {}, Column {} :: Invalid assignment target",
                    err.equal.line, err.equal.col
                )
            }
            ParserError::CannotDelete(err) => write!(
                f,
                "Line {}, Column {} :: Cannot delete {}",
                err.delete.line,
                err.delete.col,
                err.expr.get_type_string()
            ),
            ParserError::InconsistentMatrixRowLength(err) => write!(
                f,
                "Line {}, Column {} :: Row {} has length {}, but every other row previous has length {}",
                err.line,
                err.col,
                err.row,
                err.length_passed,
                err.correct_length
            ),
        }
    }
}

#[derive(Debug)]
pub struct ExpectedExpression {
    pub found: Option<Token>,
}

#[derive(Debug)]
pub struct ExpectedDelimeter {
    pub found: Option<Token>,
}

#[derive(Debug)]
pub struct ExpectedToken {
    pub expected: TokenKind,
    pub found: Option<Token>,
}

#[derive(Debug)]
pub struct ExpectedEOF {
    pub found: Token,
}

#[derive(Debug)]
pub struct InvalidAssignmentTarget {
    pub equal: Token,
}

#[derive(Debug)]
pub struct CannotDelete {
    pub delete: Token,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct InconsistentMatrixRowLength {
    pub line: usize,
    pub col: usize,
    pub row: usize,
    pub correct_length: usize,
    pub length_passed: usize,
}
