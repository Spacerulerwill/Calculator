use num_complex::Complex64;

use crate::variable::value::unit::Unit;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Groupings
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftCeiling,
    RightCeiling,
    LeftFloor,
    RightFloor,
    // Operators
    Plus,
    Minus,
    Slash,
    Star,
    Caret,
    Bang,
    Pipe,
    Percent,
    Comma,
    Equal,
    Sqrt,
    Dot,
    Cross,
    // Line delimeters
    Newline,
    Semicolon,
    // Keywords
    Delete,
    Clear,
    As,
    // Other
    Identifier(String),
    Number(Complex64),
    Unit(Unit),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct TokenPosition {
    pub line: usize,
    pub col: usize,
    pub idx: usize,
}
