use crate::num_complex::{Complex, Complex64};
use std::{
    iter::Peekable,
    str::{Chars, FromStr},
};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftCeiling,
    RightCeiling,
    LeftFloor,
    RightFloor,
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
    Identifier(String),
    Number(Complex64),
}

impl TokenKind {
    pub fn get_lexeme(&self) -> String {
        match &self {
            TokenKind::LeftParen => String::from("("),
            TokenKind::RightParen => String::from(")"),
            TokenKind::LeftCeiling => String::from("⌈"),
            TokenKind::RightCeiling => String::from("⌉"),
            TokenKind::LeftFloor => String::from("⌊"),
            TokenKind::RightFloor => String::from("⌋"),
            TokenKind::Plus => String::from("+"),
            TokenKind::Minus => String::from("-"),
            TokenKind::Slash => String::from("/"),
            TokenKind::Star => String::from("*"),
            TokenKind::Caret => String::from("^"),
            TokenKind::Bang => String::from("!"),
            TokenKind::Pipe => String::from("|"),
            TokenKind::Percent => String::from("%"),
            TokenKind::Comma => String::from(","),
            TokenKind::Equal => String::from("="),
            TokenKind::Sqrt => String::from("√"),
            TokenKind::Identifier(indentifier) => indentifier.clone(),
            TokenKind::Number(number) => number.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub col: usize,
}

#[derive(Debug)]
pub enum TokenizerError {
    BadChar(char, usize),
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    iter: Peekable<Chars<'a>>,
    prev_col: usize,
    current_col: usize,
    pub tokens: Vec<Token>,
    tabsize: u8,
}

impl<'a> Tokenizer<'a> {
    pub fn tokenize(input: &'a str, tabsize: u8) -> Result<Self, TokenizerError> {
        let mut tokenizer = Tokenizer {
            iter: input.chars().peekable(),
            prev_col: 1,
            current_col: 1,
            tokens: Vec::new(),
            tabsize: tabsize,
        };
        tokenizer.tokenize_internal()?;
        Ok(tokenizer)
    }

    pub fn tokenize_internal(&mut self) -> Result<(), TokenizerError> {
        while let Some(&ch) = self.iter.peek() {
            match ch {
                ' ' | '\t' => {
                    self.next();
                }
                '(' => self.add_single_char_token(TokenKind::LeftParen),
                ')' => self.add_single_char_token(TokenKind::RightParen),
                '⌈' => self.add_single_char_token(TokenKind::LeftCeiling),
                '⌉' => self.add_single_char_token(TokenKind::RightCeiling),
                '⌊' => self.add_single_char_token(TokenKind::LeftFloor),
                '⌋' => self.add_single_char_token(TokenKind::RightFloor),
                '+' => self.add_single_char_token(TokenKind::Plus),
                '-' => self.add_single_char_token(TokenKind::Minus),
                '/' => self.add_single_char_token(TokenKind::Slash),
                '*' => self.add_single_char_token(TokenKind::Star),
                '^' => self.add_single_char_token(TokenKind::Caret),
                '!' => self.add_single_char_token(TokenKind::Bang),
                '|' => self.add_single_char_token(TokenKind::Pipe),
                '%' => self.add_single_char_token(TokenKind::Percent),
                ',' => self.add_single_char_token(TokenKind::Comma),
                '=' => self.add_single_char_token(TokenKind::Equal),
                '√' => self.add_single_char_token(TokenKind::Sqrt),
                'a'..='z' | 'A'..='Z' | '_' | 'π' | 'ϕ' => self.tokenize_identifier(),
                '0'..='9' => self.tokenize_number(),
                _ => return Err(TokenizerError::BadChar(ch, self.current_col)),
            }
        }
        Ok(())
    }

    fn add_single_char_token(&mut self, kind: TokenKind) {
        self.next();
        self.add_token(kind);
    }

    fn consume_while<F>(&mut self, condition: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut string = String::new();
        while let Some(&ch) = self.iter.peek() {
            if !condition(ch) {
                break;
            }
            string.push(ch);
            self.next();
        }
        string
    }

    fn tokenize_number(&mut self) {
        // Consume digits before decimal point
        let mut number_string = self.consume_while(|ch| ch.is_digit(10));
        // Consume decimal point if exists,
        let iter_save = self.iter.clone();
        let current_col_save = self.current_col.clone();
        if self.iter.peek() == Some(&'.') {
            self.next();
            let post_dot_digit = self.consume_while(|ch| ch.is_digit(10));
            if post_dot_digit.is_empty() {
                self.iter = iter_save;
                self.current_col = current_col_save;
            } else {
                number_string.push('.');
                number_string.push_str(post_dot_digit.as_str());
            }
        }
        let num = Complex::new(f64::from_str(&number_string).unwrap(), 0.0);
        self.add_token(TokenKind::Number(num));
    }

    fn tokenize_identifier(&mut self) {
        let identifier = self.consume_while(|ch| ch.is_alphanumeric() || ch == '_');
        self.add_token(TokenKind::Identifier(identifier));
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind: kind,
            col: self.prev_col,
        });
        self.prev_col = self.current_col;
    }

    fn next(&mut self) -> Option<char> {
        if let Some(ch) = self.iter.next() {
            match ch {
                '\t' => self.current_col += self.tabsize as usize,
                _ => self.current_col += 1,
            }
            return Some(ch);
        }
        return None;
    }
}
