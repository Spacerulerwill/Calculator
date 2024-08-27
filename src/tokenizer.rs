use std::{iter::Peekable, str::Chars};

use rug::{Assign, Float};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Slash,
    Star,
    Caret,
    Bang,
    Number(Float),
}

impl TokenKind {
    pub fn get_lexeme(&self) -> String {
        match &self {
            TokenKind::LeftParen => String::from("("),
            TokenKind::RightParen => String::from(")"),
            TokenKind::Plus => String::from("+"),
            TokenKind::Minus => String::from("-"),
            TokenKind::Slash => String::from("/"),
            TokenKind::Star => String::from("*"),
            TokenKind::Caret => String::from("^"),
            TokenKind::Bang => String::from("!"),
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
    precision: u32,
}

impl<'a> Tokenizer<'a> {
    pub fn tokenize(input: &'a str, tabsize: u8, precision: u32) -> Result<Self, TokenizerError> {
        let mut tokenizer = Tokenizer {
            iter: input.chars().peekable(),
            prev_col: 1,
            current_col: 1,
            tokens: Vec::new(),
            tabsize: tabsize,
            precision: precision,
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
                '+' => self.add_single_char_token(TokenKind::Plus),
                '-' => self.add_single_char_token(TokenKind::Minus),
                '/' => self.add_single_char_token(TokenKind::Slash),
                '*' => self.add_single_char_token(TokenKind::Star),
                '^' => self.add_single_char_token(TokenKind::Caret),
                '!' => self.add_single_char_token(TokenKind::Bang),
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

    fn consume_digits(&mut self) -> String {
        let mut string = String::new();
        while let Some(&ch) = self.iter.peek() {
            if !ch.is_digit(10) {
                break;
            }
            string.push(ch);
            self.next();
        }
        string
    }

    fn tokenize_number(&mut self) {
        // Consume digits before decimal point
        let mut number_string = self.consume_digits();
        // Consume decimal point if exists,
        let iter_save = self.iter.clone();
        let current_col_save = self.current_col.clone();
        if self.iter.peek() == Some(&'.') {
            self.next();
            let post_dot_digit = self.consume_digits();
            if post_dot_digit.is_empty() {
                self.iter = iter_save;
                self.current_col = current_col_save;
            } else {
                number_string.push('.');
                number_string.push_str(post_dot_digit.as_str());
            }
        }
        let mut num = Float::new(self.precision);
        num.assign(Float::parse_radix(&number_string, 10).unwrap());
        self.add_token(TokenKind::Number(num));
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
