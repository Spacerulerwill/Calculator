use crate::num_complex::{Complex, Complex64};
use std::fmt;
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
    Newline,
    Identifier(String),
    Number(Complex64),
    Delete,
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

#[derive(Debug, PartialEq)]
pub enum TokenizerError {
    BadChar { line: usize, col: usize, char: char },
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenizerError::BadChar { line, col, char } => write!(
                f,
                "Line {line}, Column {col} :: Unexpected or invalid character '{char}'"
            ),
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    input: &'a str,
    iter: Peekable<Chars<'a>>,
    prev_pos: TokenPosition,
    current_pos: TokenPosition,
    pub tokens: Vec<Token>,
    tabsize: u8,
}

impl<'a> Tokenizer<'a> {
    pub fn tokenize(input: &'a str, tabsize: u8) -> Result<Self, TokenizerError> {
        let mut tokenizer = Tokenizer {
            input: input,
            iter: input.chars().peekable(),
            prev_pos: TokenPosition {
                line: 1,
                col: 1,
                idx: 0,
            },
            current_pos: TokenPosition {
                line: 1,
                col: 1,
                idx: 0,
            },
            tokens: Vec::new(),
            tabsize: tabsize,
        };
        tokenizer.tokenize_internal()?;
        Ok(tokenizer)
    }

    fn get_keyword_token_kind(lexeme: &str) -> Option<TokenKind> {
        match lexeme {
            "delete" => Some(TokenKind::Delete),
            _ => None,
        }
    }

    pub fn tokenize_internal(&mut self) -> Result<(), TokenizerError> {
        while let Some(&ch) = self.iter.peek() {
            match ch {
                ' ' | '\t' | '\r' => {
                    self.next();
                    self.prev_pos = self.current_pos.clone();
                }
                '\n' => self.add_single_char_token(TokenKind::Newline),
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
                ch => {
                    return Err(TokenizerError::BadChar {
                        line: self.current_pos.line,
                        col: self.current_pos.col,
                        char: ch,
                    })
                }
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

    fn consume_exponent_for_number(&mut self, number_string: &mut String) -> bool {
        // Save position at start
        let iter_save = self.iter.clone();
        let current_pos_save = self.current_pos.clone();

        if self.iter.peek() == Some(&'e') {
            let exponent_char = self.next().unwrap();
            let mut has_minus = false;
            // Could be a minus symbol
            if self.iter.peek() == Some(&'-') {
                has_minus = true;
                self.next();
            }
            let exponent = self.consume_while(|ch| ch.is_numeric());
            if exponent.is_empty() {
                self.iter = iter_save;
                self.current_pos = current_pos_save;
                return false;
            }
            number_string.push(exponent_char);
            if has_minus {
                number_string.push('-');
            }
            number_string.push_str(&exponent);
            return true;
        }

        self.iter = iter_save;
        self.current_pos = current_pos_save;
        false
    }

    fn tokenize_number(&mut self) {
        // Consume digits before decimal point
        let mut number_string = self.consume_while(|ch| ch.is_digit(10));
        // Consume and exponent and if there is one we stop here
        if self.consume_exponent_for_number(&mut number_string) {
            let num = Complex::new(f64::from_str(&number_string).unwrap(), 0.0);
            self.add_token(TokenKind::Number(num));
            return;
        }
        // Consume decimal point if exists,
        let iter_save = self.iter.clone();
        let current_col_save = self.current_pos.clone();
        if self.iter.peek() == Some(&'.') {
            self.next();
            let post_dot_digit = self.consume_while(|ch| ch.is_digit(10));
            if post_dot_digit.is_empty() {
                self.iter = iter_save;
                self.current_pos = current_col_save;
            } else {
                number_string.push('.');
                number_string.push_str(post_dot_digit.as_str());
            }
        }
        // Finally try and consume and exponent
        let _ = self.consume_exponent_for_number(&mut number_string);
        // Add token
        let num = Complex::new(f64::from_str(&number_string).unwrap(), 0.0);
        self.add_token(TokenKind::Number(num));
    }

    fn tokenize_identifier(&mut self) {
        let identifier = self.consume_while(|ch| ch.is_alphanumeric() || ch == '_');
        if let Some(keyword) = Self::get_keyword_token_kind(&identifier) {
            self.add_token(keyword);
        } else {
            self.add_token(TokenKind::Identifier(identifier));
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        let mut lexeme = self.input[self.prev_pos.idx..self.current_pos.idx].to_string();
        if lexeme == "\n" {
            lexeme = String::from("\\n");
        }
        self.tokens.push(Token {
            kind: kind,
            lexeme: lexeme,
            line: self.prev_pos.line,
            col: self.prev_pos.col,
        });
        self.prev_pos = self.current_pos.clone();
    }

    fn next(&mut self) -> Option<char> {
        if let Some(ch) = self.iter.next() {
            match ch {
                '\n' => {
                    self.current_pos.line += 1;
                    self.current_pos.col = 1;
                }
                '\t' => self.current_pos.col += self.tabsize as usize,
                _ => self.current_pos.col += 1,
            }
            self.current_pos.idx += ch.len_utf8();
            return Some(ch);
        }
        return None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn extract_token_types(tokens: Vec<Token>) -> Vec<TokenKind> {
        tokens.into_iter().map(|token| token.kind).collect()
    }

    fn extract_lexemes<'a>(tokens: &'a Vec<Token>) -> Vec<&'a str> {
        tokens
            .into_iter()
            .map(|token| token.lexeme.as_str())
            .collect()
    }

    fn extract_token_positions(tokens: Vec<Token>) -> Vec<(usize, usize)> {
        tokens
            .into_iter()
            .map(|token| (token.line, token.col))
            .collect()
    }

    #[test]
    fn test_all_valid_tokens() {
        for (input, result) in [
            ("(", TokenKind::LeftParen),
            (")", TokenKind::RightParen),
            ("⌈", TokenKind::LeftCeiling),
            ("⌉", TokenKind::RightCeiling),
            ("⌊", TokenKind::LeftFloor),
            ("⌋", TokenKind::RightFloor),
            ("+", TokenKind::Plus),
            ("-", TokenKind::Minus),
            ("/", TokenKind::Slash),
            ("*", TokenKind::Star),
            ("^", TokenKind::Caret),
            ("!", TokenKind::Bang),
            ("|", TokenKind::Pipe),
            ("%", TokenKind::Percent),
            (",", TokenKind::Comma),
            ("=", TokenKind::Equal),
            ("√", TokenKind::Sqrt),
            ("foo", TokenKind::Identifier(String::from("foo"))),
            ("12", TokenKind::Number(Complex64::new(12.0, 0.0))),
            ("12.5", TokenKind::Number(Complex64::new(12.5, 0.0))),
            ("0.5", TokenKind::Number(Complex64::new(0.5, 0.0))),
            ("1e6", TokenKind::Number(Complex64::new(1e6 as f64, 0.0))),
            ("1e-6", TokenKind::Number(Complex64::new(1e-6 as f64, 0.0))),
            (
                "2.5e6",
                TokenKind::Number(Complex64::new(2.5e6 as f64, 0.0)),
            ),
            (
                "2.5e-6",
                TokenKind::Number(Complex64::new(2.5e-6 as f64, 0.0)),
            ),
            ("delete", TokenKind::Delete),
        ] {
            let tokens = Tokenizer::tokenize(input, 4).unwrap().tokens;
            assert_eq!(extract_token_types(tokens), vec![result]);
        }
    }

    #[test]
    fn test_samples() {
        for (input, expected_tokens) in [
            (
                "(√(5 + 3) * 2 - foo) / ⌊bar⌋",
                vec![
                    TokenKind::LeftParen,
                    TokenKind::Sqrt,
                    TokenKind::LeftParen,
                    TokenKind::Number(Complex64::new(5.0, 0.0)),
                    TokenKind::Plus,
                    TokenKind::Number(Complex64::new(3.0, 0.0)),
                    TokenKind::RightParen,
                    TokenKind::Star,
                    TokenKind::Number(Complex64::new(2.0, 0.0)),
                    TokenKind::Minus,
                    TokenKind::Identifier(String::from("foo")),
                    TokenKind::RightParen,
                    TokenKind::Slash,
                    TokenKind::LeftFloor,
                    TokenKind::Identifier(String::from("bar")),
                    TokenKind::RightFloor,
                ],
            ),
            (
                "a_long_variable = 3.14 + π * ϕ / 2",
                vec![
                    TokenKind::Identifier(String::from("a_long_variable")),
                    TokenKind::Equal,
                    TokenKind::Number(Complex64::new(3.14, 0.0)),
                    TokenKind::Plus,
                    TokenKind::Identifier(String::from("π")),
                    TokenKind::Star,
                    TokenKind::Identifier(String::from("ϕ")),
                    TokenKind::Slash,
                    TokenKind::Number(Complex64::new(2.0, 0.0)),
                ],
            ),
            (
                "|foo^2| + √(bar - 1)",
                vec![
                    TokenKind::Pipe,
                    TokenKind::Identifier(String::from("foo")),
                    TokenKind::Caret,
                    TokenKind::Number(Complex64::new(2.0, 0.0)),
                    TokenKind::Pipe,
                    TokenKind::Plus,
                    TokenKind::Sqrt,
                    TokenKind::LeftParen,
                    TokenKind::Identifier(String::from("bar")),
                    TokenKind::Minus,
                    TokenKind::Number(Complex64::new(1.0, 0.0)),
                    TokenKind::RightParen,
                ],
            ),
        ] {
            let tokens = Tokenizer::tokenize(input, 4).unwrap().tokens;
            assert_eq!(extract_token_types(tokens), expected_tokens);
        }
    }

    #[test]
    fn test_no_input() {
        let input = "";
        let tokens = Tokenizer::tokenize(input, 4).unwrap().tokens;
        assert_eq!(tokens, vec![])
    }

    #[test]
    fn test_lexeme_extraction() {
        let input =
            "()⌈⌉⌊⌋+-/*^!|%,=√foo\tbar  baz 3.14 0.0001\t0.045    10.01 1e6 1e-6 2.5e6 2.5e-6";
        let tokens = Tokenizer::tokenize(input, 4).unwrap().tokens;
        assert_eq!(
            extract_lexemes(&tokens),
            vec![
                "(", ")", "⌈", "⌉", "⌊", "⌋", "+", "-", "/", "*", "^", "!", "|", "%", ",", "=",
                "√", "foo", "bar", "baz", "3.14", "0.0001", "0.045", "10.01", "1e6", "1e-6",
                "2.5e6", "2.5e-6"
            ]
        )
    }

    #[test]
    fn test_line_col_calculations() {
        let input = "(  )⌈ ⌉⌊⌋+
-/*^!|%,=√foo\tbar  baz
3.14 0.0001\t0.045    10.01
1e6 1e-6 2.5e6 2.5e-6";
        let tokens = Tokenizer::tokenize(input, 4).unwrap().tokens;
        let positions = extract_token_positions(tokens);
        assert_eq!(
            positions,
            vec![
                (1, 1),
                (1, 4),
                (1, 5),
                (1, 7),
                (1, 8),
                (1, 9),
                (1, 10),
                (1, 11),
                (2, 1),
                (2, 2),
                (2, 3),
                (2, 4),
                (2, 5),
                (2, 6),
                (2, 7),
                (2, 8),
                (2, 9),
                (2, 10),
                (2, 11),
                (2, 18),
                (2, 23),
                (2, 26),
                (3, 1),
                (3, 6),
                (3, 16),
                (3, 25),
                (3, 30),
                (4, 1),
                (4, 5),
                (4, 10),
                (4, 16)
            ]
        )
    }

    #[test]
    fn test_bad_char() {
        for (input, bad_char, col) in [
            (".", '.', 1),
            ("f(x) = #", '#', 8),
            ("23~", '~', 3),
            ("1e2.5", '.', 4),
        ] {
            assert_eq!(
                Tokenizer::tokenize(input, 4).unwrap_err(),
                TokenizerError::BadChar {
                    line: 1,
                    col: col,
                    char: bad_char
                }
            );
        }
    }
}
