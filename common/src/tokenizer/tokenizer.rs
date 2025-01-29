use std::{
    iter::Peekable,
    str::{Chars, FromStr},
};

use num_complex::Complex64;

use crate::variable::value::unit::{DistanceUnit, MassUnit, StorageUnit, TemperatureUnit, Unit};

use super::{Token, TokenKind, TokenPosition, TokenizerError};

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
            input,
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
            tabsize,
        };
        tokenizer.tokenize_internal()?;
        Ok(tokenizer)
    }

    fn get_keyword_token_kind(lexeme: &str) -> Option<TokenKind> {
        match lexeme {
            "delete" => Some(TokenKind::Delete),
            "cross" => Some(TokenKind::Cross),
            "as" => Some(TokenKind::As),
            "dot" => Some(TokenKind::Dot),
            "clear" => Some(TokenKind::Clear),
            "nanometer" | "nanometers" | "nm" => {
                Some(TokenKind::Unit(Unit::Distance(DistanceUnit::Nanometer)))
            }
            "micrometer" | "micrometers" | "µm" => {
                Some(TokenKind::Unit(Unit::Distance(DistanceUnit::Micrometer)))
            }
            "millimeter" | "millimeters" | "mm" => {
                Some(TokenKind::Unit(Unit::Distance(DistanceUnit::Millimeter)))
            }
            "centimeter" | "centimeters" | "cm" => {
                Some(TokenKind::Unit(Unit::Distance(DistanceUnit::Centimeter)))
            }
            "meters" | "meter" | "m" => Some(TokenKind::Unit(Unit::Distance(DistanceUnit::Meter))),
            "kilometer" | "kilometers" | "km" => {
                Some(TokenKind::Unit(Unit::Distance(DistanceUnit::Kilometer)))
            }
            "inch" | "inches" | "in" => Some(TokenKind::Unit(Unit::Distance(DistanceUnit::Inch))),
            "foot" | "feet" | "ft" => Some(TokenKind::Unit(Unit::Distance(DistanceUnit::Foot))),
            "yard" | "yards" | "yd" => Some(TokenKind::Unit(Unit::Distance(DistanceUnit::Foot))),
            "nanogram" | "nanograms" | "ng" => {
                Some(TokenKind::Unit(Unit::Mass(MassUnit::Nanogram)))
            }
            "microgram" | "micrograms" | "µg" => {
                Some(TokenKind::Unit(Unit::Mass(MassUnit::Microgram)))
            }
            "milligram" | "milligrams" | "mg" => {
                Some(TokenKind::Unit(Unit::Mass(MassUnit::Milligram)))
            }
            "gram" | "grams" | "g" => Some(TokenKind::Unit(Unit::Mass(MassUnit::Gram))),
            "kilogram" | "kilograms" | "kg" => {
                Some(TokenKind::Unit(Unit::Mass(MassUnit::Kilogram)))
            }
            "tonne" | "tonnes" | "t" => Some(TokenKind::Unit(Unit::Mass(MassUnit::Tonne))),
            "ounce" | "ounces" | "oz" => Some(TokenKind::Unit(Unit::Mass(MassUnit::Ounce))),
            "pound" | "pounds" | "lb" | "lbs" => Some(TokenKind::Unit(Unit::Mass(MassUnit::Pound))),
            "stone" | "st" => Some(TokenKind::Unit(Unit::Mass(MassUnit::Stone))),
            "°K" | "K" => Some(TokenKind::Unit(Unit::Temperature(TemperatureUnit::Kelvin))),
            "°C" | "C" => Some(TokenKind::Unit(Unit::Temperature(TemperatureUnit::Celsius))),
            "°F" | "F" => Some(TokenKind::Unit(Unit::Temperature(
                TemperatureUnit::Fahrenheit,
            ))),
            "B" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Byte))),
            "KB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Kilobyte))),
            "MB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Megabyte))),
            "GB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Gigabyte))),
            "TB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Terabyte))),
            "PB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Petabyte))),
            "EB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Exabyte))),
            "KiB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Kibibyte))),
            "MiB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Mebibyte))),
            "GiB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Gibibyte))),
            "TiB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Tebibyte))),
            "PiB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Petibyte))),
            "EiB" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Exbibyte))),
            "b" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Bit))),
            "Kb" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Kilobit))),
            "Mb" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Megabit))),
            "Gb" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Gigabit))),
            "Tb" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Terabit))),
            "Pb" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Petabit))),
            "Eb" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Exabit))),
            "Kib" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Kibibit))),
            "Mib" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Mebibit))),
            "Gib" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Gibibit))),
            "Tib" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Tebibit))),
            "Pib" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Petibit))),
            "Eib" => Some(TokenKind::Unit(Unit::Storage(StorageUnit::Exbibit))),

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
                ';' => self.add_single_char_token(TokenKind::Semicolon),
                '(' => self.add_single_char_token(TokenKind::LeftParen),
                ')' => self.add_single_char_token(TokenKind::RightParen),
                '[' => self.add_single_char_token(TokenKind::LeftBracket),
                ']' => self.add_single_char_token(TokenKind::RightBracket),
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
                '•' => self.add_single_char_token(TokenKind::Dot),
                '×' => self.add_single_char_token(TokenKind::Cross),
                'a'..='z' | 'A'..='Z' | '_' | 'π' | 'ϕ' | '°' | 'µ' => {
                    self.tokenize_identifier()
                }
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
        let mut number_string = self.consume_while(|ch| ch.is_ascii_digit());
        // Consume and exponent and if there is one we stop here
        if self.consume_exponent_for_number(&mut number_string) {
            let num = Complex64::from(f64::from_str(&number_string).unwrap());
            self.add_token(TokenKind::Number(num));
            return;
        }
        // Consume decimal point if exists,
        let iter_save = self.iter.clone();
        let current_col_save = self.current_pos.clone();
        if self.iter.peek() == Some(&'.') {
            self.next();
            let post_dot_digit = self.consume_while(|ch| ch.is_ascii_digit());
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
        let num = Complex64::from(f64::from_str(&number_string).unwrap());
        self.add_token(TokenKind::Number(num));
    }

    fn tokenize_identifier(&mut self) {
        let identifier = self.consume_while(|ch| ch.is_alphanumeric() || ch == '_' || ch == '°');
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
            kind,
            lexeme,
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
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn extract_token_types(tokens: Vec<Token>) -> Vec<TokenKind> {
        tokens.into_iter().map(|token| token.kind).collect()
    }

    fn extract_lexemes(tokens: &[Token]) -> Vec<&str> {
        tokens.iter().map(|token| token.lexeme.as_str()).collect()
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
            ("[", TokenKind::LeftBracket),
            ("]", TokenKind::RightBracket),
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
            ("1e6", TokenKind::Number(Complex64::new(1e6_f64, 0.0))),
            ("1e-6", TokenKind::Number(Complex64::new(1e-6_f64, 0.0))),
            ("2.5e6", TokenKind::Number(Complex64::new(2.5e6_f64, 0.0))),
            ("2.5e-6", TokenKind::Number(Complex64::new(2.5e-6_f64, 0.0))),
            ("delete", TokenKind::Delete),
            ("cross", TokenKind::Cross),
            ("×", TokenKind::Cross),
            ("dot", TokenKind::Dot),
            ("•", TokenKind::Dot),
            ("\n", TokenKind::Newline),
            (";", TokenKind::Semicolon),
            ("clear", TokenKind::Clear),
            ("as", TokenKind::As),
            // Distance Units
            (
                "nanometer",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Nanometer)),
            ),
            (
                "nanometers",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Nanometer)),
            ),
            (
                "nm",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Nanometer)),
            ),
            (
                "micrometer",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Micrometer)),
            ),
            (
                "micrometers",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Micrometer)),
            ),
            (
                "µm",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Micrometer)),
            ),
            (
                "millimeter",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Millimeter)),
            ),
            (
                "millimeters",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Millimeter)),
            ),
            (
                "mm",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Millimeter)),
            ),
            (
                "centimeter",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Centimeter)),
            ),
            (
                "centimeters",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Centimeter)),
            ),
            (
                "cm",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Centimeter)),
            ),
            (
                "meter",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Meter)),
            ),
            (
                "meters",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Meter)),
            ),
            ("m", TokenKind::Unit(Unit::Distance(DistanceUnit::Meter))),
            (
                "kilometer",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Kilometer)),
            ),
            (
                "kilometers",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Kilometer)),
            ),
            (
                "km",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Kilometer)),
            ),
            ("inch", TokenKind::Unit(Unit::Distance(DistanceUnit::Inch))),
            (
                "inches",
                TokenKind::Unit(Unit::Distance(DistanceUnit::Inch)),
            ),
            ("in", TokenKind::Unit(Unit::Distance(DistanceUnit::Inch))),
            ("foot", TokenKind::Unit(Unit::Distance(DistanceUnit::Foot))),
            ("feet", TokenKind::Unit(Unit::Distance(DistanceUnit::Foot))),
            ("ft", TokenKind::Unit(Unit::Distance(DistanceUnit::Foot))),
            ("yard", TokenKind::Unit(Unit::Distance(DistanceUnit::Foot))),
            ("yards", TokenKind::Unit(Unit::Distance(DistanceUnit::Foot))),
            ("yd", TokenKind::Unit(Unit::Distance(DistanceUnit::Foot))),
            // Mass Units
            ("nanogram", TokenKind::Unit(Unit::Mass(MassUnit::Nanogram))),
            ("nanograms", TokenKind::Unit(Unit::Mass(MassUnit::Nanogram))),
            ("ng", TokenKind::Unit(Unit::Mass(MassUnit::Nanogram))),
            (
                "microgram",
                TokenKind::Unit(Unit::Mass(MassUnit::Microgram)),
            ),
            (
                "micrograms",
                TokenKind::Unit(Unit::Mass(MassUnit::Microgram)),
            ),
            ("µg", TokenKind::Unit(Unit::Mass(MassUnit::Microgram))),
            (
                "milligram",
                TokenKind::Unit(Unit::Mass(MassUnit::Milligram)),
            ),
            (
                "milligrams",
                TokenKind::Unit(Unit::Mass(MassUnit::Milligram)),
            ),
            ("mg", TokenKind::Unit(Unit::Mass(MassUnit::Milligram))),
            ("gram", TokenKind::Unit(Unit::Mass(MassUnit::Gram))),
            ("grams", TokenKind::Unit(Unit::Mass(MassUnit::Gram))),
            ("g", TokenKind::Unit(Unit::Mass(MassUnit::Gram))),
            ("kilogram", TokenKind::Unit(Unit::Mass(MassUnit::Kilogram))),
            ("kilograms", TokenKind::Unit(Unit::Mass(MassUnit::Kilogram))),
            ("kg", TokenKind::Unit(Unit::Mass(MassUnit::Kilogram))),
            ("tonne", TokenKind::Unit(Unit::Mass(MassUnit::Tonne))),
            ("tonnes", TokenKind::Unit(Unit::Mass(MassUnit::Tonne))),
            ("t", TokenKind::Unit(Unit::Mass(MassUnit::Tonne))),
            ("ounce", TokenKind::Unit(Unit::Mass(MassUnit::Ounce))),
            ("ounces", TokenKind::Unit(Unit::Mass(MassUnit::Ounce))),
            ("oz", TokenKind::Unit(Unit::Mass(MassUnit::Ounce))),
            ("pound", TokenKind::Unit(Unit::Mass(MassUnit::Pound))),
            ("pounds", TokenKind::Unit(Unit::Mass(MassUnit::Pound))),
            ("lb", TokenKind::Unit(Unit::Mass(MassUnit::Pound))),
            ("lbs", TokenKind::Unit(Unit::Mass(MassUnit::Pound))),
            ("stone", TokenKind::Unit(Unit::Mass(MassUnit::Stone))),
            ("st", TokenKind::Unit(Unit::Mass(MassUnit::Stone))),
            // Temperature Units
            (
                "°K",
                TokenKind::Unit(Unit::Temperature(TemperatureUnit::Kelvin)),
            ),
            (
                "K",
                TokenKind::Unit(Unit::Temperature(TemperatureUnit::Kelvin)),
            ),
            (
                "°C",
                TokenKind::Unit(Unit::Temperature(TemperatureUnit::Celsius)),
            ),
            (
                "C",
                TokenKind::Unit(Unit::Temperature(TemperatureUnit::Celsius)),
            ),
            (
                "°F",
                TokenKind::Unit(Unit::Temperature(TemperatureUnit::Fahrenheit)),
            ),
            (
                "F",
                TokenKind::Unit(Unit::Temperature(TemperatureUnit::Fahrenheit)),
            ),
        ] {
            let tokens = Tokenizer::tokenize(input, 4).unwrap().tokens;
            assert_eq!(extract_token_types(tokens), vec![result]);
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
        let input = "( ) [ ] ⌈ ⌉ ⌊ ⌋ + - / * ^ ! | % , = √ foo 12 12.5 0.5 1e6 1e-6 2.5e6 2.5e-6 ; delete cross × dot • clear as nanometer nanometers nm micrometer micrometers µm millimeter millimeters mm centimeter centimeters cm meter meters m kilometer kilometers km inch inches in foot feet ft yard yards yd nanogram nanograms ng microgram micrograms µg milligram milligrams mg gram grams g kilogram kilograms kg tonne tonnes t ounce ounces oz pound pounds lb lbs stone st °K K °C C °F F";

        let tokens = Tokenizer::tokenize(input, 4).unwrap().tokens;
        assert_eq!(
            extract_lexemes(&tokens),
            vec![
                "(",
                ")",
                "[",
                "]",
                "⌈",
                "⌉",
                "⌊",
                "⌋",
                "+",
                "-",
                "/",
                "*",
                "^",
                "!",
                "|",
                "%",
                ",",
                "=",
                "√",
                "foo",
                "12",
                "12.5",
                "0.5",
                "1e6",
                "1e-6",
                "2.5e6",
                "2.5e-6",
                ";",
                "delete",
                "cross",
                "×",
                "dot",
                "•",
                "clear",
                "as",
                "nanometer",
                "nanometers",
                "nm",
                "micrometer",
                "micrometers",
                "µm",
                "millimeter",
                "millimeters",
                "mm",
                "centimeter",
                "centimeters",
                "cm",
                "meter",
                "meters",
                "m",
                "kilometer",
                "kilometers",
                "km",
                "inch",
                "inches",
                "in",
                "foot",
                "feet",
                "ft",
                "yard",
                "yards",
                "yd",
                "nanogram",
                "nanograms",
                "ng",
                "microgram",
                "micrograms",
                "µg",
                "milligram",
                "milligrams",
                "mg",
                "gram",
                "grams",
                "g",
                "kilogram",
                "kilograms",
                "kg",
                "tonne",
                "tonnes",
                "t",
                "ounce",
                "ounces",
                "oz",
                "pound",
                "pounds",
                "lb",
                "lbs",
                "stone",
                "st",
                "°K",
                "K",
                "°C",
                "C",
                "°F",
                "F"
            ]
        )
    }

    #[test]
    fn test_line_col_calculations() {
        let input = "(  )⌈ ⌉⌊⌋+
-/*^!|%,=√foo\tbar  baz
3.14 0.0001\t0.045    10.01
1e6 1e-6 2.5e6 2.5e-6;";
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
                (4, 16),
                (4, 22)
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
                    col,
                    char: bad_char
                }
            );
        }
    }

    #[test]
    fn test_missing_digits_after_decimal_point() {
        let input = "123.";
        assert_eq!(
            Tokenizer::tokenize(input, 4).unwrap_err(),
            TokenizerError::BadChar {
                line: 1,
                col: 4,
                char: '.'
            }
        );
    }

    #[test]
    fn test_missing_exponent_number() {
        let input = "10e";
        assert_eq!(
            Tokenizer::tokenize(input, 4).unwrap().tokens,
            vec![
                Token {
                    kind: TokenKind::Number(Complex64::from(10.0)),
                    lexeme: String::from("10"),
                    line: 1,
                    col: 1
                },
                Token {
                    kind: TokenKind::Identifier(String::from("e")),
                    lexeme: String::from("e"),
                    line: 1,
                    col: 3
                }
            ]
        );
    }
}
