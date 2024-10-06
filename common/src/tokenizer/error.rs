use std::fmt;

#[derive(Debug, PartialEq)]
pub enum TokenizerError {
    BadChar { line: usize, col: usize, char: char },
}

impl std::error::Error for TokenizerError {}

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
