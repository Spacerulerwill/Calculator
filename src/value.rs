use std::fmt;

use num_complex::Complex64;

use crate::function::Function;

#[derive(Debug, Clone)]
pub enum Value {
    Function(Function),
    Number(Complex64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Function(func) => write!(f, "{}", func),
            Value::Number(num) => write!(f, "{}", num),
        }
    }
}
