use std::fmt;

use num_complex::Complex64;

use crate::function::Function;

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Function(Function<'a>),
    Number(Complex64),
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Function(func) => write!(f, "{}", func),
            Value::Number(num) => write!(f, "{}", num),
        }
    }
}

impl Value<'_> {
    pub fn get_type_string(&self) -> &str {
        match self {
            Value::Function(_) => "function",
            Value::Number(num) => match num.im {
                0.0 => match num.re.fract() {
                    0.0 => "integer",
                    _ => "real"
                },
                _ => "complex"
            },
        }
    }
}
