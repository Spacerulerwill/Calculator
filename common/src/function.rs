use std::fmt;

use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Function {
    pub function: fn(Vec<Value>) -> Value,
    pub arity: usize,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Define how to display a Function here
        write!(f, "function <{:?}>", self.function) // Example placeholder
    }
}
