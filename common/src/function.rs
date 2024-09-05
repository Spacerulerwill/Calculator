use std::fmt;

use crate::{expr::EvaluationError, value::Value};

#[derive(Debug, Clone)]
pub struct Function<'a>  {
    pub name: &'a str,
    pub function: fn(usize, Vec<Value>) -> Result<Value, EvaluationError>,
    pub arity: usize,
}

impl<'a> fmt::Display for Function<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Define how to display a Function here
        write!(f, "function <{:?}>", self.function) // Example placeholder
    }
}
