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
        write!(f, "function <{:?}>", self.function)  // Example placeholder
    }
}


fn internal_builtin_sin(args: Vec<Value>) -> Value {
    if let Value::Number(num) = args.get(0).unwrap() {
        Value::Number(num.sin())
    } else {
        todo!()
    }
}

pub const BUILTIN_SIN: Value = Value::Function(Function {
    function: internal_builtin_sin,
    arity: 1,
});
