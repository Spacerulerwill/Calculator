use std::fmt;

use num_complex::Complex64;

use crate::{
    expr::{complex_to_string, EvaluationError, Expr},
    value::Value,
};

#[derive(Debug, Clone)]
pub enum Function<'a> {
    NativeFunction(NativeFunction<'a>),
    UserDefinedFunction(UserDefinedFunction),
}

impl<'a> fmt::Display for Function<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::NativeFunction(func) => write!(f, "{}", func),
            Function::UserDefinedFunction(func) => write!(f, "{}", func),
        }
    }
}

impl<'a> Function<'a> {
    pub fn name(&self) -> &str {
        match self {
            Function::NativeFunction(f) => f.name,
            Function::UserDefinedFunction(f) => &f.name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NativeFunction<'a> {
    pub name: &'a str,
    pub function: fn(usize, Vec<Value>) -> Result<Value, EvaluationError>,
    pub arity: usize,
}

impl<'a> fmt::Display for NativeFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} (built-in)", self.name)
    }
}

// Used in
#[derive(Debug, Clone)]
pub enum UserDefinedFunctionArgType {
    Identifier(String),
    Number(Complex64),
}

impl fmt::Display for UserDefinedFunctionArgType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UserDefinedFunctionArgType::Identifier(identifier) => write!(f, "{}", identifier),
            UserDefinedFunctionArgType::Number(number) => {
                write!(f, "{}", complex_to_string(number))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserDefinedFunction {
    pub name: String,
    pub signatures: Vec<(Vec<UserDefinedFunctionArgType>, Expr)>,
}

impl fmt::Display for UserDefinedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.signatures.iter().peekable();

        while let Some((args, expr)) = iter.next() {
            let args_str: Vec<String> = args.iter().map(|arg| arg.to_string()).collect();
            if iter.peek().is_some() {
                writeln!(
                    f,
                    "{}({}) = {}",
                    self.name,
                    args_str.join(", "),
                    expr.to_string()
                )?;
            } else {
                write!(
                    f,
                    "{}({}) = {}",
                    self.name,
                    args_str.join(", "),
                    expr.to_string()
                )?;
            }
        }

        Ok(())
    }
}
