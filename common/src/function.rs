use std::fmt;

use num_complex::Complex64;

use crate::{
    expr::{complex_to_string, EvaluationError, Expr},
    tokenizer::Token,
    value::Value,
    variable::{Variable, VariableMap},
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

    pub fn call(
        &self,
        col: usize,
        arguments: Vec<Value<'a>>,
        variables: &mut VariableMap<'a>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match self {
            Function::NativeFunction(func) => {
                if func.arity != arguments.len() {
                    return Err(EvaluationError::IncorrectFunctionArgumentCount {
                        col: col,
                        name: func.name,
                        received: arguments.len(),
                        required: func.arity,
                    });
                }
                Ok((func.function)(col, arguments)?)
            }
            Function::UserDefinedFunction(func) => {
                for (signature, expr) in func.signatures.iter() {
                    if signature.matches_parameters(&arguments) {
                        let mut inputs = variables.clone();
                        for (arg_type, val) in
                            signature.parameters.iter().zip(arguments.into_iter())
                        {
                            if let UserDefinedFunctionArgType::Identifier(identifier) = arg_type {
                                inputs.insert(identifier.clone(), Variable::as_variable(val));
                            }
                        }
                        return expr.clone().evaluate(&mut inputs);
                    }
                }
                Err(EvaluationError::NoMatchingSignature {
                    col: col,
                    name: func.name.clone(),
                })
            }
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

#[derive(Debug, Clone)]
pub enum UserDefinedFunctionArgType {
    Identifier(String),
    Number(Complex64),
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub parameters: Vec<UserDefinedFunctionArgType>,
}

impl Signature {
    pub fn from_call_expression(callee: Expr, paramaters: Vec<Expr>) -> Result<(Token, Self), ()> {
        if let Expr::Identifier { name } = callee {
            let mut signature = Signature {
                parameters: Vec::with_capacity(paramaters.len()),
            };
            for param in paramaters {
                match param {
                    Expr::Identifier { name } => signature
                        .parameters
                        .push(UserDefinedFunctionArgType::Identifier(name.lexeme)),
                    Expr::Number { number } => signature
                        .parameters
                        .push(UserDefinedFunctionArgType::Number(number)),
                    _ => return Err(()),
                }
            }
            return Ok((name, signature));
        }
        Err(())
    }
    pub fn matches_parameters(&self, parameters: &Vec<Value>) -> bool {
        if self.parameters.len() != parameters.len() {
            return false;
        }

        for (param, arg) in self.parameters.iter().zip(parameters.iter()) {
            match param {
                UserDefinedFunctionArgType::Identifier(_) => continue,
                UserDefinedFunctionArgType::Number(expected) => {
                    match arg {
                        Value::Function(_) => return false,
                        Value::Number(num) => {
                            if num != expected {
                                return false;
                            }
                        }
                    };
                }
            }
        }

        true
    }
}

impl PartialEq for Signature {
    fn eq(&self, other: &Self) -> bool {
        if self.parameters.len() != other.parameters.len() {
            return false;
        }
        for (arg_type_1, arg_type_2) in self.parameters.iter().zip(other.parameters.iter()) {
            if std::mem::discriminant(arg_type_1) != std::mem::discriminant(arg_type_2) {
                return false;
            }
            match (arg_type_1, arg_type_2) {
                (
                    UserDefinedFunctionArgType::Number(one),
                    UserDefinedFunctionArgType::Number(two),
                ) => {
                    if one != two {
                        return false;
                    }
                }
                _ => {}
            }
        }
        true
    }
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
    pub signatures: Vec<(Signature, Expr)>,
}

impl fmt::Display for UserDefinedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.signatures.iter().peekable();

        while let Some((signature, expr)) = iter.next() {
            let args_str: Vec<String> = signature
                .parameters
                .iter()
                .map(|arg| arg.to_string())
                .collect();
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
