use std::collections::HashMap;

use crate::{
    tokenizer::{Token, TokenKind},
    variable::Variable,
    value::Value,
    function::Function,
    num_complex::Complex64
};

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Absolute {
        pipe: Token,
        expr: Box<Expr>,
    },
    Number {
        number: Complex64,
    },
    Identifier {
        name: Token,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum EvaluationError<'a> {
    DivisionByZero {
        operator: Token,
    },
    IncorrectFunctionArgumentCount {
        paren: Token,
        function: Function<'a>,
        received: usize,
    },
    IncorrectFunctionArgumentType {
        function_name: &'a str,
        function_col: usize,
        idx: usize,
        name: &'a str,
        value: Value<'a>,
        expected_type: &'a str,
    },
    UnsupportedBinaryOperator {
        left: Value<'a>,
        operator: Token,
        right: Value<'a>,
    },
    UnsupportedUnaryOperator {
        value: Value<'a>,
        operator: Token,
    },
    UnsupportedAbsoluteOperand {
        pipe: Token,
        value: Value<'a>
    },
    InvalidCallable {
        callee: Value<'a>,
        paren: Token
    }
}

impl<'a> Expr {
    pub fn evaluate(
        self,
        variables: &HashMap<&str, Variable<'a>>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.evaluate(variables)?;
                let right = right.evaluate(variables)?;
                match operator.kind {
                    TokenKind::Plus => match (&left, &right) {
                        (Value::Number(left), Value::Number(right)) => {
                            return Ok(Value::Number(left + right))
                        }
                        _ => {}
                    },
                    TokenKind::Minus => match (&left, &right) {
                        (Value::Number(left), Value::Number(right)) => {
                            return Ok(Value::Number(left - right))
                        }
                        _ => {},
                    },
                    TokenKind::Star => match (&left, &right) {
                        (Value::Number(left), Value::Number(right)) => {
                            return Ok(Value::Number(left * right))
                        }
                        _ => {},
                    },
                    TokenKind::Slash => match (&left, &right) {
                        (Value::Number(left), Value::Number(right)) => {
                            if right.norm() == 0.0 {
                                return Err(EvaluationError::DivisionByZero { operator: operator });
                            }
                            return Ok(Value::Number(left / right))
                        }
                        _ => {},
                    },
                    TokenKind::Caret => match (&left, &right) {
                        (Value::Number(left), Value::Number(right)) => {
                            return Ok(Value::Number(left.powc(*right)))
                        }
                        _ => {},
                    },
                    TokenKind::Percent => match (&left, &right) {
                        (Value::Number(left), Value::Number(right)) => {
                            if right.norm() == 0.0 {
                                return Err(EvaluationError::DivisionByZero { operator: operator });
                            }
                            return Ok(Value::Number(left % right))
                        }
                        _ => {},
                    },
                    kind => panic!("Invalid token kind for binary operation: {:?}", kind),
                }
                // None were matched, unsupported
                Err(EvaluationError::UnsupportedBinaryOperator {
                    left,
                    operator,
                    right,
                })
            }
            Expr::Unary { operator, right } => {
                let right = right.evaluate(variables)?;
                match operator.kind {
                    TokenKind::Minus => match &right {
                        Value::Number(right) => {
                            return Ok(Value::Number(right * Complex64::new(-1.0, 0.0)));
                        }
                        _ => {},
                    },
                    TokenKind::Bang => match right {
                        Value::Number(right) => {
                            let real_part = right.re;
                            if right.im == 0.0 {
                                if real_part.fract() == 0.0 && real_part >= 0.0 {
                                    return Ok(Value::Number(factorial(real_part as u64).into()))
                                }
                            }
                        }
                        _ => {},
                    },
                    kind => panic!("Invalid token kind for unary operation: {:?}", kind),
                }
                // None were matched, unsupported
                Err(EvaluationError::UnsupportedUnaryOperator { value: right, operator: operator })
            }
            Expr::Grouping { expr } => Ok(expr.evaluate(variables)?),
            Expr::Absolute { pipe, expr } => {
                let result = expr.evaluate(variables)?;
                match result {
                    Value::Number(result) => Ok(Value::Number(result.norm().into())),
                    _ => Err(EvaluationError::UnsupportedAbsoluteOperand { pipe: pipe, value: result }),
                }
            }
            Expr::Number { number } => Ok(Value::Number(number)),
            Expr::Identifier { name } => {
                if let Some(variable) = variables.get(name.kind.get_lexeme().as_str()) {
                    Ok(variable.value.clone())
                } else {
                    Ok(Value::Number(Complex64::from(0.0)))
                }
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = callee.evaluate(variables)?;
                if let Value::Function(function) = callee {
                    if function.arity != arguments.len() {
                        return Err(EvaluationError::IncorrectFunctionArgumentCount {
                            paren: paren,
                            function: function,
                            received: arguments.len(),
                        });
                    }
                    let mut evaluated_arguments = Vec::with_capacity(arguments.len());
                    for argument in arguments {
                        evaluated_arguments.push(argument.evaluate(variables)?);
                    }
                    Ok((function.function)(paren.col, evaluated_arguments)?)
                } else {
                    Err(EvaluationError::InvalidCallable { callee: callee, paren: paren })
                }
            }
        }
    }
}

// Computes factorial of a non-negative integer
fn factorial(n: u64) -> f64 {
    (1..=n).map(|x| x as f64).product()
}
