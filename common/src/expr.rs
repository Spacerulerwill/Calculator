use std::fmt;

use crate::{
    num_complex::Complex64,
    tokenizer::{Token, TokenKind},
    value::{Value, ValueConstraint},
    variable::VariableMap,
};

#[derive(Debug, Copy, Clone)]
pub enum GroupingKind {
    Grouping,
    Absolute,
    Ceil,
    Floor,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        operand: Box<Expr>,
    },
    Grouping {
        paren: Token,
        kind: GroupingKind,
        expr: Box<Expr>,
    },
    Number {
        number: Complex64,
    },
    Vector(Vec<Expr>),
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
    /// Division by zero is naughty (noughty)
    DivisionByZero { line: usize, col: usize },
    /// Only applicable to native functions
    IncorrectFunctionArgumentCount {
        line: usize,
        col: usize,
        name: &'a str,
        received: usize,
        required: usize,
    },
    /// Only applicable to native functions
    IncorrectFunctionArgumentType {
        function_name: String,
        line: usize,
        col: usize,
        idx: usize,
        name: String,
        constraint: ValueConstraint,
    },
    /// If the user tries to add a new signature to a native function
    CantAddSignatureToNativeFunction { name: Token },
    /// If the user tries to delete a signature from a native function
    CantDeleteSignatureFromNativeFunction { name: Token },
    /// No matching signature found for a function
    NoMatchingSignature {
        line: usize,
        col: usize,
        name: String,
    },
    /// Operands of binary operator don't meet value constraint
    UnsupportedBinaryOperator {
        operator: Token,
        constraint: ValueConstraint,
    },
    /// Operand of unary operator doesn't meet value constraint
    UnsupportedUnaryOperator {
        operator: Token,
        constraint: ValueConstraint,
    },
    /// Grouping operand does not meet value constraint
    GroupingValueConstraintNotMet {
        line: usize,
        col: usize,
        kind: GroupingKind,
        constraint: ValueConstraint,
    },
    /// Try to perform function call on a non function value
    InvalidCallable { line: usize, col: usize },
    /// Not allowed to assign value to constants
    ConstantAssignment { name: Token },
    /// Not allowed to delete constants
    ConstantDeletion { name: Token },
    /// Tried to access or use a non-existant variable
    UnknownVariable { name: Token },
}

impl<'a> fmt::Display for EvaluationError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvaluationError::DivisionByZero { line, col } => write!(
                f,
                "Line {}, Column {} :: Divison by zero",
                line,
                col
            ),
            EvaluationError::IncorrectFunctionArgumentCount {
                line,
                col,
                name,
                received,
                required,
            } => write!(
                f,
                "Line {}, Column {} :: Function '{}' requires {} argument(s) but received {}",
                line, col, name, required, received,
            ),
            EvaluationError::NoMatchingSignature { line, col, name } => write!(
                f,
                "Line {}, Column {} :: No matching parameter signature for function '{}'. Type '{}' to see list of signatures",
                line,
                col,
                name,
                name
            ),
            EvaluationError::UnsupportedBinaryOperator {
                operator,
                constraint
            } => write!(
                f,
                "Line {}, Column {} :: Cannot apply binary operator '{}' as one or more operands does not meet value constraint '{}'",
                operator.line,
                operator.col,
                &operator.lexeme,
                constraint
            ),
            EvaluationError::UnsupportedUnaryOperator { operator, constraint} => write!(
                f,
                "Line {}, Column {} :: Cannot apply unary operator '{}' as operand does not meet value constraint '{}'",
                operator.line,
                operator.col,
                &operator.lexeme,
                constraint
            ),
            EvaluationError::InvalidCallable { line, col } => write!(
                f,
                "Line {}, Column {} :: Callee does not meet value constraint '{}'",
                line,
                col,
                ValueConstraint::Function
            ),
            EvaluationError::GroupingValueConstraintNotMet { line, col, kind, constraint } => {
                let grouping_str = match kind {
                    GroupingKind::Grouping => "grouping",
                    GroupingKind::Absolute => "absolute grouping",
                    GroupingKind::Ceil => "ceil grouping",
                    GroupingKind::Floor => "floor grouping"
                };
                write!(
                    f,
                    "Line {}, Column {} :: Value in {grouping_str} does meet value constraint '{}'",
                    line,
                    col,
                    constraint
                )
            },
            EvaluationError::IncorrectFunctionArgumentType {
                function_name,
                line,
                col,
                idx,
                name,
                constraint
            } => write!(
                f,
                "Line {}, Column {} :: Argument {} ({}) for function '{}' does not meet value constraint '{}'",
                line,
                col,
                idx,
                name,
                function_name,
                constraint,
            ),
            EvaluationError::ConstantAssignment { name } => write!(
                f,
                "Line {}, Column {} :: Cannot assign to '{}' as it is constant",
                name.line,
                name.col,
                &name.lexeme
            ),
            EvaluationError::ConstantDeletion { name } => write!(
                f,
                "Line {}, Column {} :: Cannot delete '{}' as it is constant",
                name.line,
                name.col,
                &name.lexeme
            ),
            EvaluationError::UnknownVariable { name } => write!(
                f,
                "Line {}, Column {} :: Unknown variable '{}'",
                name.line,
                name.col,
                &name.lexeme
            ),
            EvaluationError::CantAddSignatureToNativeFunction { name } => write!(
                f,
                "Line {}, Column {} :: Can't add signature to native function '{}'",
                name.line,
                name.col,
                &name.lexeme
            ),
            EvaluationError::CantDeleteSignatureFromNativeFunction { name } => write!(
                f,
                "Line {}, Column {} :: Can't delete signature from native function '{}'",
                name.line,
                name.col,
                &name.lexeme
            )
        }
    }
}

impl<'a> Expr {
    pub fn evaluate(
        &self,
        variables: &mut VariableMap<'a>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => Self::evaluate_binary(&*left, operator, &*right, variables),
            Expr::Unary { operator, operand } => {
                Self::evaluate_unary(&*operand, operator, variables)
            }
            Expr::Grouping { paren, kind, expr } => {
                Self::evaluate_grouping(paren, *kind, &*expr, variables)
            }
            Expr::Number { number } => Ok(Value::Number(*number)),
            Expr::Vector(args) => {
                let mut values = Vec::with_capacity(args.len());
                for arg in args {
                    let value = arg.evaluate(variables)?;
                    match value {
                        Value::Number(num) => {
                            values.push(num);
                        }
                        _ => panic!("Non number in vector"),
                    }
                }
                Ok(Value::Vector(values))
            }
            Expr::Identifier { name } => Self::evaluate_identifier(name, variables),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => Self::evaluate_call(&*callee, paren, arguments, variables),
        }
    }

    fn evaluate_binary(
        left: &Expr,
        operator: &Token,
        right: &Expr,
        variables: &mut VariableMap<'a>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let left = left.evaluate(variables)?;
        let right = right.evaluate(variables)?;
        match &operator.kind {
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
                _ => {}
            },
            TokenKind::Star => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    return Ok(Value::Number(left * right))
                }
                _ => {}
            },
            TokenKind::Slash => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    if right.norm() == 0.0 {
                        return Err(EvaluationError::DivisionByZero {
                            line: operator.line,
                            col: operator.col,
                        });
                    }
                    return Ok(Value::Number(left / right));
                }
                _ => {}
            },
            TokenKind::Caret => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    return Ok(Value::Number(left.powc(*right)))
                }
                _ => {}
            },
            TokenKind::Percent => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    if right.norm() == 0.0 {
                        return Err(EvaluationError::DivisionByZero {
                            line: operator.line,
                            col: operator.col,
                        });
                    }
                    return Ok(Value::Number(left % right));
                }
                _ => {}
            },
            kind => panic!("Invalid token kind for binary operation: {:?}", kind),
        }
        // None were matched, unsupported
        Err(EvaluationError::UnsupportedBinaryOperator {
            operator: operator.clone(),
            constraint: ValueConstraint::Number,
        })
    }

    fn evaluate_unary(
        operand: &Expr,
        operator: &Token,
        variables: &mut VariableMap<'a>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let operand = operand.evaluate(variables)?;
        match &operator.kind {
            TokenKind::Minus => match &operand {
                Value::Number(right) => {
                    return Ok(Value::Number(right * Complex64::new(-1.0, 0.0)));
                }
                _ => {
                    return Err(EvaluationError::UnsupportedUnaryOperator {
                        operator: operator.clone(),
                        constraint: ValueConstraint::Number,
                    })
                }
            },
            TokenKind::Sqrt => match &operand {
                Value::Number(right) => {
                    return Ok(Value::Number(right.sqrt()));
                }
                _ => {
                    return Err(EvaluationError::UnsupportedUnaryOperator {
                        operator: operator.clone(),
                        constraint: ValueConstraint::Number,
                    })
                }
            },
            TokenKind::Bang => match &operand {
                Value::Number(right) if operand.fits_value_constraint(ValueConstraint::Natural) => {
                    return Ok(Value::Number(factorial(right.re as u64).into()))
                }
                _ => {
                    return Err(EvaluationError::UnsupportedUnaryOperator {
                        operator: operator.clone(),
                        constraint: ValueConstraint::Natural,
                    })
                }
            },
            kind => panic!("Invalid token kind for unary operation: {:?}", kind),
        }
    }

    fn evaluate_grouping(
        paren: &Token,
        kind: GroupingKind,
        expr: &Expr,
        variables: &mut VariableMap<'a>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match kind {
            GroupingKind::Grouping => expr.evaluate(variables),
            GroupingKind::Absolute => {
                let result = expr.evaluate(variables)?;
                match result {
                    Value::Number(result) => Ok(Value::Number(result.norm().into())),
                    _ => Err(EvaluationError::GroupingValueConstraintNotMet {
                        line: paren.line,
                        col: paren.col,
                        kind: kind,
                        constraint: ValueConstraint::Number,
                    }),
                }
            }
            GroupingKind::Ceil => {
                let result = expr.evaluate(variables)?;
                match result {
                    Value::Number(num) if result.fits_value_constraint(ValueConstraint::Real) => {
                        return Ok(Value::Number(num.re.ceil().into()));
                    }
                    _ => Err(EvaluationError::GroupingValueConstraintNotMet {
                        line: paren.line,
                        col: paren.col,
                        kind: kind,
                        constraint: ValueConstraint::Real,
                    }),
                }
            }
            GroupingKind::Floor => {
                let result = expr.evaluate(variables)?;
                match result {
                    Value::Number(num) if result.fits_value_constraint(ValueConstraint::Real) => {
                        return Ok(Value::Number(num.re.floor().into()));
                    }
                    _ => Err(EvaluationError::GroupingValueConstraintNotMet {
                        line: paren.line,
                        col: paren.col,
                        kind: kind,
                        constraint: ValueConstraint::Real,
                    }),
                }
            }
        }
    }

    fn evaluate_identifier(
        name: &Token,
        variables: &mut VariableMap<'a>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match variables.get(name.lexeme.as_str()) {
            Some(variable) => Ok(variable.value.clone()),
            _ => Err(EvaluationError::UnknownVariable { name: name.clone() }),
        }
    }

    fn evaluate_call(
        callee: &Expr,
        paren: &Token,
        arguments: &Vec<Expr>,
        variables: &mut VariableMap<'a>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let callee = callee.evaluate(variables)?;
        match callee {
            Value::Function(function) => {
                let mut evaluated_arguments = Vec::with_capacity(arguments.len());
                for argument in arguments {
                    evaluated_arguments.push(argument.evaluate(variables)?);
                }
                let function = function.borrow();
                function.call(paren.line, paren.col, evaluated_arguments, variables)
            }
            _ => {
                return Err(EvaluationError::InvalidCallable {
                    line: paren.line,
                    col: paren.col,
                })
            }
        }
    }

    pub fn get_type_string(&self) -> &'static str {
        match self {
            Expr::Binary {
                left: _,
                operator: _,
                right: _,
            } => "binary expression",
            Expr::Unary {
                operator: _,
                operand: _,
            } => "unary expression",
            Expr::Grouping {
                paren: _,
                kind: _,
                expr: _,
            } => "grouping",
            Expr::Number { number: _ } => "number",
            Expr::Vector(_) => "vector",
            Expr::Identifier { name: _ } => "identifier",
            Expr::Call {
                callee: _,
                paren: _,
                arguments: _,
            } => "function call expression",
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => write!(f, "{left}{}{right}", &operator.lexeme),
            Expr::Unary { operator, operand } => match &operator.kind {
                TokenKind::Bang => write!(f, "{operand}{}", &operator.lexeme),
                _ => write!(f, "{}{operand}", &operator.lexeme),
            },
            Expr::Grouping {
                paren: _,
                kind,
                expr,
            } => match kind {
                GroupingKind::Grouping => write!(f, "({expr})"),
                GroupingKind::Absolute => write!(f, "|{expr}|"),
                GroupingKind::Ceil => write!(f, "⌈{expr}⌉"),
                GroupingKind::Floor => write!(f, "⌊{expr}⌋"),
            },
            Expr::Number { number } => write!(f, "{}", complex_to_string(number)),
            Expr::Vector(expressions) => write!(
                f,
                "[{}]",
                expressions
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expr::Identifier { name } => write!(f, "{}", &name.lexeme),
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                let args: Vec<String> = arguments.iter().map(|arg| format!("{}", arg)).collect();
                let args_str = args.join(", ");
                write!(f, "{}({})", callee, args_str)
            }
        }
    }
}

pub fn complex_to_string(num: &Complex64) -> String {
    let has_real = num.re != 0.0;
    let has_imaginary = num.im != 0.0;

    if has_real && has_imaginary {
        if num.im == 1.0 {
            return format!("{} + i", num.re);
        } else if num.im == -1.0 {
            return format!("{} - i", num.re);
        } else if num.im < 0.0 {
            return format!("{} - {}i", num.re, num.im.abs());
        } else {
            return format!("{} + {}i", num.re, num.im);
        }
    } else if has_real {
        return format!("{}", num.re);
    } else if num.im == 1.0 {
        return "i".to_string();
    } else if num.im == -1.0 {
        return "-i".to_string();
    } else if num.im == 0.0 {
        return String::from("0");
    } else {
        return format!("{}i", num.im);
    }
}

// Computes factorial of a non-negative integer
fn factorial(n: u64) -> f64 {
    (2..=n).map(|x| x as f64).product()
}
