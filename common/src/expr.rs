use std::fmt;

use crate::{
    function::{Function, UserDefinedFunctionArgType},
    num_complex::Complex64,
    tokenizer::{Token, TokenKind},
    value::{Value, ValueConstraint},
    variable::{Variable, VariableMap},
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
    DivisionByZero { operator: Token },
    /// Only applicable to native functions
    IncorrectFunctionArgumentCount {
        paren: Token,
        name: &'a str,
        received: usize,
        required: usize,
    },
    /// Only applicable to native functions
    IncorrectFunctionArgumentType {
        function_name: String,
        function_col: usize,
        idx: usize,
        name: String,
        constraint: ValueConstraint,
    },
    /// If the user tries to add a new signature to a native function
    CantAddSignatureToNativeFunction {
        name: Token,
    },
    /// If the user tries to delete a signature from a native function
    CantDeleteSignatureFromNativeFunction {
        name: Token,
    },
    /// No matching signature found for a function
    NoMatchingSignature { col: usize, name: String },
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
        paren: Token,
        kind: GroupingKind,
        constraint: ValueConstraint,
    },
    /// Try to perform function call on a non function value
    InvalidCallable { col: usize },
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
            EvaluationError::DivisionByZero { operator } => write!(
                f,
                "Column {} :: Divison by zero on right side of '{}'",
                operator.col,
                &operator.lexeme
            ),
            EvaluationError::IncorrectFunctionArgumentCount {
                paren,
                name,
                received,
                required,
            } => write!(
                f,
                "Column {} :: Function '{}' requires {} argument(s) but received {}",
                paren.col, name, required, received,
            ),
            EvaluationError::NoMatchingSignature { col, name } => write!(
                f,
                "Column {} :: No matching parameter signature for function '{}'. Type '{}' to see list of signatures",
                col,
                name,
                name
            ),
            EvaluationError::UnsupportedBinaryOperator {
                operator,
                constraint
            } => write!(
                f,
                "Column {} :: Cannot apply binary operator '{}' as one or more operands does not meet value constraint '{}'",
                operator.col,
                &operator.lexeme,
                constraint
            ),
            EvaluationError::UnsupportedUnaryOperator { operator, constraint} => write!(
                f,
                "Column {} :: Cannot apply unary operator '{}' as operand does not meet value constraint '{}'",
                operator.col,
                &operator.lexeme,
                constraint
            ),
            EvaluationError::InvalidCallable { col } => write!(
                f,
                "Column {} :: Callee does not meet value constraint '{}'",
                *col,
                ValueConstraint::Function
            ),
            EvaluationError::GroupingValueConstraintNotMet { paren, kind, constraint } => {
                let grouping_str = match kind {
                    GroupingKind::Grouping => "grouping",
                    GroupingKind::Absolute => "absolute grouping",
                    GroupingKind::Ceil => "ceil grouping",
                    GroupingKind::Floor => "floor grouping"
                };
                write!(
                    f,
                    "Column {} :: Value in {grouping_str} does meet value constraint '{}'",
                    paren.col,
                    constraint
                )
            },
            EvaluationError::IncorrectFunctionArgumentType {
                function_name,
                function_col,
                idx,
                name,
                constraint
            } => write!(
                f,
                "Column {} :: Argument {} ({}) for function '{}' does not meet value constraint '{}'",
                function_col,
                idx,
                name,
                function_name,
                constraint,
            ),
            EvaluationError::ConstantAssignment { name } => write!(
                f,
                "Column {} :: Cannot assign to '{}' as it is constant",
                name.col,
                &name.lexeme
            ),
            EvaluationError::ConstantDeletion { name } => write!(
                f,
                "Column {} :: Cannot delete '{}' as it is constant",
                name.col,
                &name.lexeme
            ),
            EvaluationError::UnknownVariable { name } => write!(
                f,
                "Column {} :: Unknown variable '{}'",
                name.col,
                &name.lexeme
            ),
            EvaluationError::CantAddSignatureToNativeFunction { name } => write!(
                f,
                "Column {} :: Can't add signature to native function '{}'",
                name.col,
                &name.lexeme
            ),
            EvaluationError::CantDeleteSignatureFromNativeFunction { name } => write!(
                f,
                "Column {} :: Can't delete signature from native function '{}'",
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
                            operator: operator.clone(),
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
                            operator: operator.clone(),
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
                        paren: paren.clone(),
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
                        paren: paren.clone(),
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
                        paren: paren.clone(),
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
                match &*function {
                    Function::NativeFunction(func) => {
                        if func.arity != evaluated_arguments.len() {
                            return Err(EvaluationError::IncorrectFunctionArgumentCount {
                                paren: paren.clone(),
                                name: func.name,
                                received: evaluated_arguments.len(),
                                required: func.arity,
                            });
                        }
                        Ok((func.function)(paren.col, evaluated_arguments)?)
                    }
                    Function::UserDefinedFunction(func) => {
                        for (signature, expr) in func.signatures.iter() {
                            if signature.matches_parameters(&evaluated_arguments) {
                                let mut inputs = variables.clone();
                                for (arg_type, val) in signature
                                    .parameters
                                    .iter()
                                    .zip(evaluated_arguments.into_iter())
                                {
                                    if let UserDefinedFunctionArgType::Identifier(identifier) =
                                        arg_type
                                    {
                                        inputs
                                            .insert(identifier.clone(), Variable::as_variable(val));
                                    }
                                }
                                return expr.clone().evaluate(&mut inputs);
                            }
                        }
                        Err(EvaluationError::NoMatchingSignature {
                            col: paren.col,
                            name: func.name.clone(),
                        })
                    }
                }
            }
            _ => {
                return Err(EvaluationError::InvalidCallable {
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
    (1..=n).map(|x| x as f64).product()
}
