use std::{collections::HashMap, fmt};

use crate::{
    function::Function, num_complex::Complex64, tokenizer::{Token, TokenKind}, value::{Value, ValueConstraint}, variable::Variable
};

#[derive(Debug, Clone)]
pub enum GroupingKind {
    Grouping,
    Absolute,
    Ceil,
    Floor
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assign {
        name: Token,
        new_value: Box<Expr>,
    },
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
        constraint: ValueConstraint,
    },
    UnsupportedBinaryOperator {
        operator: Token,
        constraint: ValueConstraint
    },
    UnsupportedUnaryOperator {
        operator: Token,
        constraint: ValueConstraint
    },
    GroupingValueConstraintNotMet {
        paren: Token,
        kind: GroupingKind,
        constraint: ValueConstraint,
    },
    InvalidCallable {
        paren: Token
    },
    ConstantAssignment {
        name: Token,
    }
}

impl<'a> Expr {
    pub fn evaluate(
        self,
        variables: &mut HashMap<String, Variable<'a>>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match self {
            Expr::Assign { name, new_value } => Self::evaluate_assign(name, new_value, variables),
            Expr::Binary {
                left,
                operator,
                right,
            } => Self::evaluate_binary(left, operator, right, variables),
            Expr::Unary { operator, operand } => Self::evaluate_unary(operand, operator, variables),
            Expr::Grouping { paren, kind, expr } => Self::evaluate_grouping(paren, kind, expr, variables),
            Expr::Number { number } => Ok(Value::Number(number)),
            Expr::Identifier { name } => Self::evaluate_identifier(name, variables),
            Expr::Call { callee, paren, arguments} => Self::evaluate_call(callee, paren, arguments, variables),
        }
    }

    fn evaluate_assign(
        name: Token,
        new_value: Box<Expr>,
        variables: &mut HashMap<String, Variable<'a>>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let new_value = new_value.evaluate(variables)?;
        if let Some(variable) = variables.get(&name.kind.get_lexeme()) {
            if variable.constant {
                return Err(EvaluationError::ConstantAssignment { name: name })
            }
        }
        variables.insert(name.kind.get_lexeme(), Variable::as_variable(new_value.clone()));
        Ok(new_value)
    }
    

    fn evaluate_binary(
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
        variables: &mut HashMap<String, Variable<'a>>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
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
            operator,
            constraint: ValueConstraint::Number
        })
    }

    fn evaluate_unary(
        operand: Box<Expr>,
        operator: Token,
        variables: &mut HashMap<String, Variable<'a>>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let operand = operand.evaluate(variables)?;
        match operator.kind {
            TokenKind::Minus => match &operand {
                Value::Number(right) => {
                    return Ok(Value::Number(right * Complex64::new(-1.0, 0.0)));
                }
                _ => return Err(EvaluationError::UnsupportedUnaryOperator { operator: operator, constraint: ValueConstraint::Number  })
            },
            TokenKind::Bang => match &operand {
                Value::Number(right) if operand.fits_value_constraint(ValueConstraint::Natural) => {
                    return Ok(Value::Number(factorial(right.re as u64).into()))
                }
                _ => return Err(EvaluationError::UnsupportedUnaryOperator { operator: operator, constraint: ValueConstraint::Natural  })
            },
            kind => panic!("Invalid token kind for unary operation: {:?}", kind),
        }
    }

    fn evaluate_grouping(
        paren: Token,
        kind: GroupingKind,
        expr: Box<Expr>,
        variables: &mut HashMap<String, Variable<'a>>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match kind {
            GroupingKind::Grouping => expr.evaluate(variables),
            GroupingKind::Absolute => {
                let result = expr.evaluate(variables)?;
                match result {
                    Value::Number(result) => Ok(Value::Number(result.norm().into())),
                    _ => Err(EvaluationError::GroupingValueConstraintNotMet { paren: paren, kind: kind, constraint: ValueConstraint::Number }),
                }
            },
            GroupingKind::Ceil => {
                let result = expr.evaluate(variables)?;
                match result {
                    Value::Number(num) if result.fits_value_constraint(ValueConstraint::Real) => {
                        return Ok(Value::Number(num.re.ceil().into()));
                    }
                    _ => Err(EvaluationError::GroupingValueConstraintNotMet { paren: paren, kind: kind, constraint: ValueConstraint::Real }),
                }
            },
            GroupingKind::Floor => {
                let result = expr.evaluate(variables)?;
                match result {
                    Value::Number(num) if result.fits_value_constraint(ValueConstraint::Real) => {
                        return Ok(Value::Number(num.re.floor().into()));
                    }
                    _ => Err(EvaluationError::GroupingValueConstraintNotMet { paren: paren, kind: kind, constraint: ValueConstraint::Real }),
                }
            },
        }
    }

    fn evaluate_identifier(
        name: Token,
        variables: &mut HashMap<String, Variable<'a>>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match variables.get(name.kind.get_lexeme().as_str()) {
            Some(variable) => Ok(variable.value.clone()),
            None => Ok(Value::Number(Complex64::from(0.0)))
        }
    }

    fn evaluate_call(
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
        variables: &mut HashMap<String, Variable<'a>>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let callee = callee.evaluate(variables)?;
        match callee {
            Value::Function(function) => {
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
                return Ok((function.function)(paren.col, evaluated_arguments)?)
            } 
            _ => return Err(EvaluationError::InvalidCallable { paren: paren })

        }
    } 
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Assign { name, new_value } => write!(f, "{}={}", name.kind.get_lexeme(), new_value),
            Expr::Binary { left, operator, right } => write!(f, "{left}{}{right}", &operator.kind.get_lexeme()),
            Expr::Unary { operator, operand } => match &operator.kind {
                TokenKind::Bang => write!(f, "{operand}{}", &operator.kind.get_lexeme()),
                TokenKind::Minus => write!(f, "{}{operand}", &operator.kind.get_lexeme()),
                kind => panic!("Invalid operator for unary expression {:?}", kind)
            }
            Expr::Grouping { paren: _, kind, expr } => match kind {
                GroupingKind::Grouping => write!(f, "({expr})"),
                GroupingKind::Absolute => write!(f, "|{expr}|"),
                GroupingKind::Ceil => write!(f, "⌈{expr}⌉"),
                GroupingKind::Floor => write!(f, "⌊{expr}⌋"),
            },
            Expr::Number { number } => write!(f, "{}", complex_to_string(number)),
            Expr::Identifier { name } => write!(f, "{}", name.kind.get_lexeme()),
            Expr::Call { callee, paren: _, arguments } => {
                let args: Vec<String> = arguments.iter().map(|arg| format!("{}", arg)).collect();
                let args_str = args.join(", ");
                write!(f, "{}({})", callee, args_str)
            },
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
        return String::from("0")
    } else {
        return format!("{}i", num.im);
    }
}

// Computes factorial of a non-negative integer
fn factorial(n: u64) -> f64 {
    (1..=n).map(|x| x as f64).product()
}
