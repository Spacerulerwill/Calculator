pub mod error;

use std::fmt;

use num_complex::Complex64;

use crate::{
    tokenizer::token::{Token, TokenKind},
    variable::{
        value::{
            complex_to_string,
            constraint::ValueConstraint,
            matrix::{matrix_format, Matrix},
            measurement::Measurement,
            unit::Unit,
            Value,
        },
        VariableMap,
    },
};

use error::{
    DivisionByZero, EvaluationError, GroupingValueConstraintNotMet, InvalidCallable,
    InvalidGroupingOperand, InvalidMatrixParameter, InvalidMeasurementConversion,
    UnaryOperatorValueConstraintNotMet, UnknownVariable, UnsupportedBinaryOperator,
    UnsupportedUnaryOperator,
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum GroupingKind {
    Grouping,
    Absolute,
    Ceil,
    Floor,
}

impl GroupingKind {
    pub fn get_type_string(&self) -> &'static str {
        match self {
            GroupingKind::Grouping => "grouping",
            GroupingKind::Absolute => "absolute grouping",
            GroupingKind::Ceil => "ceil grouping",
            GroupingKind::Floor => "floor grouping",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    As {
        expr: Box<Expr>,
        _as: Token,
        unit: Unit,
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
    Measurement {
        measurement: Measurement,
    },
    Matrix {
        bracket: Token,
        parameters: Vec<Vec<Expr>>,
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

impl<'a> Expr {
    pub fn evaluate(
        &self,
        variables: &mut VariableMap<'a>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match self {
            Expr::As { expr, _as, unit } => {
                let value = expr.evaluate(variables)?;
                match &value {
                    Value::Measurement(measurement) => {
                        if let Ok(measurement) = measurement.to_other_unit(*unit) {
                            return Ok(Value::Measurement(measurement));
                        }
                    }
                    Value::Number(num) => {
                        return Ok(Value::Measurement(Measurement::new(*num, *unit)))
                    }
                    _ => {}
                }
                Err(EvaluationError::InvalidMeasurementConversion(Box::new(
                    InvalidMeasurementConversion {
                        line: _as.line,
                        col: _as.col,
                        value,
                        unit: *unit,
                    },
                )))
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => Self::evaluate_binary(left, operator, right, variables),
            Expr::Unary { operator, operand } => Self::evaluate_unary(operand, operator, variables),
            Expr::Grouping { paren, kind, expr } => {
                Self::evaluate_grouping(paren, *kind, expr, variables)
            }
            Expr::Number { number } => Ok(Value::Number(*number)),
            Expr::Measurement { measurement } => Ok(Value::Measurement(measurement.clone())),
            Expr::Matrix {
                bracket,
                parameters,
            } => {
                let row_count = parameters.len();
                let col_count = parameters[0].len();
                let mut rows = Vec::with_capacity(row_count);
                for (row_idx, parameter_row) in parameters.iter().enumerate() {
                    let mut row = Vec::with_capacity(col_count);
                    for (col_idx, parmameter) in parameter_row.iter().enumerate() {
                        let value = parmameter.evaluate(variables)?;
                        match value {
                            Value::Number(num) => {
                                row.push(num);
                            }
                            _ => {
                                return Err(EvaluationError::InvalidMatrixParameter(Box::new(
                                    InvalidMatrixParameter {
                                        line: bracket.line,
                                        col: bracket.col,
                                        parameter_row: row_idx + 1,
                                        parameter_col: col_idx + 1,
                                        provided: value,
                                    },
                                )))
                            }
                        }
                    }
                    rows.push(row);
                }
                Ok(Value::Matrix(Matrix::from_rows(rows)))
            }
            Expr::Identifier { name } => Self::evaluate_identifier(name, variables),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => Self::evaluate_call(callee, paren, arguments, variables),
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
                (Value::Measurement(measurement1), Value::Measurement(measurement2))
                    if std::mem::discriminant(&measurement1.unit)
                        == std::mem::discriminant(&measurement2.unit) =>
                {
                    let measurement1 = measurement1.to_si_base_unit();
                    let measurement2 = measurement2.to_si_base_unit();
                    return Ok(Value::Measurement(Measurement::new(
                        measurement1.num + measurement2.num,
                        measurement1.unit,
                    )));
                }
                (Value::Matrix(matrix1), Value::Matrix(matrix2))
                    if matrix1.dimensions() == matrix2.dimensions() =>
                {
                    return Ok(Value::Matrix(matrix1 + matrix2))
                }
                _ => {}
            },
            TokenKind::Minus => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    return Ok(Value::Number(left - right))
                }
                (Value::Measurement(measurement1), Value::Measurement(measurement2))
                    if std::mem::discriminant(&measurement1.unit)
                        == std::mem::discriminant(&measurement2.unit) =>
                {
                    let measurement1 = measurement1.to_si_base_unit();
                    let measurement2 = measurement2.to_si_base_unit();
                    return Ok(Value::Measurement(Measurement::new(
                        measurement1.num - measurement2.num,
                        measurement1.unit,
                    )));
                }
                (Value::Matrix(matrix1), Value::Matrix(matrix2))
                    if matrix1.dimensions() == matrix2.dimensions() =>
                {
                    return Ok(Value::Matrix(matrix1 - matrix2))
                }
                _ => {}
            },
            TokenKind::Star => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    return Ok(Value::Number(left * right))
                }
                (Value::Number(scalar), Value::Matrix(matrix)) => {
                    return Ok(Value::Matrix(matrix * *scalar))
                }
                (Value::Matrix(matrix), Value::Number(scalar)) => {
                    return Ok(Value::Matrix(*scalar * matrix))
                }
                (Value::Matrix(matrix1), Value::Matrix(matrix2))
                    if matrix1.cols() == matrix2.rows() =>
                {
                    return Ok(Value::Matrix(matrix1 * matrix2))
                }
                (Value::Number(num), Value::Measurement(measurement)) => {
                    let measurement = measurement.to_si_base_unit();
                    return Ok(Value::Measurement(Measurement::new(
                        num * measurement.num,
                        measurement.unit,
                    )));
                }
                (Value::Measurement(measurement), Value::Number(num)) => {
                    let measurement = measurement.to_si_base_unit();
                    return Ok(Value::Measurement(Measurement::new(
                        num * measurement.num,
                        measurement.unit,
                    )));
                }
                _ => {}
            },
            TokenKind::Slash => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    if right.norm() == 0.0 {
                        return Err(EvaluationError::DivisionByZero(Box::new(DivisionByZero {
                            line: operator.line,
                            col: operator.col,
                        })));
                    }
                    return Ok(Value::Number(left / right));
                }
                (Value::Matrix(matrix), Value::Number(divisor)) => {
                    if divisor.norm() == 0.0 {
                        return Err(EvaluationError::DivisionByZero(Box::new(DivisionByZero {
                            line: operator.line,
                            col: operator.col,
                        })));
                    }
                    return Ok(Value::Matrix(matrix / *divisor));
                }
                (Value::Measurement(measurement), Value::Number(num)) => {
                    let measurement = measurement.to_si_base_unit();
                    return Ok(Value::Measurement(Measurement::new(
                        num / measurement.num,
                        measurement.unit,
                    )));
                }
                _ => {}
            },
            TokenKind::Caret => {
                if let (Value::Number(left), Value::Number(right)) = (&left, &right) {
                    return Ok(Value::Number(left.powc(*right)));
                }
            }
            TokenKind::Percent => {
                if let (Value::Number(left), Value::Number(right)) = (&left, &right) {
                    if right.norm() == 0.0 {
                        return Err(EvaluationError::DivisionByZero(Box::new(DivisionByZero {
                            line: operator.line,
                            col: operator.col,
                        })));
                    }
                    return Ok(Value::Number(left % right));
                }
            }
            TokenKind::Dot => match (&left, &right) {
                // Row vector dot product
                (Value::Matrix(left), Value::Matrix(right))
                    if left.rows() == 1 && right.rows() == 1 && left.cols() == right.cols() =>
                {
                    return Ok(Value::Number(left.row_dot(right)));
                }
                // column vector dot product
                (Value::Matrix(left), Value::Matrix(right))
                    if left.cols() == 1 && right.cols() == 1 && left.rows() == right.rows() =>
                {
                    return Ok(Value::Number(left.column_dot(right)));
                }
                _ => {}
            },
            TokenKind::Cross => match (&left, &right) {
                // Row vector cross product
                (Value::Matrix(left), Value::Matrix(right))
                    if left.rows() == 1
                        && right.rows() == 1
                        && left.cols() == 3
                        && right.cols() == 3 =>
                {
                    return Ok(Value::Matrix(left.row_cross(right)));
                }
                // Column vector dot product
                (Value::Matrix(left), Value::Matrix(right))
                    if left.cols() == 1
                        && right.cols() == 1
                        && left.rows() == 3
                        && right.rows() == 3 =>
                {
                    return Ok(Value::Matrix(left.column_cross(right)));
                }
                _ => {}
            },
            kind => panic!("Invalid token kind for binary operation: {:?}", kind),
        }
        // None were matched, unsupported
        Err(EvaluationError::UnsupportedBinaryOperator(Box::new(
            UnsupportedBinaryOperator {
                left,
                operator: operator.clone(),
                right,
            },
        )))
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
                    return Ok(Value::Number(right * Complex64::from(-1.0)));
                }
                Value::Measurement(measurement) => {
                    let measurement = measurement.to_si_base_unit();
                    return Ok(Value::Measurement(Measurement::new(
                        measurement.num * Complex64::from(-1.0),
                        measurement.unit,
                    )));
                }
                Value::Matrix(matrix) => return Ok(Value::Matrix(-matrix)),
                _ => {}
            },
            TokenKind::Sqrt => {
                if let Value::Number(right) = &operand {
                    return Ok(Value::Number(right.sqrt()));
                }
            }
            TokenKind::Bang => {
                if let Value::Number(right) = &operand {
                    if ValueConstraint::Natural.does_value_fit(&operand) {
                        return Ok(Value::Number(factorial(right.re as u64).into()));
                    } else {
                        return Err(EvaluationError::UnaryOperatorValueConstraintNotMet(
                            Box::new(UnaryOperatorValueConstraintNotMet {
                                operator: operator.clone(),
                                value: operand,
                                constraint: ValueConstraint::Natural,
                            }),
                        ));
                    }
                }
            }
            kind => panic!("Invalid token kind for unary operation: {:?}", kind),
        }
        Err(EvaluationError::UnsupportedUnaryOperator(Box::new(
            UnsupportedUnaryOperator {
                operator: operator.clone(),
                operand,
            },
        )))
    }

    fn evaluate_grouping(
        paren: &Token,
        kind: GroupingKind,
        expr: &Expr,
        variables: &mut VariableMap<'a>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let value = expr.evaluate(variables)?;
        match kind {
            GroupingKind::Grouping => return Ok(value),
            GroupingKind::Absolute => match &value {
                Value::Number(result) => return Ok(Value::Number(result.norm().into())),
                Value::Matrix(matrix) if matrix.rows() == 1 => {
                    return Ok(Value::Number(
                        matrix.rows[0]
                            .iter()
                            .map(|x| x * x)
                            .sum::<Complex64>()
                            .sqrt(),
                    ))
                }
                Value::Matrix(matrix) if matrix.cols() == 1 => {
                    return Ok(Value::Number(
                        matrix
                            .rows
                            .iter()
                            .map(|x| x[0] * x[0])
                            .sum::<Complex64>()
                            .sqrt(),
                    ))
                }
                _ => {}
            },
            GroupingKind::Ceil => {
                if let Value::Number(num) = value {
                    if ValueConstraint::Real.does_value_fit(&value) {
                        return Ok(Value::Number(num.re.ceil().into()));
                    } else {
                        return Err(EvaluationError::GroupingValueConstraintNotMet(Box::new(
                            GroupingValueConstraintNotMet {
                                line: paren.line,
                                col: paren.col,
                                kind,
                                constraint: ValueConstraint::Real,
                            },
                        )));
                    }
                }
            }
            GroupingKind::Floor => {
                if let Value::Number(num) = value {
                    if ValueConstraint::Real.does_value_fit(&value) {
                        return Ok(Value::Number(num.re.floor().into()));
                    } else {
                        return Err(EvaluationError::GroupingValueConstraintNotMet(Box::new(
                            GroupingValueConstraintNotMet {
                                line: paren.line,
                                col: paren.col,
                                kind,
                                constraint: ValueConstraint::Real,
                            },
                        )));
                    }
                }
            }
        };
        Err(EvaluationError::InvalidGroupingOperand(Box::new(
            InvalidGroupingOperand {
                line: paren.line,
                col: paren.col,
                kind,
                value,
            },
        )))
    }

    fn evaluate_identifier(
        name: &Token,
        variables: &mut VariableMap<'a>,
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match variables.get(name.lexeme.as_str()) {
            Some(variable) => Ok(variable.value.clone()),
            _ => Err(EvaluationError::UnknownVariable(Box::new(
                UnknownVariable { name: name.clone() },
            ))),
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
            _ => Err(EvaluationError::InvalidCallable(Box::new(
                InvalidCallable {
                    line: paren.line,
                    col: paren.col,
                },
            ))),
        }
    }

    pub fn get_type_string(&self) -> &'static str {
        match self {
            Expr::As {
                expr: _,
                _as: _,
                unit: _,
            } => "as expression",
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
            Expr::Measurement { measurement: _ } => "measurement",
            Expr::Matrix {
                bracket: _,
                parameters,
            } => {
                let row_count = parameters.len();
                let col_count = parameters[0].len();
                if row_count == 1 {
                    "row vector"
                } else if col_count == 1 {
                    "column vector"
                } else {
                    "matrix"
                }
            }
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
            Expr::As { expr, _as: _, unit } => write!(f, "{} as {}", expr, unit),
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
            Expr::Measurement { measurement } => write!(f, "{measurement}"),
            Expr::Matrix {
                bracket: _,
                parameters,
            } => write!(f, "{}", matrix_format(parameters)),
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

// Computes factorial of a non-negative integer
fn factorial(n: u64) -> f64 {
    (2..=n).map(|x| x as f64).product()
}
