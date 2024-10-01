use std::fmt;

use crate::{
    matrix::{matrix_format, Matrix},
    num_complex::Complex64,
    tokenizer::{Token, TokenKind},
    value::{complex_to_string, Value, ValueConstraint},
    variable::VariableMap,
};

use super::{
    DivisionByZero, EvaluationError, GroupingValueConstraintNotMet, InvalidCallable,
    InvalidGroupingOperand, InvalidMatrixParameter, UnaryOperatorValueConstraintNotMet,
    UnknownVariable, UnsupportedBinaryOperator, UnsupportedUnaryOperator,
};

#[derive(Debug, Copy, Clone)]
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
                    return Ok(Value::Matrix(matrix / *divisor))
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
                        return Err(EvaluationError::DivisionByZero(Box::new(DivisionByZero {
                            line: operator.line,
                            col: operator.col,
                        })));
                    }
                    return Ok(Value::Number(left % right));
                }
                _ => {}
            },
            TokenKind::Dot => match (&left, &right) {
                // Row vector dot product
                (Value::Matrix(left), Value::Matrix(right))
                    if left.rows() == 1 && right.rows() == 1 && left.cols() == right.cols() =>
                {
                    let left_values = &left.rows[0];
                    let right_values = &right.rows[0];
                    return Ok(Value::Number(
                        left_values
                            .iter()
                            .zip(right_values.iter())
                            .map(|(x, y)| x * y)
                            .sum(),
                    ));
                }
                // column vector dot product
                (Value::Matrix(left), Value::Matrix(right))
                    if left.cols() == 1 && right.cols() == 1 && left.rows() == right.rows() =>
                {
                    let left_values: Vec<Complex64> = left.rows.iter().map(|x| x[0]).collect();
                    let right_values: Vec<Complex64> = right.rows.iter().map(|x| x[0]).collect();
                    return Ok(Value::Number(
                        left_values
                            .iter()
                            .zip(right_values.iter())
                            .map(|(x, y)| x * y)
                            .sum(),
                    ));
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
                    let vec1 = &left.rows[0];
                    let a1 = vec1[0];
                    let a2 = vec1[1];
                    let a3 = vec1[2];

                    let vec2 = &right.rows[0];
                    let b1 = vec2[0];
                    let b2 = vec2[1];
                    let b3 = vec2[2];

                    let cross_product = Value::Matrix(Matrix::from_rows(vec![vec![
                        a2 * b3 - a3 * b2,
                        a3 * b1 - a1 * b3,
                        a1 * b2 - a2 * b1,
                    ]]));

                    return Ok(cross_product);
                }
                // Column vector dot product
                (Value::Matrix(left), Value::Matrix(right))
                    if left.cols() == 1
                        && right.cols() == 1
                        && left.rows() == 3
                        && right.rows() == 3 =>
                {
                    let vec1: Vec<Complex64> = left.rows.iter().map(|x| x[0]).collect();
                    let a1 = vec1[0];
                    let a2 = vec1[1];
                    let a3 = vec1[2];

                    let vec2: Vec<Complex64> = right.rows.iter().map(|x| x[0]).collect();
                    let b1 = vec2[0];
                    let b2 = vec2[1];
                    let b3 = vec2[2];

                    let cross_product = Value::Matrix(Matrix::from_rows(vec![vec![
                        a2 * b3 - a3 * b2,
                        a3 * b1 - a1 * b3,
                        a1 * b2 - a2 * b1,
                    ]]));

                    return Ok(cross_product);
                }
                _ => {}
            },
            kind => panic!("Invalid token kind for binary operation: {:?}", kind),
        }
        // None were matched, unsupported
        Err(EvaluationError::UnsupportedBinaryOperator(Box::new(
            UnsupportedBinaryOperator {
                left: left,
                operator: operator.clone(),
                right: right,
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
                    return Ok(Value::Number(right * Complex64::new(-1.0, 0.0)));
                }
                Value::Matrix(matrix) => return Ok(Value::Matrix(-matrix)),
                _ => {}
            },
            TokenKind::Sqrt => match &operand {
                Value::Number(right) => {
                    return Ok(Value::Number(right.sqrt()));
                }
                _ => {}
            },
            TokenKind::Bang => match &operand {
                Value::Number(right) => {
                    if operand.fits_value_constraint(ValueConstraint::Natural) {
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
                _ => {}
            },
            kind => panic!("Invalid token kind for unary operation: {:?}", kind),
        }
        Err(EvaluationError::UnsupportedUnaryOperator(Box::new(
            UnsupportedUnaryOperator {
                operator: operator.clone(),
                operand: operand,
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
            GroupingKind::Ceil => match value {
                Value::Number(num) => {
                    if value.fits_value_constraint(ValueConstraint::Real) {
                        return Ok(Value::Number(num.re.ceil().into()));
                    } else {
                        return Err(EvaluationError::GroupingValueConstraintNotMet(Box::new(
                            GroupingValueConstraintNotMet {
                                line: paren.line,
                                col: paren.col,
                                kind: kind,
                                constraint: ValueConstraint::Real,
                            },
                        )));
                    }
                }
                _ => {}
            },
            GroupingKind::Floor => match value {
                Value::Number(num) => {
                    if value.fits_value_constraint(ValueConstraint::Real) {
                        return Ok(Value::Number(num.re.floor().into()));
                    } else {
                        return Err(EvaluationError::GroupingValueConstraintNotMet(Box::new(
                            GroupingValueConstraintNotMet {
                                line: paren.line,
                                col: paren.col,
                                kind: kind,
                                constraint: ValueConstraint::Real,
                            },
                        )));
                    }
                }
                _ => {}
            },
        };
        Err(EvaluationError::InvalidGroupingOperand(Box::new(
            InvalidGroupingOperand {
                line: paren.line,
                col: paren.col,
                kind: kind,
                value: value,
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
            _ => {
                return Err(EvaluationError::InvalidCallable(Box::new(
                    InvalidCallable {
                        line: paren.line,
                        col: paren.col,
                    },
                )))
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
