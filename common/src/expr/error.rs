use crate::{
    tokenizer::Token,
    value::{Value, ValueConstraint},
};
use std::fmt;

use super::GroupingKind;

#[derive(Debug)]
pub enum EvaluationError<'a> {
    NativeFunctionIncorrectParameterCount(Box<NativeFunctionIncorrectParameterCount<'a>>),
    NativeFunctionIncorrectParameterType(Box<NativeFunctionIncorrectParameterType>),
    NativeFunctionCantAddSignature(Box<NativeFunctionCantAddSignature>),
    NativeFunctionCantDeleteSignature(Box<NativeFunctionCantDeleteSignature>),
    UserDefinedFunctionNoMatchingSignature(Box<UserDefinedFunctionNoMatchingSignature>),
    DivisionByZero(Box<DivisionByZero>),
    UnsupportedBinaryOperator(Box<UnsupportedBinaryOperator<'a>>),
    UnsupportedUnaryOperator(Box<UnsupportedUnaryOperator<'a>>),
    UnaryOperatorValueConstraintNotMet(Box<UnaryOperatorValueConstraintNotMet<'a>>),
    InvalidGroupingOperand(Box<InvalidGroupingOperand<'a>>),
    GroupingValueConstraintNotMet(Box<GroupingValueConstraintNotMet>),
    InvalidCallable(Box<InvalidCallable>),
    ConstantAssignment(Box<ConstantAssignment>),
    ConstantDeletion(Box<ConstantDeletion>),
    UnknownVariable(Box<UnknownVariable>),
    InvalidMatrixParameter(Box<InvalidMatrixParameter<'a>>),
    NoInverseForMatrix(Box<NoInverseForMatrix>),
}

impl<'a> std::error::Error for EvaluationError<'a> {}

impl<'a> fmt::Display for EvaluationError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvaluationError::DivisionByZero(err)=> write!(
                f,
                "Line {}, Column {} :: Divison by zero",
                err.line,
                err.col,
            ),
            EvaluationError::NativeFunctionIncorrectParameterCount(err) => write!(
                f,
                "Line {}, Column {} :: Function '{}' requires {} argument(s) but received {}",
                err.line, err.col, err.name, err.required, err.received,
            ),
            EvaluationError::UserDefinedFunctionNoMatchingSignature(err) => write!(
                f,
                "Line {}, Column {} :: No matching parameter signature for function '{}'. Type '{}' to see list of signatures",
                err.line,
                err.col,
                err.name,
                err.name
            ),
            EvaluationError::InvalidCallable(err) => write!(
                f,
                "Line {}, Column {} :: Callee does not meet value constraint '{}'",
                err.line,
                err.col,
                ValueConstraint::Function
            ),
            EvaluationError::InvalidGroupingOperand(err) => write! {
                f,
                "Line {}, Column {} :: '{}' is a not a supported type for a '{}'",
                err.line,
                err.col,
                &err.value.get_type_string(),
                err.kind.get_type_string()
            },
            EvaluationError::GroupingValueConstraintNotMet(err) => write!(
                f,
                "Line {}, Column {} :: Value in {} does meet value constraint '{}'",
                err.line,
                err.col,
                err.kind.get_type_string(),
                err.constraint
            ),
            EvaluationError::NativeFunctionIncorrectParameterType(err) => write!(
                f,
                "Line {}, Column {} :: Argument {} ({}) for function '{}' does not meet value constraint '{}'",
                err.line,
                err.col,
                err.idx,
                err.name,
                err.function_name,
                err.constraint,
            ),
            EvaluationError::ConstantAssignment(err) => write!(
                f,
                "Line {}, Column {} :: Cannot assign to '{}' as it is constant",
                err.name.line,
                err.name.col,
                &err.name.lexeme
            ),
            EvaluationError::ConstantDeletion(err) => write!(
                f,
                "Line {}, Column {} :: Cannot delete '{}' as it is constant",
                err.name.line,
                err.name.col,
                &err.name.lexeme
            ),
            EvaluationError::UnknownVariable(err) => write!(
                f,
                "Line {}, Column {} :: Unknown variable '{}'",
                err.name.line,
                err.name.col,
                &err.name.lexeme
            ),
            EvaluationError::NativeFunctionCantAddSignature(err) => write!(
                f,
                "Line {}, Column {} :: Can't add signature to native function '{}'",
                err.name.line,
                err.name.col,
                &err.name.lexeme
            ),
            EvaluationError::NativeFunctionCantDeleteSignature(err) => write!(
                f,
                "Line {}, Column {} :: Can't delete signature from native function '{}'",
                err.name.line,
                err.name.col,
                &err.name.lexeme
            ),
            EvaluationError::UnsupportedBinaryOperator(err) => write!(
                f,
                "Line {}, Column {} :: Binary operator '{}' not supported between types '{}' and '{}'",
                err.operator.line,
                err.operator.col,
                &err.operator.lexeme,
                &err.left.get_type_string(),
                &err.right.get_type_string()
            ),
            EvaluationError::UnsupportedUnaryOperator(err) => write!(
                f,
                "Line {}, Column {} :: Unary operator '{}' not supported for type '{}'",
                err.operator.line,
                err.operator.col,
                &err.operator.lexeme,
                &err.operand.get_type_string()
            ),
            EvaluationError::UnaryOperatorValueConstraintNotMet(err) => write!(
                f,
                "Line {}, Column {} :: Cannot perform unary operator '{}' on type '{}' as it does not meet value constraint '{}'",
                err.operator.line,
                err.operator.col,
                &err.operator.lexeme,
                &err.value.get_type_string(),
                err.constraint
            ),
            EvaluationError::InvalidMatrixParameter(err) => write!(
                f,
                "Line {}, Column {} :: Invalid value passed at position ({}, {}) when constructor vector or matrix - value must be a number, but a '{}' was provided",
                err.line,
                err.col,
                err.parameter_row,
                err.parameter_col,
                &err.provided.get_type_string()
            ),
            EvaluationError::NoInverseForMatrix(err) => write!(
                f,
                "Line {}, Column {} :: Cannot invert matrix as its determinant is zero",
                err.line,
                err.col
            ),
        }
    }
}

#[derive(Debug)]
pub struct NativeFunctionIncorrectParameterCount<'a> {
    pub line: usize,
    pub col: usize,
    pub name: &'a str,
    pub received: usize,
    pub required: usize,
}

#[derive(Debug)]
pub struct NativeFunctionIncorrectParameterType {
    pub function_name: String,
    pub line: usize,
    pub col: usize,
    pub idx: usize,
    pub name: String,
    pub constraint: ValueConstraint,
}

#[derive(Debug)]
pub struct NativeFunctionCantAddSignature {
    pub name: Token,
}

#[derive(Debug)]
pub struct NativeFunctionCantDeleteSignature {
    pub name: Token,
}

#[derive(Debug)]
pub struct UserDefinedFunctionNoMatchingSignature {
    pub line: usize,
    pub col: usize,
    pub name: String,
}

#[derive(Debug)]
pub struct DivisionByZero {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug)]
pub struct UnsupportedBinaryOperator<'a> {
    pub left: Value<'a>,
    pub operator: Token,
    pub right: Value<'a>,
}

#[derive(Debug)]
pub struct UnsupportedUnaryOperator<'a> {
    pub operator: Token,
    pub operand: Value<'a>,
}

#[derive(Debug)]
pub struct UnaryOperatorValueConstraintNotMet<'a> {
    pub operator: Token,
    pub value: Value<'a>,
    pub constraint: ValueConstraint,
}

#[derive(Debug)]
pub struct InvalidGroupingOperand<'a> {
    pub line: usize,
    pub col: usize,
    pub kind: GroupingKind,
    pub value: Value<'a>,
}

#[derive(Debug)]
pub struct GroupingValueConstraintNotMet {
    pub line: usize,
    pub col: usize,
    pub kind: GroupingKind,
    pub constraint: ValueConstraint,
}

#[derive(Debug)]
pub struct InvalidCallable {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug)]
pub struct ConstantAssignment {
    pub name: Token,
}

#[derive(Debug)]
pub struct ConstantDeletion {
    pub name: Token,
}

#[derive(Debug)]
pub struct UnknownVariable {
    pub name: Token,
}

#[derive(Debug)]
pub struct InvalidMatrixParameter<'a> {
    pub line: usize,
    pub col: usize,
    pub parameter_row: usize,
    pub parameter_col: usize,
    pub provided: Value<'a>,
}

#[derive(Debug)]
pub struct NoInverseForMatrix {
    pub line: usize,
    pub col: usize,
}
