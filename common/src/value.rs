use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

use num_complex::Complex64;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use crate::{expr::complex_to_string, function::Function, matrix::Matrix};

#[derive(Debug)]
pub enum ValueConstraint {
    Function,
    Number,
    Real,
    Natural,
    Integer,
    PositiveInteger,
    Matrix,
}

impl FromStr for ValueConstraint {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "function" => Ok(ValueConstraint::Function),
            "number" => Ok(ValueConstraint::Number),
            "real" => Ok(ValueConstraint::Real),
            "natural" => Ok(ValueConstraint::Natural),
            "integer" => Ok(ValueConstraint::Integer),
            "positive_integer" => Ok(ValueConstraint::PositiveInteger),
            "matrix" => Ok(ValueConstraint::Matrix),
            _ => Err(()),
        }
    }
}

impl fmt::Display for ValueConstraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueConstraint::Function => write!(f, "function"),
            ValueConstraint::Number => write!(f, "number"),
            ValueConstraint::Real => write!(f, "real"),
            ValueConstraint::Natural => write!(f, "natural"),
            ValueConstraint::Integer => write!(f, "integer"),
            ValueConstraint::PositiveInteger => write!(f, "positive integer"),
            ValueConstraint::Matrix => write!(f, "matrix"),
        }
    }
}

impl ToTokens for ValueConstraint {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let token_str = match self {
            ValueConstraint::Function => quote! { ValueConstraint::Function },
            ValueConstraint::Number => quote! { ValueConstraint::Number },
            ValueConstraint::Real => quote! { ValueConstraint::Real },
            ValueConstraint::Natural => quote! { ValueConstraint::Natural },
            ValueConstraint::Integer => quote! { ValueConstraint::Integer },
            ValueConstraint::PositiveInteger => quote! { ValueConstraint::PositiveInteger },
            ValueConstraint::Matrix => quote! { ValueConstraint::Matrix }
        };
        tokens.extend(token_str);
    }
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Function(Rc<RefCell<Function<'a>>>),
    Number(Complex64),
    Matrix(Matrix),
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Function(func) => write!(f, "{}", func.borrow()),
            Value::Number(num) => write!(f, "{}", complex_to_string(&num)),
            Value::Matrix(matrix) => write!(f, "{matrix}"),
        }
    }
}

impl Value<'_> {
    pub fn fits_value_constraint(&self, constraint: ValueConstraint) -> bool {
        match constraint {
            ValueConstraint::Function => match self {
                Value::Function(_) => true,
                _ => false,
            },
            ValueConstraint::Number => match self {
                Value::Number(_) => true,
                _ => false,
            },
            ValueConstraint::Real => match self {
                Value::Number(num) => num.im == 0.0,
                _ => false,
            },
            ValueConstraint::Natural => match self {
                Value::Number(num) => num.im == 0.0 && num.re.fract() == 0.0 && num.re >= 0.0,
                _ => false,
            },
            ValueConstraint::Integer => match self {
                Value::Number(num) => num.im == 0.0 && num.re.fract() == 0.0,
                _ => false,
            },
            ValueConstraint::PositiveInteger => match self {
                Value::Number(num) => num.im == 0.0 && num.re.fract() == 0.0 && num.re > 0.0,
                _ => false
            },
            ValueConstraint::Matrix => match self {
                Value::Matrix(_) => true,
                _ => false
            }
        }
    }

    pub fn get_type_string(&self) -> String {
        match self {
            Value::Function(_) => String::from("function"),
            Value::Number(_) => String::from("number"),
            Value::Matrix(matrix) => {
                let one_row = matrix.rows() == 1;
                let one_col = matrix.cols() == 1;
                if one_row ^ one_col {
                    if one_row {
                        format!("{}D row vector", matrix.cols())
                    } else {
                        format!("{}D column vector", matrix.rows())
                    }
                } else {
                    // matrix
                    format!("{}x{} matrix", matrix.rows(), matrix.cols())
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use num::Zero;

    use crate::{expr::EvaluationError, function::NativeFunction};

    use super::*;

    #[test]
    fn test_value_get_type_string() {
        fn dummy(_: usize, _: usize, _: Vec<Value>) -> Result<Value, EvaluationError> {
            Ok(Value::Number(Complex64::zero()))
        }

        for (input, result) in &[
            (Value::Number(Complex64::zero()), String::from("number")),
            (
                Value::Function(Rc::new(RefCell::new(Function::NativeFunction(
                    NativeFunction {
                        name: "dummy",
                        function: dummy,
                        arity: 0,
                    },
                )))),
                String::from("function"),
            ),
            (
                Value::Matrix(Matrix::from_rows(vec![vec![
                    Complex64::zero(),
                    Complex64::zero(),
                ]])),
                String::from("2D row vector"),
            ),
            (
                Value::Matrix(Matrix::from_rows(vec![
                    vec![Complex64::zero()],
                    vec![Complex64::zero()],
                ])),
                String::from("2D column vector"),
            ),
            (
                Value::Matrix(Matrix::from_rows(vec![vec![Complex64::zero()]])),
                String::from("1x1 matrix"),
            ),
            (
                Value::Matrix(Matrix::from_rows(vec![
                    vec![Complex64::zero(), Complex64::zero()],
                    vec![Complex64::zero(), Complex64::zero()],
                ])),
                String::from("2x2 matrix"),
            ),
        ] {
            assert_eq!(input.get_type_string(), *result)
        }
    }
}
