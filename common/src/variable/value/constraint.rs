use std::{fmt, str::FromStr};

use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;

use super::Value;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueConstraint {
    Function,
    Number,
    Real,
    Natural,
    Integer,
    PositiveInteger,
    Matrix,
    SquareMatrix,
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
            "square_matrix" => Ok(ValueConstraint::SquareMatrix),
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
            ValueConstraint::SquareMatrix => write!(f, "square matrix"),
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
            ValueConstraint::Matrix => quote! { ValueConstraint::Matrix },
            ValueConstraint::SquareMatrix => quote! { ValueConstraint::SquareMatrix },
        };
        tokens.extend(token_str);
    }
}

impl ValueConstraint {
    pub fn does_value_fit(&self, value: &Value) -> bool {
        match self {
            ValueConstraint::Function => match value {
                Value::Function(_) => true,
                _ => false,
            },
            ValueConstraint::Number => match value {
                Value::Number(_) => true,
                _ => false,
            },
            ValueConstraint::Real => match value {
                Value::Number(num) => num.im == 0.0,
                _ => false,
            },
            ValueConstraint::Natural => match value {
                Value::Number(num) => num.im == 0.0 && num.re.fract() == 0.0 && num.re >= 0.0,
                _ => false,
            },
            ValueConstraint::Integer => match value {
                Value::Number(num) => num.im == 0.0 && num.re.fract() == 0.0,
                _ => false,
            },
            ValueConstraint::PositiveInteger => match value {
                Value::Number(num) => num.im == 0.0 && num.re.fract() == 0.0 && num.re > 0.0,
                _ => false,
            },
            ValueConstraint::Matrix => match value {
                Value::Matrix(_) => true,
                _ => false,
            },
            ValueConstraint::SquareMatrix => match value {
                Value::Matrix(matrix) => matrix.rows() == matrix.cols(),
                _ => false,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use num_complex::Complex64;

    use crate::variable::value::{
        function::{Function, NativeFunction, UserDefinedFunction},
        matrix::Matrix,
        measurement::Measurement,
        unit::{MassUnit, Unit},
    };

    use super::*;

    fn get_test_cases() -> Vec<Value<'static>> {
        vec![
            // Native function
            Value::from_function(Function::NativeFunction(NativeFunction {
                name: "bruh",
                function: |_, _, _| unimplemented!(),
                arity: 0,
            })),
            // User defined function
            Value::from_function(Function::UserDefinedFunction(UserDefinedFunction {
                name: String::from("test"),
                signatures: vec![],
            })),
            // Number, Natural, real, integer
            Value::Number(Complex64::ZERO),
            // Number, Natural, real, integer, positive_integer
            Value::Number(Complex64::from(1.0)),
            // Number, Natural, real, integer
            Value::Number(Complex64::from(-1.0)),
            // Number, real
            Value::Number(Complex64::from(1.5)),
            // Number, real
            Value::Number(Complex64::from(-1.5)),
            // Number
            Value::Number(Complex64::i()),
            // Number
            Value::Number(Complex64::new(2.5, 3.5)),
            // fits none
            Value::Measurement(Measurement::new(
                Complex64::new(1.0, 2.0),
                Unit::Mass(MassUnit::Kilogram),
            )),
            // matrix, square matrix
            Value::Matrix(Matrix::from_rows(vec![vec![Complex64::ZERO]])),
            // matrix
            Value::Matrix(Matrix::from_rows(vec![vec![
                Complex64::ZERO,
                Complex64::ZERO,
            ]])),
            // matrix
            Value::Matrix(Matrix::from_rows(vec![
                vec![Complex64::ZERO],
                vec![Complex64::ZERO],
            ])),
            // matrix, square matrix
            Value::Matrix(Matrix::from_rows(vec![
                vec![Complex64::ZERO, Complex64::ZERO],
                vec![Complex64::ZERO, Complex64::ZERO],
            ])),
        ]
    }

    fn perform_value_constraint_test(value_constraint: ValueConstraint, values: &[bool]) {
        let test_cases = get_test_cases();
        assert_eq!(test_cases.len(), values.len());
        for (input, &expected) in test_cases.iter().zip(values.iter()) {
            assert_eq!(
                value_constraint.does_value_fit(input),
                expected,
                "{value_constraint} {input}"
            )
        }
    }

    #[test]
    fn test_value_constraint_from_str() {
        for (input, result) in [
            ("function", Ok(ValueConstraint::Function)),
            ("number", Ok(ValueConstraint::Number)),
            ("real", Ok(ValueConstraint::Real)),
            ("natural", Ok(ValueConstraint::Natural)),
            ("integer", Ok(ValueConstraint::Integer)),
            ("positive_integer", Ok(ValueConstraint::PositiveInteger)),
            ("matrix", Ok(ValueConstraint::Matrix)),
            ("square_matrix", Ok(ValueConstraint::SquareMatrix)),
            ("blah blah", Err(())),
            ("square matrix", Err(())),
            ("positive integer", Err(())),
        ] {
            assert_eq!(ValueConstraint::from_str(input), result)
        }
    }

    #[test]
    fn test_display_value_constraint() {
        for (constraint, expected) in [
            (ValueConstraint::Function, "function"),
            (ValueConstraint::Number, "number"),
            (ValueConstraint::Real, "real"),
            (ValueConstraint::Natural, "natural"),
            (ValueConstraint::Integer, "integer"),
            (ValueConstraint::PositiveInteger, "positive integer"),
            (ValueConstraint::Matrix, "matrix"),
            (ValueConstraint::SquareMatrix, "square matrix"),
        ] {
            assert_eq!(constraint.to_string(), expected)
        }
    }

    #[test]
    fn test_value_constraint_to_tokens() {
        let cases = [
            (
                ValueConstraint::Function,
                quote! { ValueConstraint::Function },
            ),
            (ValueConstraint::Number, quote! { ValueConstraint::Number }),
            (ValueConstraint::Real, quote! { ValueConstraint::Real }),
            (
                ValueConstraint::Natural,
                quote! { ValueConstraint::Natural },
            ),
            (
                ValueConstraint::Integer,
                quote! { ValueConstraint::Integer },
            ),
            (
                ValueConstraint::PositiveInteger,
                quote! { ValueConstraint::PositiveInteger },
            ),
            (ValueConstraint::Matrix, quote! { ValueConstraint::Matrix }),
            (
                ValueConstraint::SquareMatrix,
                quote! { ValueConstraint::SquareMatrix },
            ),
        ];

        for (constraint, expected_tokens) in cases.iter() {
            let mut tokens = TokenStream::new();
            constraint.to_tokens(&mut tokens);
            assert_eq!(tokens.to_string(), expected_tokens.to_string());
        }
    }

    #[test]
    fn test_function_value_constraint() {
        perform_value_constraint_test(
            ValueConstraint::Function,
            &[
                true, true, false, false, false, false, false, false, false, false, false, false,
                false, false,
            ],
        );
    }

    #[test]
    fn test_number_value_constraint() {
        perform_value_constraint_test(
            ValueConstraint::Number,
            &[
                false, false, true, true, true, true, true, true, true, false, false, false, false,
                false,
            ],
        );
    }

    #[test]
    fn test_real_value_constraint() {
        perform_value_constraint_test(
            ValueConstraint::Real,
            &[
                false, false, true, true, true, true, true, false, false, false, false, false,
                false, false,
            ],
        );
    }

    #[test]
    fn test_natural_value_constraint() {
        perform_value_constraint_test(
            ValueConstraint::Natural,
            &[
                false, false, true, true, false, false, false, false, false, false, false, false,
                false, false,
            ],
        );
    }

    #[test]
    fn test_integer_value_constraint() {
        perform_value_constraint_test(
            ValueConstraint::Integer,
            &[
                false, false, true, true, true, false, false, false, false, false, false, false,
                false, false,
            ],
        );
    }

    #[test]
    fn test_positive_integer_value_constraint() {
        perform_value_constraint_test(
            ValueConstraint::PositiveInteger,
            &[
                false, false, false, true, false, false, false, false, false, false, false, false,
                false, false,
            ],
        );
    }

    #[test]
    fn test_matrix_value_constraint() {
        perform_value_constraint_test(
            ValueConstraint::Matrix,
            &[
                false, false, false, false, false, false, false, false, false, false, true, true,
                true, true,
            ],
        );
    }

    #[test]
    fn test_square_matrix_value_constraint() {
        perform_value_constraint_test(
            ValueConstraint::SquareMatrix,
            &[
                false, false, false, false, false, false, false, false, false, false, true, false,
                false, true,
            ],
        );
    }
}
