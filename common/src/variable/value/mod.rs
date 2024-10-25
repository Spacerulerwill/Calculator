pub mod constraint;
pub mod function;
pub mod matrix;
pub mod measurement;
pub mod unit;

use std::{cell::RefCell, fmt, rc::Rc};

use function::Function;
use matrix::Matrix;
use measurement::Measurement;
use num_complex::Complex64;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Function(Rc<RefCell<Function<'a>>>),
    Number(Complex64),
    Measurement(Measurement),
    Matrix(Matrix),
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Function(func) => write!(f, "{}", func.borrow()),
            Value::Number(num) => write!(f, "{}", complex_to_string(&num)),
            Value::Measurement(measurement) => write!(f, "{measurement}"),
            Value::Matrix(matrix) => write!(f, "{matrix}"),
        }
    }
}

impl<'a> Value<'a> {
    pub fn from_function(function: Function<'a>) -> Self {
        Self::Function(Rc::new(RefCell::new(function)))
    }
}

impl Value<'_> {
    pub fn get_type_string(&self) -> String {
        match self {
            Value::Function(_) => String::from("function"),
            Value::Number(_) => String::from("number"),
            Value::Measurement(measurement) => String::from(measurement.unit.get_type_string()),
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

#[cfg(test)]
mod tests {
    use num::Zero;
    use num_complex::Complex64;

    use crate::variable::value::{
        complex_to_string,
        function::Function,
        matrix::Matrix,
        measurement::Measurement,
        unit::{DistanceUnit, Unit},
    };

    use super::{function::NativeFunction, Value};

    #[test]
    fn test_complex_to_string() {
        for (input, expected) in [
            (Complex64::new(3.5, 3.0), "3.5 + 3i"),
            (Complex64::new(3.0, -3.5), "3 - 3.5i"),
            (Complex64::new(3.5, 1.0), "3.5 + i"),
            (Complex64::new(3.0, -1.0), "3 - i"),
            (Complex64::new(-3.0, -3.0), "-3 - 3i"),
            (Complex64::new(3.0, 0.0), "3"),
            (Complex64::new(3.5, 0.0), "3.5"),
            (Complex64::new(-3.0, 0.0), "-3"),
            (Complex64::new(-3.5, 0.0), "-3.5"),
            (Complex64::new(0.0, 0.0), "0"),
            (Complex64::new(0.0, 3.0), "3i"),
            (Complex64::new(0.0, -3.0), "-3i"),
            (Complex64::new(0.0, 3.5), "3.5i"),
            (Complex64::new(0.0, -3.5), "-3.5i"),
            (Complex64::new(0.0, -1.0), "-i"),
            (Complex64::new(0.0, 1.0), "i"),
        ] {
            assert_eq!(complex_to_string(&input), expected);
        }
    }

    #[test]
    fn test_value_get_type_string() {
        let measurement = Measurement::new(Complex64::zero(), Unit::Distance(DistanceUnit::Meter));

        for (input, expected) in [
            (
                Value::from_function(Function::NativeFunction(NativeFunction {
                    name: "test",
                    function: |_, _, _| panic!(),
                    arity: 0,
                })),
                "function",
            ),
            (Value::Number(Complex64::zero()), "number"),
            (
                Value::Measurement(measurement.clone()),
                measurement.unit.get_type_string(),
            ),
            (
                Value::Matrix(Matrix::from_rows(vec![vec![Complex64::zero()]])),
                "1x1 matrix",
            ),
            (
                Value::Matrix(Matrix::from_rows(vec![
                    vec![Complex64::zero(), Complex64::zero(), Complex64::zero()],
                    vec![Complex64::zero(), Complex64::zero(), Complex64::zero()],
                    vec![Complex64::zero(), Complex64::zero(), Complex64::zero()],
                ])),
                "3x3 matrix",
            ),
            (
                Value::Matrix(Matrix::from_rows(vec![
                    vec![Complex64::zero(), Complex64::zero(), Complex64::zero()],
                    vec![Complex64::zero(), Complex64::zero(), Complex64::zero()],
                ])),
                "2x3 matrix",
            ),
            (
                Value::Matrix(Matrix::from_rows(vec![
                    vec![Complex64::zero(), Complex64::zero()],
                    vec![Complex64::zero(), Complex64::zero()],
                    vec![Complex64::zero(), Complex64::zero()],
                ])),
                "3x2 matrix",
            ),
            (
                Value::Matrix(Matrix::from_rows(vec![vec![
                    Complex64::zero(),
                    Complex64::zero(),
                    Complex64::zero(),
                ]])),
                "3D row vector",
            ),
            (
                Value::Matrix(Matrix::from_rows(vec![
                    vec![Complex64::zero()],
                    vec![Complex64::zero()],
                    vec![Complex64::zero()],
                ])),
                "3D column vector",
            ),
        ] {
            assert_eq!(input.get_type_string(), expected)
        }
    }
}
