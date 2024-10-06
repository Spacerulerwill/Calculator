pub mod constraint;
pub mod function;
pub mod matrix;
pub mod measurement;
pub mod unit;

use std::{cell::RefCell, fmt, rc::Rc};

use constraint::ValueConstraint;
use function::Function;
use matrix::Matrix;
use measurement::Measurement;
use num_complex::Complex64;

#[derive(Debug, Clone)]
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
                _ => false,
            },
            ValueConstraint::Matrix => match self {
                Value::Matrix(_) => true,
                _ => false,
            },
            ValueConstraint::SquareMatrix => match self {
                Value::Matrix(matrix) => matrix.rows() == matrix.cols(),
                _ => false,
            },
        }
    }

    pub fn get_type_string(&self) -> String {
        match self {
            Value::Function(_) => String::from("function"),
            Value::Number(_) => String::from("number"),
            Value::Measurement(measurement) => String::from(measurement.kind.get_type_string()),
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
