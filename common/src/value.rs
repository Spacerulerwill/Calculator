use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

use num_complex::Complex64;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use crate::{function::Function, matrix::Matrix};

#[derive(Debug)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DistanceUnit {
    // Metric
    Nanometer,
    Micrometer,
    Millimeter,
    Centimeter,
    Meter,
    Kilometer,
    // Imperial
    Inch,
    Foot,
    Yard,
    Mile,
}

impl fmt::Display for DistanceUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DistanceUnit::Nanometer => write!(f, "nm"),
            DistanceUnit::Micrometer => write!(f, "μm"),
            DistanceUnit::Millimeter => write!(f, "mm"),
            DistanceUnit::Centimeter => write!(f, "cm"),
            DistanceUnit::Meter => write!(f, "m"),
            DistanceUnit::Kilometer => write!(f, "km"),
            DistanceUnit::Inch => write!(f, "in"),
            DistanceUnit::Foot => write!(f, "ft"),
            DistanceUnit::Yard => write!(f, "yd"),
            DistanceUnit::Mile => write!(f, "mi"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MassUnit {
    // Metric
    Nanogram,
    Microgram,
    Milligram,
    Gram,
    Kilogram,
    Tonne,
    // Imperial
    Ounce,
    Pound,
    Stone,
}

impl fmt::Display for MassUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MassUnit::Nanogram => write!(f, "ng"),
            MassUnit::Microgram => write!(f, "µg"),
            MassUnit::Milligram => write!(f, "mg"),
            MassUnit::Gram => write!(f, "g"),
            MassUnit::Kilogram => write!(f, "kg"),
            MassUnit::Tonne => write!(f, "t"),
            MassUnit::Ounce => write!(f, "oz"),
            MassUnit::Pound => write!(f, "lb"),
            MassUnit::Stone => write!(f, "st"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TemperatureUnit {
    Kelvin,
    Celsius,
    Fahrenheit,
}

impl fmt::Display for TemperatureUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemperatureUnit::Celsius => write!(f, "°C"),
            TemperatureUnit::Kelvin => write!(f, "°K"),
            TemperatureUnit::Fahrenheit => write!(f, "°F"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MeasurementKind {
    Distance(DistanceUnit),
    Mass(MassUnit),
    Temperature(TemperatureUnit),
}

impl MeasurementKind {
    pub fn get_type_string(&self) -> &'static str {
        match self {
            MeasurementKind::Distance(_) => "distance",
            MeasurementKind::Mass(_) => "mass",
            MeasurementKind::Temperature(_) => "temperature",
        }
    }
}

impl fmt::Display for MeasurementKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MeasurementKind::Distance(distance_unit) => write!(f, "{distance_unit}"),
            MeasurementKind::Mass(mass_unit) => write!(f, "{mass_unit}"),
            MeasurementKind::Temperature(temperature_unit) => write!(f, "{temperature_unit}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Measurement {
    pub num: Complex64,
    pub kind: MeasurementKind,
}

impl fmt::Display for Measurement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // If both real and imaginary parts are non-zero (i.e., complex number), wrap in brackets
        if self.num.re != 0.0 && self.num.im != 0.0 {
            write!(f, "({}){}", complex_to_string(&self.num), self.kind)
        } else {
            write!(f, "{}{}", complex_to_string(&self.num), self.kind)
        }
    }
}

impl Measurement {
    pub fn to_si_base_units(&self) -> Self {
        let si_measurement = match self.kind {
            MeasurementKind::Distance(distance_unit) => {
                let value_in_meters = match distance_unit {
                    DistanceUnit::Nanometer => self.num / Complex64::new(1e9, 0.0), // 1 nm = 1e-9 m
                    DistanceUnit::Micrometer => self.num / Complex64::new(1e6, 0.0), // 1 µm = 1e-6 m
                    DistanceUnit::Millimeter => self.num / Complex64::new(1e3, 0.0), // 1 mm = 1e-3 m
                    DistanceUnit::Centimeter => self.num / Complex64::new(1e2, 0.0), // 1 cm = 1e-2 m
                    DistanceUnit::Meter => self.num, // already in meters
                    DistanceUnit::Kilometer => self.num * Complex64::new(1e3, 0.0), // 1 km = 1000 m
                    DistanceUnit::Inch => self.num / Complex64::new(39.3701, 0.0), // 1 in = 0.0254 m
                    DistanceUnit::Foot => self.num / Complex64::new(3.28084, 0.0), // 1 ft = 0.3048 m
                    DistanceUnit::Yard => self.num / Complex64::new(1.09361, 0.0), // 1 yd = 0.9144 m
                    DistanceUnit::Mile => self.num / Complex64::new(0.000621371, 0.0), // 1 mi = 1609.34 m
                };
                Measurement {
                    num: value_in_meters,
                    kind: MeasurementKind::Distance(DistanceUnit::Meter), // Convert to meters
                }
            }
            MeasurementKind::Mass(mass_unit) => {
                let value_in_kilograms = match mass_unit {
                    MassUnit::Nanogram => self.num / Complex64::new(1e9, 0.0), // 1 ng = 1e-9 kg
                    MassUnit::Microgram => self.num / Complex64::new(1e6, 0.0), // 1 µg = 1e-6 kg
                    MassUnit::Milligram => self.num / Complex64::new(1e3, 0.0), // 1 mg = 1e-3 kg
                    MassUnit::Gram => self.num / Complex64::new(1.0, 0.0),     // 1 g = 1 kg
                    MassUnit::Kilogram => self.num, // already in kilograms
                    MassUnit::Tonne => self.num * Complex64::new(1e3, 0.0), // 1 t = 1000 kg
                    MassUnit::Ounce => self.num / Complex64::new(35.274, 0.0), // 1 oz = 0.0283495 kg
                    MassUnit::Pound => self.num / Complex64::new(2.20462, 0.0), // 1 lb = 0.453592 kg
                    MassUnit::Stone => self.num / Complex64::new(0.157473, 0.0), // 1 st = 6.35029 kg
                };
                Measurement {
                    num: value_in_kilograms,
                    kind: MeasurementKind::Mass(MassUnit::Kilogram), // Convert to kilograms
                }
            }
            MeasurementKind::Temperature(temperature_unit) => {
                let value_in_kelvin = match temperature_unit {
                    TemperatureUnit::Kelvin => self.num, // already in Kelvin
                    TemperatureUnit::Celsius => self.num + Complex64::new(273.15, 0.0), // K = °C + 273.15
                    TemperatureUnit::Fahrenheit => {
                        (self.num - Complex64::new(32.0, 0.0)) * Complex64::new(5.0 / 9.0, 0.0)
                            + Complex64::new(273.15, 0.0)
                    } // K = (°F - 32) × 5/9 + 273.15
                };
                Measurement {
                    num: value_in_kelvin,
                    kind: MeasurementKind::Temperature(TemperatureUnit::Kelvin), // Convert to Kelvin
                }
            }
        };

        si_measurement
    }
}

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
