use common::{
    expr::{EvaluationError, NoInverseForMatrix},
    num::integer::{gcd as _gcd, lcm as _lcm},
    num_complex::Complex64,
    variable::{
        value::{matrix::Matrix, Value},
        Variable, VariableMap,
    },
};
use proc_macros::define_calculator_native_function;
use std::f64::consts::{E, PI, TAU};

const C: f64 = 299792458_f64;
const G: f64 = 9.80665_f64;
const PHI: f64 = 1.618033988749894848204586834365638118_f64;

// trigonometry
define_calculator_native_function!(sin, (val: number), Ok(Value::Number(val.sin())));
define_calculator_native_function!(cos, (val: number), Ok(Value::Number(val.cos())));
define_calculator_native_function!(tan, (val: number), Ok(Value::Number(val.tan())));
define_calculator_native_function!(asin, (val: number), Ok(Value::Number(val.asin())));
define_calculator_native_function!(acos, (val: number), Ok(Value::Number(val.acos())));
define_calculator_native_function!(atan, (val: number), Ok(Value::Number(val.atan())));
define_calculator_native_function!(sinh, (val: number), Ok(Value::Number(val.sinh())));
define_calculator_native_function!(cosh, (val: number), Ok(Value::Number(val.cosh())));
define_calculator_native_function!(tanh, (val: number), Ok(Value::Number(val.tanh())));
define_calculator_native_function!(asinh, (val: number), Ok(Value::Number(val.asinh())));
define_calculator_native_function!(acosh, (val: number), Ok(Value::Number(val.acosh())));
define_calculator_native_function!(atanh, (val: number), Ok(Value::Number(val.atanh())));
// complex numbers
define_calculator_native_function!(re, (val: number), Ok(Value::Number(val.re.into())));
define_calculator_native_function!(im, (val: number), Ok(Value::Number(val.im.into())));
define_calculator_native_function!(arg, (val: number), Ok(Value::Number(val.arg().into())));
define_calculator_native_function!(conj, (val: number), Ok(Value::Number(val.conj())));
// matrices
define_calculator_native_function!(identity, (size: positive_integer), Ok(Value::Matrix(Matrix::identity(size as usize))));
define_calculator_native_function!(transpose, (matrix: matrix), Ok(Value::Matrix(matrix.transpose())));
define_calculator_native_function!(determinant, (matrix: square_matrix), Ok(Value::Number(matrix.determinant())));
define_calculator_native_function!(inverse, (matrix: square_matrix), {
    if let Some(_inverse) = matrix.inverse() {
        Ok(Value::Matrix(_inverse))
    } else {
        Err(EvaluationError::NoInverseForMatrix(Box::new(NoInverseForMatrix { line: line, col: col })))
    }
});

// other
define_calculator_native_function!(abs, (val: number), Ok(Value::Number(Complex64::from(val.abs()))));
define_calculator_native_function!(ceil, (val: real), Ok(Value::Number(Complex64::from(val.ceil()))));
define_calculator_native_function!(floor, (val: real), Ok(Value::Number(Complex64::from(val.floor()))));
define_calculator_native_function!(log, (base: real, val: number), Ok(Value::Number(val.log(base))));
define_calculator_native_function!(log2, (val: number), Ok(Value::Number(val.log2())));
define_calculator_native_function!(log10, (val: number), Ok(Value::Number(val.log10())));
define_calculator_native_function!(ln, (val: number), Ok(Value::Number(val.ln())));
define_calculator_native_function!(sqrt, (val: number), Ok(Value::Number(val.sqrt())));
define_calculator_native_function!(gcd, (first: integer, second: integer), Ok(Value::Number(Complex64::from(_gcd(first as i64, second as i64) as f64))));
define_calculator_native_function!(lcm, (first: integer, second: integer), Ok(Value::Number(Complex64::from(_lcm(first as i64, second as i64) as f64))));

#[rustfmt::skip]
pub fn get_constants() -> VariableMap<'static> {
    VariableMap::from([
        (String::from("i"), Variable::as_constant(Value::Number(Complex64::I))),
        (String::from("e"), Variable::as_constant(Value::Number(Complex64::from(E)))),
        (String::from("pi"), Variable::as_constant(Value::Number(Complex64::from(PI)))),
        (String::from("π"), Variable::as_constant(Value::Number(Complex64::from(PI)))),
        (String::from("phi"), Variable::as_constant(Value::Number(Complex64::from(PHI)))),
        (String::from("ϕ"), Variable::as_constant(Value::Number(Complex64::from(PHI)))),
        (String::from("tau"), Variable::as_constant(Value::Number(Complex64::from(TAU)))),
        (String::from("c"), Variable::as_constant(Value::Number(Complex64::from(C)))),
        (String::from("G"), Variable::as_constant(Value::Number(Complex64::from(G)))),
        (String::from("sin"), Variable::as_constant(Value::from_function(sin))),
        (String::from("cos"), Variable::as_constant(Value::from_function(cos))),
        (String::from("tan"), Variable::as_constant(Value::from_function(tan))),
        (String::from("asin"), Variable::as_constant(Value::from_function(asin))),
        (String::from("acos"), Variable::as_constant(Value::from_function(acos))),
        (String::from("atan"), Variable::as_constant(Value::from_function(atan))),
        (String::from("sinh"), Variable::as_constant(Value::from_function(sinh))),
        (String::from("cosh"), Variable::as_constant(Value::from_function(cosh))),
        (String::from("tanh"), Variable::as_constant(Value::from_function(tanh))),
        (String::from("asinh"), Variable::as_constant(Value::from_function(asinh))),
        (String::from("acosh"), Variable::as_constant(Value::from_function(acosh))),
        (String::from("atanh"), Variable::as_constant(Value::from_function(atanh))),
        (String::from("re"), Variable::as_constant(Value::from_function(re))),
        (String::from("im"), Variable::as_constant(Value::from_function(im))),
        (String::from("arg"), Variable::as_constant(Value::from_function(arg))),
        (String::from("conj"), Variable::as_constant(Value::from_function(conj))),
        (String::from("abs"), Variable::as_constant(Value::from_function(abs))),
        (String::from("ceil"), Variable::as_constant(Value::from_function(ceil))),
        (String::from("floor"), Variable::as_constant(Value::from_function(floor))),
        (String::from("log"), Variable::as_constant(Value::from_function(log))),
        (String::from("log2"), Variable::as_constant(Value::from_function(log2))),
        (String::from("log10"), Variable::as_constant(Value::from_function(log10))),
        (String::from("ln"), Variable::as_constant(Value::from_function(ln))),
        (String::from("sqrt"), Variable::as_constant(Value::from_function(sqrt))),
        (String::from("gcd"), Variable::as_constant(Value::from_function(gcd))),
        (String::from("lcm"), Variable::as_constant(Value::from_function(lcm))),
        (String::from("identity"), Variable::as_constant(Value::from_function(identity))),
        (String::from("transpose"), Variable::as_constant(Value::from_function(transpose))),
        (String::from("determinant"), Variable::as_constant(Value::from_function(determinant))),
        (String::from("inverse"), Variable::as_constant(Value::from_function(inverse)))
    ])
}
