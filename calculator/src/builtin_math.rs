use std::{
    collections::HashMap,
    f64::consts::{E, PI, TAU},
};

use common::{num_complex::Complex64, value::Value, variable::Variable};

use proc_macros::define_calculator_builtin_function;

define_calculator_builtin_function!(sin, (val: complex), Ok(Value::Number(val.sin())));
define_calculator_builtin_function!(cos, (val: complex), Ok(Value::Number(val.cos())));
define_calculator_builtin_function!(tan, (val: complex), Ok(Value::Number(val.tan())));
define_calculator_builtin_function!(log, (base: real, val: complex), Ok(Value::Number(val.log(base))));
define_calculator_builtin_function!(log2, (val: complex), Ok(Value::Number(val.log2())));
define_calculator_builtin_function!(log10, (val: complex), Ok(Value::Number(val.log10())));
define_calculator_builtin_function!(ln, (val: complex), Ok(Value::Number(val.ln())));

#[rustfmt::skip]
pub fn get_starting_variables() -> HashMap<String, Variable<'static>> {
    HashMap::from([
        (String::from("i"), Variable::as_constant(Value::Number(Complex64::I))),
        (String::from("e"), Variable::as_constant(Value::Number(Complex64::from(E)))),
        (String::from("pi"), Variable::as_constant(Value::Number(Complex64::from(PI)))),
        (String::from("tau"), Variable::as_constant(Value::Number(Complex64::from(TAU)))),
        (String::from("sin"), Variable::as_constant(sin)),
        (String::from("cos"), Variable::as_constant(cos)),
        (String::from("tan"), Variable::as_constant(tan)),
        (String::from("log"), Variable::as_constant(log)),
        (String::from("log2"), Variable::as_constant(log2)),
        (String::from("log10"), Variable::as_constant(log10)),
        (String::from("ln"), Variable::as_constant(ln)),
    ])
}
