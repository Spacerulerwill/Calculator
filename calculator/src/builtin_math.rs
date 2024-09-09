use std::{
    collections::HashMap,
    f64::consts::{E, PI, TAU},
};

use common::{num_complex::{Complex64, ComplexFloat}, value::Value, variable::Variable};

use proc_macros::define_calculator_builtin_function;

define_calculator_builtin_function!(sin, (val: complex), Ok(Value::Number(val.sin())));
define_calculator_builtin_function!(cos, (val: complex), Ok(Value::Number(val.cos())));
define_calculator_builtin_function!(tan, (val: complex), Ok(Value::Number(val.tan())));
define_calculator_builtin_function!(asin, (val: complex), Ok(Value::Number(val.asin())));
define_calculator_builtin_function!(acos, (val: complex), Ok(Value::Number(val.acos())));
define_calculator_builtin_function!(atan, (val: complex), Ok(Value::Number(val.atan())));
define_calculator_builtin_function!(sinh, (val: complex), Ok(Value::Number(val.sinh())));
define_calculator_builtin_function!(cosh, (val: complex), Ok(Value::Number(val.cosh())));
define_calculator_builtin_function!(tanh, (val: complex), Ok(Value::Number(val.tanh())));
define_calculator_builtin_function!(asinh, (val: complex), Ok(Value::Number(val.asinh())));
define_calculator_builtin_function!(acosh, (val: complex), Ok(Value::Number(val.acosh())));
define_calculator_builtin_function!(atanh, (val: complex), Ok(Value::Number(val.atanh())));
define_calculator_builtin_function!(log, (base: real, val: complex), Ok(Value::Number(val.log(base))));
define_calculator_builtin_function!(log2, (val: complex), Ok(Value::Number(val.log2())));
define_calculator_builtin_function!(log10, (val: complex), Ok(Value::Number(val.log10())));
define_calculator_builtin_function!(ln, (val: complex), Ok(Value::Number(val.ln())));
define_calculator_builtin_function!(sqrt, (val: complex), Ok(Value::Number(val.sqrt())));
define_calculator_builtin_function!(re, (val: complex), Ok(Value::Number(val.re().into())));
define_calculator_builtin_function!(im, (val: complex), Ok(Value::Number(val.im().into())));
define_calculator_builtin_function!(arg, (val: complex), Ok(Value::Number(val.arg().into())));
define_calculator_builtin_function!(conj, (val: complex), Ok(Value::Number(val.conj())));


#[rustfmt::skip]
pub fn get_starting_variables() -> HashMap<String, Variable<'static>> {
    HashMap::from([
        (String::from("i"), Variable::as_constant(Value::Number(Complex64::I))),
        (String::from("e"), Variable::as_constant(Value::Number(Complex64::from(E)))),
        (String::from("pi"), Variable::as_constant(Value::Number(Complex64::from(PI)))),
        (String::from("π"), Variable::as_constant(Value::Number(Complex64::from(PI)))),
        (String::from("tau"), Variable::as_constant(Value::Number(Complex64::from(TAU)))),
        (String::from("sin"), Variable::as_constant(sin)),
        (String::from("cos"), Variable::as_constant(cos)),
        (String::from("tan"), Variable::as_constant(tan)),
        (String::from("asin"), Variable::as_constant(asin)),
        (String::from("acos"), Variable::as_constant(acos)),
        (String::from("atan"), Variable::as_constant(atan)),
        (String::from("sinh"), Variable::as_constant(sinh)),
        (String::from("cosh"), Variable::as_constant(cosh)),
        (String::from("tanh"), Variable::as_constant(tanh)),
        (String::from("asinh"), Variable::as_constant(asinh)),
        (String::from("acosh"), Variable::as_constant(acosh)),
        (String::from("atanh"), Variable::as_constant(atanh)),
        (String::from("log"), Variable::as_constant(log)),
        (String::from("log2"), Variable::as_constant(log2)),
        (String::from("log10"), Variable::as_constant(log10)),
        (String::from("ln"), Variable::as_constant(ln)),
        (String::from("sqrt"), Variable::as_constant(sqrt)),
        (String::from("re"), Variable::as_constant(re)),
        (String::from("im"), Variable::as_constant(im)),
        (String::from("arg"), Variable::as_constant(arg)),
        (String::from("conj"), Variable::as_constant(conj)),


    ])
}
