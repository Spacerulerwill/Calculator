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

pub fn get_starting_variables() -> HashMap<&'static str, Variable<'static>> {
    HashMap::from([
        (
            "i",
            Variable {
                constant: true,
                value: Value::Number(Complex64::i()),
            },
        ),
        (
            "e",
            Variable {
                constant: true,
                value: Value::Number(Complex64::from(E)),
            },
        ),
        (
            "pi",
            Variable {
                constant: true,
                value: Value::Number(Complex64::from(PI)),
            },
        ),
        (
            "tau",
            Variable {
                constant: true,
                value: Value::Number(Complex64::from(TAU)),
            },
        ),
        (
            "sin",
            Variable {
                constant: true,
                value: sin,
            },
        ),
        (
            "cos",
            Variable {
                constant: true,
                value: cos,
            },
        ),
        (
            "tan",
            Variable {
                constant: true,
                value: tan,
            },
        ),
        (
            "log",
            Variable {
                constant: true,
                value: log,
            },
        ),
        (
            "log2",
            Variable {
                constant: true,
                value: log2,
            },
        ),
        (
            "log10",
            Variable {
                constant: true,
                value: log10,
            },
        ),
        (
            "ln",
            Variable {
                constant: true,
                value: ln,
            },
        ),
    ])
}
