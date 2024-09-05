use std::{
    collections::HashMap,
    f64::consts::{E, PI, TAU},
};

use common::{num_complex::Complex64, value::Value, variable::Variable};

use proc_macros::define_calculator_builtin_function;

define_calculator_builtin_function!(SIN, (val: complex), Ok(Value::Number(val.sin())));
define_calculator_builtin_function!(COS, (val: complex), Ok(Value::Number(val.cos())));
define_calculator_builtin_function!(TAN, (val: complex), Ok(Value::Number(val.tan())));
define_calculator_builtin_function!(LOG, (base: real, val: complex), Ok(Value::Number(val.log(base))));
define_calculator_builtin_function!(LOG2, (val: complex), Ok(Value::Number(val.log2())));
define_calculator_builtin_function!(LOG10, (val: complex), Ok(Value::Number(val.log10())));
define_calculator_builtin_function!(LN, (val: complex), Ok(Value::Number(val.ln())));

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
                value: SIN,
            },
        ),
        (
            "cos",
            Variable {
                constant: true,
                value: COS,
            },
        ),
        (
            "tan",
            Variable {
                constant: true,
                value: TAN,
            },
        ),
        (
            "log",
            Variable {
                constant: true,
                value: LOG,
            },
        ),
        (
            "log2",
            Variable {
                constant: true,
                value: LOG2,
            },
        ),
        (
            "log10",
            Variable {
                constant: true,
                value: LOG10,
            },
        ),
        (
            "ln",
            Variable {
                constant: true,
                value: LN,
            },
        ),
    ])
}
