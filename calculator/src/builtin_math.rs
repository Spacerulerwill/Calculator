use common::value::Value;

use proc_macros::define_calculator_builtin_function;

define_calculator_builtin_function!(sin, (val: complex), Ok(Value::Number(val.sin())));

define_calculator_builtin_function!(cos, (val: complex), Ok(Value::Number(val.cos())));

define_calculator_builtin_function!(tan, (val: complex), Ok(Value::Number(val.tan())));

define_calculator_builtin_function!(log, (base: complex, val: real), Ok(Value::Number(base.log(val))));
