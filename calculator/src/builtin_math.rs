use common::value::Value;
use proc_macros::define_calculator_builtin_function;

define_calculator_builtin_function!(sin, (val), { Value::Number(val.sin()) });

define_calculator_builtin_function!(cos, (val), { Value::Number(val.cos()) });

define_calculator_builtin_function!(tan, (val), { Value::Number(val.tan()) });