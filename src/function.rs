use num_complex::Complex64;

use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Function {
    pub function: fn(Vec<Complex64>) -> Complex64,
    pub arity: usize,
}

fn internal_builtin_sin(args: Vec<Complex64>) -> Complex64 {
    let input = args.get(0).unwrap();
    input.sin()
}

pub const BUILTIN_SIN: Value = Value::Function(Function {
    function: internal_builtin_sin,
    arity: 1,
});
