use common::{
    num::integer::{gcd as _gcd, lcm as _lcm},
    num_complex::Complex64,
    value::{Value, ValueMap},
};
use proc_macros::define_calculator_builtin_function;
use std::{cell::RefCell, f64::consts::{E, PI, TAU}, rc::Rc};

const C: f64 = 299792458_f64;
const G: f64 = 9.80665_f64;
const PHI: f64 = 1.618033988749894848204586834365638118_f64;

// trigonometry
define_calculator_builtin_function!(sin, (val: number), Ok(Value::Number(val.sin())));
define_calculator_builtin_function!(cos, (val: number), Ok(Value::Number(val.cos())));
define_calculator_builtin_function!(tan, (val: number), Ok(Value::Number(val.tan())));
define_calculator_builtin_function!(asin, (val: number), Ok(Value::Number(val.asin())));
define_calculator_builtin_function!(acos, (val: number), Ok(Value::Number(val.acos())));
define_calculator_builtin_function!(atan, (val: number), Ok(Value::Number(val.atan())));
define_calculator_builtin_function!(sinh, (val: number), Ok(Value::Number(val.sinh())));
define_calculator_builtin_function!(cosh, (val: number), Ok(Value::Number(val.cosh())));
define_calculator_builtin_function!(tanh, (val: number), Ok(Value::Number(val.tanh())));
define_calculator_builtin_function!(asinh, (val: number), Ok(Value::Number(val.asinh())));
define_calculator_builtin_function!(acosh, (val: number), Ok(Value::Number(val.acosh())));
define_calculator_builtin_function!(atanh, (val: number), Ok(Value::Number(val.atanh())));
// complex numbers
define_calculator_builtin_function!(re, (val: number), Ok(Value::Number(val.re.into())));
define_calculator_builtin_function!(im, (val: number), Ok(Value::Number(val.im.into())));
define_calculator_builtin_function!(arg, (val: number), Ok(Value::Number(val.arg().into())));
define_calculator_builtin_function!(conj, (val: number), Ok(Value::Number(val.conj())));
// other
define_calculator_builtin_function!(abs, (val: number), Ok(Value::Number(Complex64::from(val.abs()))));
define_calculator_builtin_function!(ceil, (val: real), Ok(Value::Number(Complex64::from(val.ceil()))));
define_calculator_builtin_function!(floor, (val: real), Ok(Value::Number(Complex64::from(val.floor()))));
define_calculator_builtin_function!(log, (base: real, val: number), Ok(Value::Number(val.log(base))));
define_calculator_builtin_function!(log2, (val: number), Ok(Value::Number(val.log2())));
define_calculator_builtin_function!(log10, (val: number), Ok(Value::Number(val.log10())));
define_calculator_builtin_function!(ln, (val: number), Ok(Value::Number(val.ln())));
define_calculator_builtin_function!(sqrt, (val: number), Ok(Value::Number(val.sqrt())));
define_calculator_builtin_function!(gcd, (first: integer, second: integer), Ok(Value::Number(Complex64::from(_gcd(first as i64, second as i64) as f64))));
define_calculator_builtin_function!(lcm, (first: integer, second: integer), Ok(Value::Number(Complex64::from(_lcm(first as i64, second as i64) as f64))));

#[rustfmt::skip]
pub fn get_constants() -> ValueMap<'static>{
    ValueMap::from([
        (String::from("i"), Value::Number(Complex64::I)),
        (String::from("e"), Value::Number(Complex64::from(E))),
        (String::from("pi"), Value::Number(Complex64::from(PI))),
        (String::from("π"), Value::Number(Complex64::from(PI))),
        (String::from("phi"), Value::Number(Complex64::from(PHI))),
        (String::from("ϕ"), Value::Number(Complex64::from(PHI))),
        (String::from("tau"), Value::Number(Complex64::from(TAU))),
        (String::from("c"), Value::Number(Complex64::from(C))),
        (String::from("g"), Value::Number(Complex64::from(G))),
        (String::from("sin"), Value::Function(Rc::new(RefCell::new(sin)))),
        (String::from("cos"), Value::Function(Rc::new(RefCell::new(cos)))),
        (String::from("tan"), Value::Function(Rc::new(RefCell::new(tan)))),
        (String::from("asin"), Value::Function(Rc::new(RefCell::new(asin)))),
        (String::from("acos"), Value::Function(Rc::new(RefCell::new(acos)))),
        (String::from("atan"), Value::Function(Rc::new(RefCell::new(atan)))),
        (String::from("sinh"), Value::Function(Rc::new(RefCell::new(sinh)))),
        (String::from("cosh"), Value::Function(Rc::new(RefCell::new(cosh)))),
        (String::from("tanh"), Value::Function(Rc::new(RefCell::new(tanh)))),
        (String::from("asinh"), Value::Function(Rc::new(RefCell::new(asinh)))),
        (String::from("acosh"), Value::Function(Rc::new(RefCell::new(acosh)))),
        (String::from("atanh"), Value::Function(Rc::new(RefCell::new(atanh)))),
        (String::from("re"), Value::Function(Rc::new(RefCell::new(re)))),
        (String::from("im"), Value::Function(Rc::new(RefCell::new(im)))),
        (String::from("arg"), Value::Function(Rc::new(RefCell::new(arg)))),
        (String::from("conj"), Value::Function(Rc::new(RefCell::new(conj)))),
        (String::from("abs"), Value::Function(Rc::new(RefCell::new(abs)))),
        (String::from("ceil"), Value::Function(Rc::new(RefCell::new(ceil)))),
        (String::from("floor"), Value::Function(Rc::new(RefCell::new(floor)))),
        (String::from("log"), Value::Function(Rc::new(RefCell::new(log)))),
        (String::from("log2"), Value::Function(Rc::new(RefCell::new(log2)))),
        (String::from("log10"), Value::Function(Rc::new(RefCell::new(log10)))),
        (String::from("ln"), Value::Function(Rc::new(RefCell::new(ln)))),
        (String::from("sqrt"), Value::Function(Rc::new(RefCell::new(sqrt)))),
        (String::from("gcd"), Value::Function(Rc::new(RefCell::new(gcd)))),
        (String::from("lcm"), Value::Function(Rc::new(RefCell::new(lcm)))),
    ])
}
