use common::{
    expr::Expr,
    function::{Function, UserDefinedFunction, UserDefinedFunctionArgType},
    num::integer::{gcd as _gcd, lcm as _lcm},
    num_complex::Complex64,
    tokenizer::{Token, TokenKind},
    value::Value,
    variable::Variable,
};
use proc_macros::define_calculator_builtin_function;
use std::{
    collections::HashMap,
    f64::consts::{E, PI, TAU},
};

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
pub fn get_starting_variables() -> HashMap<String, Variable<'static>> {
    HashMap::from([
        (String::from("i"), Variable::as_constant(Value::Number(Complex64::I))),
        (String::from("e"), Variable::as_constant(Value::Number(Complex64::from(E)))),
        (String::from("pi"), Variable::as_constant(Value::Number(Complex64::from(PI)))),
        (String::from("π"), Variable::as_constant(Value::Number(Complex64::from(PI)))),
        (String::from("phi"), Variable::as_constant(Value::Number(Complex64::from(PHI)))),
        (String::from("ϕ"), Variable::as_constant(Value::Number(Complex64::from(PHI)))),
        (String::from("tau"), Variable::as_constant(Value::Number(Complex64::from(TAU)))),
        (String::from("c"), Variable::as_constant(Value::Number(Complex64::from(C)))),
        (String::from("g"), Variable::as_constant(Value::Number(Complex64::from(G)))),
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
        (String::from("re"), Variable::as_constant(re)),
        (String::from("im"), Variable::as_constant(im)),
        (String::from("arg"), Variable::as_constant(arg)),
        (String::from("conj"), Variable::as_constant(conj)),
        (String::from("abs"), Variable::as_constant(abs)),
        (String::from("ceil"), Variable::as_constant(ceil)),
        (String::from("floor"), Variable::as_constant(floor)),
        (String::from("log"), Variable::as_constant(log)),
        (String::from("log2"), Variable::as_constant(log2)),
        (String::from("log10"), Variable::as_constant(log10)),
        (String::from("ln"), Variable::as_constant(ln)),
        (String::from("sqrt"), Variable::as_constant(sqrt)),
        (String::from("gcd"), Variable::as_constant(gcd)),
        (String::from("lcm"), Variable::as_constant(lcm)),
        (String::from("fib"), Variable::as_variable(Value::Function(Function::UserDefinedFunction(UserDefinedFunction {
            name: "fib",
            signatures: vec![
                (
                    vec![UserDefinedFunctionArgType::Number(Complex64::from(0.0))],
                    Expr::Number { number: Complex64::from(0.0) }
                ),
                (
                    vec![UserDefinedFunctionArgType::Number(Complex64::from(1.0))],
                    Expr::Number { number: Complex64::from(1.0) }
                ),
                (
                    vec![UserDefinedFunctionArgType::Identifier(String::from("x"))], 
                    Expr::Binary { 
                        left: Box::new(Expr::Call { 
                            callee: Box::new(Expr::Identifier { name: Token { kind: TokenKind::Identifier(String::from("fib")), col: 0 } }), 
                            paren: Token { kind: TokenKind::LeftParen, col: 0 }, 
                            arguments: vec![
                                Expr::Binary { 
                                    left: Box::new(Expr::Identifier { name: Token { kind: TokenKind::Identifier(String::from("x")), col: 0 } }), 
                                    operator: Token { kind: TokenKind::Minus, col: 0 }, 
                                    right: Box::new(Expr::Number { number: Complex64::from(1.0) }) 
                                }
                            ]
                        }), 
                        operator: Token { kind: TokenKind::Plus, col: 0}, 
                        right: Box::new(Expr::Call { 
                            callee: Box::new(Expr::Identifier { name: Token { kind: TokenKind::Identifier(String::from("fib")), col: 0 } }), 
                            paren: Token { kind: TokenKind::LeftParen, col: 0 }, 
                            arguments: vec![
                                Expr::Binary { 
                                    left: Box::new(Expr::Identifier { name: Token { kind: TokenKind::Identifier(String::from("x")), col: 0 } }), 
                                    operator: Token { kind: TokenKind::Minus, col: 0 }, 
                                    right: Box::new(Expr::Number { number: Complex64::from(2.0) }) 
                                }
                            ]
                        }),
                    }
                )],
        }))))
    ])
}
