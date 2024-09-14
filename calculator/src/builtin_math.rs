use common::{
    expr::Expr,
    function::{Function, UserDefinedFunction, UserDefinedFunctionArgType},
    num::integer::{gcd as _gcd, lcm as _lcm},
    num_complex::Complex64,
    tokenizer::{Token, TokenKind},
    value::{Value, ValueMap},
};
use proc_macros::define_calculator_builtin_function;
use std::f64::consts::{E, PI, TAU};

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
        (String::from("sin"), sin),
        (String::from("cos"), cos),
        (String::from("tan"), tan),
        (String::from("asin"), asin),
        (String::from("acos"), acos),
        (String::from("atan"), atan),
        (String::from("sinh"), sinh),
        (String::from("cosh"), cosh),
        (String::from("tanh"), tanh),
        (String::from("asinh"), asinh),
        (String::from("acosh"), acosh),
        (String::from("atanh"), atanh),
        (String::from("re"), re),
        (String::from("im"), im),
        (String::from("arg"), arg),
        (String::from("conj"), conj),
        (String::from("abs"), abs),
        (String::from("ceil"), ceil),
        (String::from("floor"), floor),
        (String::from("log"), log),
        (String::from("log2"), log2),
        (String::from("log10"), log10),
        (String::from("ln"), ln),
        (String::from("sqrt"), sqrt),
        (String::from("gcd"), gcd),
        (String::from("lcm"), lcm),
        (String::from("fib"), Value::Function(Function::UserDefinedFunction(UserDefinedFunction {
            name: String::from("fib"),
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
        })))
    ])
}
