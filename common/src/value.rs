use std::{collections::HashMap, str::FromStr};
use std::fmt;

use num_complex::Complex64;
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};

use crate::{expr::complex_to_string, function::Function};

#[derive(Debug)]
pub enum ValueConstraint {
    Function,
    Number,
    Real,
    Natural,
    Integer,
}

impl FromStr for ValueConstraint {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "function" => Ok(ValueConstraint::Function),    
            "number" => Ok(ValueConstraint::Number),
            "real" => Ok(ValueConstraint::Real),
            "natural" => Ok(ValueConstraint::Natural),
            "integer" => Ok(ValueConstraint::Integer),
            _ => Err(())
        }
    }
}

impl fmt::Display for ValueConstraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueConstraint::Function => write!(f, "function"),
            ValueConstraint::Number => write!(f, "number"),
            ValueConstraint::Real => write!(f, "real"),
            ValueConstraint::Natural => write!(f, "natural"),
            ValueConstraint::Integer => write!(f, "integer")
        }
    }
}


impl ToTokens for ValueConstraint {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let token_str = match self {
            ValueConstraint::Function => quote! { ValueConstraint::Function },
            ValueConstraint::Number => quote! { ValueConstraint::Number },
            ValueConstraint::Real => quote! { ValueConstraint::Real },
            ValueConstraint::Natural => quote! { ValueConstraint::Natural },
            ValueConstraint::Integer => quote! { ValueConstraint::Integer }
        };
        tokens.extend(token_str);
    }
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Function(Function<'a>),
    Number(Complex64),
}

pub type ValueMap<'a> = HashMap<String, Value<'a>>;

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Function(func) => write!(f, "{}", func),
            Value::Number(num) => write!(f, "{}", complex_to_string(&num))
        }
    }
}

impl Value<'_> {
    pub fn fits_value_constraint(&self, constraint: ValueConstraint) -> bool {
        match constraint {
            ValueConstraint::Function => match self {
                Value::Function(_) => true,
                _ => false,
            },
            ValueConstraint::Number => match self {
                Value::Number(_) => true,
                _ => false,
            },
            ValueConstraint::Real => match self {
                Value::Number(num) => num.im == 0.0,
                _ => false,
            },
            ValueConstraint::Natural => match self {
                Value::Number(num) => num.im == 0.0 && num.re.fract() == 0.0 && num.re >= 0.0,
                _ => false
            }
            ValueConstraint::Integer => match self {
                Value::Number(num) => num.im == 0.0 && num.re.fract() == 0.0,
                _ => false
            }
        }
    }
}
