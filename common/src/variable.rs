use std::collections::HashMap;

use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Variable<'a> {
    pub value: Value<'a>,
    pub constant: bool
}

impl<'a> Variable<'a> {
    pub fn new(value: Value<'a>, constant: bool) -> Self {
        Variable { value: value, constant: constant }
    }

    pub fn as_variable(value: Value<'a>) -> Self {
        Variable { value: value, constant: false }
    }

    pub fn as_constant(value: Value<'a>) -> Self {
        Variable { value: value, constant: true }
    }
}

pub type VariableMap<'a> = HashMap<String, Variable<'a>>;