use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Variable<'a> {
    pub constant: bool,
    pub value: Value<'a>,
}

impl Variable<'_> {
    pub fn new(constant: bool, value: Value) -> Variable {
        Variable {
            constant: constant,
            value: value
        }
    }
    pub fn as_constant(value: Value) -> Variable {
        Variable::new(true, value)
    }
    
    pub fn as_variable(value: Value) -> Variable {
        Variable::new(false, value)
    }
}