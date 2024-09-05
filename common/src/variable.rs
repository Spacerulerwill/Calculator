use crate::value::Value;

#[derive(Debug)]
pub struct Variable<'a> {
    pub constant: bool,
    pub value: Value<'a>,
}

impl Variable<'_> {
    pub fn as_constant(value: Value) -> Variable {
        Variable {
            constant: true,
            value: value
        }
    }
}