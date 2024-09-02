use crate::value::Value;

#[derive(Debug)]
pub struct Variable {
    pub constant: bool,
    pub value: Value,
}
