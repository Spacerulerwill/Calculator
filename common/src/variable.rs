use crate::value::Value;

#[derive(Debug)]
pub struct Variable<'a> {
    pub constant: bool,
    pub value: Value<'a>,
}
