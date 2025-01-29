pub mod value;

use std::collections::HashMap;
use value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Variable<'a> {
    pub value: Value<'a>,
    pub constant: bool,
}

impl<'a> Variable<'a> {
    pub fn new(value: Value<'a>, constant: bool) -> Self {
        Variable { value, constant }
    }

    pub fn as_variable(value: Value<'a>) -> Self {
        Variable {
            value,
            constant: false,
        }
    }

    pub fn as_constant(value: Value<'a>) -> Self {
        Variable {
            value,
            constant: true,
        }
    }
}

pub type VariableMap<'a> = HashMap<String, Variable<'a>>;

#[cfg(test)]
mod tests {
    use num::Zero;
    use num_complex::Complex64;

    use super::{value::Value, Variable};

    #[test]
    fn test_variable_new() {
        let value = Value::Number(Complex64::zero());
        let constant = false;
        let variable = Variable::new(value.clone(), constant);
        assert_eq!(variable, Variable { value, constant });
    }

    #[test]
    fn test_variable_as_variable() {
        let value = Value::Number(Complex64::zero());
        let variable = Variable::as_variable(value.clone());
        assert_eq!(
            variable,
            Variable {
                value,
                constant: false
            }
        );
    }

    #[test]
    fn test_variable_as_constant() {
        let value = Value::Number(Complex64::zero());
        let variable = Variable::as_constant(value.clone());
        assert_eq!(
            variable,
            Variable {
                value,
                constant: true
            }
        );
    }
}
