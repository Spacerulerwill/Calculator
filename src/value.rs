use num_complex::Complex64;

use crate::function::Function;

#[derive(Debug, Clone)]
pub enum Value {
    Function(Function),
    Number(Complex64),
}
