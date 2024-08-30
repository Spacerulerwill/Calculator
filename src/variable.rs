use num_complex::Complex64;

#[derive(Debug)]
pub struct Variable {
    pub constant: bool,
    pub value: Complex64
}