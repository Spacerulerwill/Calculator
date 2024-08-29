use rug::Complex;

#[derive(Debug)]
pub struct Variable {
    pub constant: bool,
    pub value: Complex
}