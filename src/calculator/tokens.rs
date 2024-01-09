use phf::phf_map;
use std::fmt;

// The types that the calculator uses for integer and floating point operations
pub type FloatType = f64;
pub type IntType = i128;

// All infix binary operators (require an operand and after the operator e.g 1 + 2)
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum BinaryOp {
    ADD ,
    SUB,
    MUL,
    DIV,
    MOD,
    EXP,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::ADD => write!(f, "+"),
            BinaryOp::SUB => write!(f, "-"),
            BinaryOp::MUL => write!(f, "*"),
            BinaryOp::DIV => write!(f, "/"),
            BinaryOp::MOD => write!(f, "%"),
            BinaryOp::EXP => write!(f, "^"),
        }
    }
}

/*
All unary operators (only supporting prefix operators)
Functions are currently treated as unary operators
*/
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum UnaryOp {
    NEGATE,
    ABS,
    SIN,
    COS,
    TAN,
    ARCSIN,
    ARCCOS,
    ARCTAN,
    COSEC,
    SEC,
    COT,
    SINH,
    COSH,
    TANH,
    COSECH,
    SECH,
    COTH,
    ARSINH,
    ARCOSH,
    ARTANH,
    RAD,
    DEG
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::NEGATE => write!(f, "-"),
            UnaryOp::ABS => write!(f, "abs"),
            UnaryOp::SIN => write!(f, "sin"),
            UnaryOp::COS => write!(f, "cos"),
            UnaryOp::TAN => write!(f, "tan"),
            UnaryOp::ARCSIN => write!(f, "asin"),
            UnaryOp::ARCCOS => write!(f, "acos"),
            UnaryOp::ARCTAN => write!(f, "atan"),
            UnaryOp::COSEC => write!(f, "cosec"),
            UnaryOp::SEC => write!(f, "sec"),
            UnaryOp::COT => write!(f, "cot"),
            UnaryOp::SINH => write!(f, "sinh"),
            UnaryOp::COSH => write!(f, "cosh"),
            UnaryOp::TANH => write!(f, "tanh"),
            UnaryOp::COSECH => write!(f, "cosech"),
            UnaryOp::SECH => write!(f, "sech"),
            UnaryOp::COTH => write!(f, "coth"),
            UnaryOp::ARSINH => write!(f, "arsinh"),
            UnaryOp::ARCOSH => write!(f, "arcosh"),
            UnaryOp::ARTANH => write!(f, "artanh"),
            UnaryOp::RAD => write!(f, "rad"),
            UnaryOp::DEG => write!(f, "deg")
        }
    }
}

// Map of keywords to their associated token
pub static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "abs" => Token::UnaryOp(UnaryOp::ABS),
    "sin" => Token::UnaryOp(UnaryOp::SIN),
    "cos" => Token::UnaryOp(UnaryOp::COS),
    "tan" => Token::UnaryOp(UnaryOp::TAN),
    "asin" => Token::UnaryOp(UnaryOp::ARCSIN),
    "acos" => Token::UnaryOp(UnaryOp::ARCCOS),
    "atan" => Token::UnaryOp(UnaryOp::ARCTAN),
    "cosec" => Token::UnaryOp(UnaryOp::COSEC),
    "sec" => Token::UnaryOp(UnaryOp::SEC),
    "cot" => Token::UnaryOp(UnaryOp::COT),
    "sinh" => Token::UnaryOp(UnaryOp::SINH),
    "cosh" => Token::UnaryOp(UnaryOp::COSH),
    "tanh" => Token::UnaryOp(UnaryOp::TANH),
    "cosech" => Token::UnaryOp(UnaryOp::COSECH),
    "sech" => Token::UnaryOp(UnaryOp::SECH),
    "coth" => Token::UnaryOp(UnaryOp::COTH),
    "asinh" => Token::UnaryOp(UnaryOp::ARSINH),
    "acosh" => Token::UnaryOp(UnaryOp::ARCOSH),
    "atanh" => Token::UnaryOp(UnaryOp::ARTANH),
    "rad" => Token::UnaryOp(UnaryOp::RAD),
    "deg" => Token::UnaryOp(UnaryOp::DEG),
    "e" => Token::Constant(Number::Float(std::f64::consts::E)),
    "pi" => Token::Constant(Number::Float(std::f64::consts::PI)),
    "tau" => Token::Constant(Number::Float(std::f64::consts::TAU))
};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Parenthesis {
    OPEN,
    CLOSED
}

// Numbers can be either integers or floats in the calculator, operations will be different depending on this
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Number {
    Integer(IntType),
    Float(FloatType),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Float(val) => write!(f, "{}", val.to_string()),
            Number::Integer(val) => write!(f, "{}", val.to_string())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Associativity {
    LEFT,
    RIGHT
}

/*
All Tokens:
* Number - either an IntType or a FloatType
* Constant - A number, but has implicit multiplcation e.g 2pi instead of 2 * pi
* BinaryOp - Infix binary operation e.g 1 + 2
* UnaryOp - Prefix unary operation e.g -5 (Functions are also treated as unary operations)
* Parenthesis - ()
*/
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Token {
    Number(Number),
    Constant(Number),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Parenthesis(Parenthesis)
}

// Get the precedence for a binary opeartor (Higher is higher precedence)
pub fn get_binary_operator_precedence(op: BinaryOp) -> IntType {
    return match op {
        BinaryOp::ADD | BinaryOp::SUB => 0,
        BinaryOp::MUL | BinaryOp::DIV | BinaryOp::MOD => 1,
        BinaryOp::EXP  => 2,
    }
}

// Get the associativity for a binary operator
pub fn get_binary_operator_associativity(op: BinaryOp) -> Associativity {
    return match op {
        BinaryOp::EXP => Associativity::RIGHT,
        _ => Associativity::LEFT 
    }
}