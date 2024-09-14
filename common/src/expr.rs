use std::{cell::RefCell, collections::HashMap, fmt::{self}, rc::Rc};

use crate::{
    function::{Function, UserDefinedFunction, UserDefinedFunctionArgType}, 
    num_complex::Complex64, tokenizer::{Token, TokenKind}, 
    value::{Value, ValueConstraint, ValueMap},
};

#[derive(Debug, Clone)]
pub enum GroupingKind {
    Grouping,
    Absolute,
    Ceil,
    Floor
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assign {
        name: Token,
        new_value: Box<Expr>,
    },
    FunctionAssign {
        name: Token,
        signature: Vec<Expr>,
        body: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        operand: Box<Expr>,
    },
    Grouping {
        paren: Token,
        kind: GroupingKind,
        expr: Box<Expr>,
    },
    Number {
        number: Complex64,
    },
    Identifier {
        name: Token,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum EvaluationError<'a> {
    DivisionByZero {
        operator: Token,
    },
    IncorrectFunctionArgumentCount {
        paren: Token,
        name: &'a str,
        received: usize,
        required: usize,
    },
    IncorrectFunctionArgumentSignature {
        paren: Token,
        name: String,
    },
    IncorrectFunctionArgumentType {
        function_name: String,
        function_col: usize,
        idx: usize,
        name: String,
        constraint: ValueConstraint,
    },
    UnsupportedBinaryOperator {
        operator: Token,
        constraint: ValueConstraint
    },
    UnsupportedUnaryOperator {
        operator: Token,
        constraint: ValueConstraint
    },
    GroupingValueConstraintNotMet {
        paren: Token,
        kind: GroupingKind,
        constraint: ValueConstraint,
    },
    InvalidCallable {
        paren: Token
    },
    ConstantAssignment {
        name: Token,
    },
    UnknownVariable {
        name: Token,
    }
}

impl<'a> Expr {
    pub fn evaluate(
        self,
        constants: &ValueMap<'a>,
        variables: &mut ValueMap<'a>
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match self {
            Expr::Assign { name, new_value } => Self::evaluate_assign(name, new_value, constants, variables),
            Expr::FunctionAssign { name, signature, body } => Self::evaluate_function_assign(name, signature, body, constants, variables),
            Expr::Binary {
                left,
                operator,
                right,
            } => Self::evaluate_binary(left, operator, right, constants, variables),
            Expr::Unary { operator, operand } => Self::evaluate_unary(operand, operator, constants, variables),
            Expr::Grouping { paren, kind, expr } => Self::evaluate_grouping(paren, kind, expr, constants, variables),
            Expr::Number { number } => Ok(Value::Number(number)),
            Expr::Identifier { name } => Self::evaluate_identifier(name, constants, variables),
            Expr::Call { callee, paren, arguments} => Self::evaluate_call(callee, paren, arguments, constants, variables),
        }
    }

    fn evaluate_assign(
        name: Token,
        new_value: Box<Expr>,
        constants: &ValueMap<'a>,
        variables: &mut ValueMap<'a>
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let new_value = new_value.evaluate(constants, variables)?;
        if constants.contains_key(&name.kind.get_lexeme()) {
            return Err(EvaluationError::ConstantAssignment { name: name })
        }
        variables.insert(name.kind.get_lexeme(), new_value.clone());
        Ok(new_value)
    }
    
    fn evaluate_function_assign(
        name: Token, 
        signature: Vec<Expr>,
        body: Box<Expr>,
        constants: &ValueMap<'a>,
        variables: &mut ValueMap<'a>
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        if constants.contains_key(&name.kind.get_lexeme()) {
            return Err(EvaluationError::ConstantAssignment { name: name })
        }
        // create signature
        let mut final_signature = Vec::new();
        for expr in signature {
            match expr {
                Expr::Number { number } => final_signature.push(UserDefinedFunctionArgType::Number(number)),
                Expr::Identifier { name } => final_signature.push(UserDefinedFunctionArgType::Identifier(name.kind.get_lexeme())),
                _ => panic!("Invalid signature")
            }
        }
        // If already a user-defined function check an equivalent signature does not exist and then add it
        if let Some(value) = variables.get(&name.kind.get_lexeme()).cloned() {
            if let Value::Function(func) = value.clone() {
                let mut func = func.borrow_mut(); 
                match &mut *func {
                    Function::UserDefinedFunction(ref mut func) => {
                        if func.signatures.iter().any(|sig| Self::are_signatures_equivalent(&sig.0, &final_signature)) {
                            panic!("Equivalent signature found!");
                        }
                        func.signatures.push((final_signature, *body));
                        return Ok(value)
                    },
                    _ => {}
                }
            }
        }

        // otherwise just completely replace as new user defined function
        let function = Value::Function(Rc::new(RefCell::new(Function::UserDefinedFunction(UserDefinedFunction{
            name: name.kind.get_lexeme(),
            signatures: vec![(final_signature, *body)]
        }))));
        variables.insert(name.kind.get_lexeme(), function.clone());
        Ok(function)
    }
    

    fn evaluate_binary(
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
        constants: &ValueMap<'a>,
        variables: &mut ValueMap<'a>
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let left = left.evaluate(constants, variables)?;
        let right = right.evaluate(constants, variables)?;
        match operator.kind {
            TokenKind::Plus => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    return Ok(Value::Number(left + right))
                }
                _ => {}
            },
            TokenKind::Minus => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    return Ok(Value::Number(left - right))
                }
                _ => {},
            },
            TokenKind::Star => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    return Ok(Value::Number(left * right))
                }
                _ => {},
            },
            TokenKind::Slash => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    if right.norm() == 0.0 {
                        return Err(EvaluationError::DivisionByZero { operator: operator });
                    }
                    return Ok(Value::Number(left / right))
                }
                _ => {},
            },
            TokenKind::Caret => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    return Ok(Value::Number(left.powc(*right)))
                }
                _ => {},
            },
            TokenKind::Percent => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => {
                    if right.norm() == 0.0 {
                        return Err(EvaluationError::DivisionByZero { operator: operator });
                    }
                    return Ok(Value::Number(left % right))
                }
                _ => {},
            },
            kind => panic!("Invalid token kind for binary operation: {:?}", kind),
        }
        // None were matched, unsupported
        Err(EvaluationError::UnsupportedBinaryOperator {
            operator,
            constraint: ValueConstraint::Number
        })
    }

    fn evaluate_unary(
        operand: Box<Expr>,
        operator: Token,
        constants: &ValueMap<'a>,
        variables: &mut ValueMap<'a>
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let operand = operand.evaluate(constants, variables)?;
        match operator.kind {
            TokenKind::Minus => match &operand {
                Value::Number(right) => {
                    return Ok(Value::Number(right * Complex64::new(-1.0, 0.0)));
                }
                _ => return Err(EvaluationError::UnsupportedUnaryOperator { operator: operator, constraint: ValueConstraint::Number  })
            },
            TokenKind::Sqrt => match &operand {
                Value::Number(right) => {
                    return Ok(Value::Number(right.sqrt()));
                }
                _ => return Err(EvaluationError::UnsupportedUnaryOperator { operator: operator, constraint: ValueConstraint::Number  })
            }
            TokenKind::Bang => match &operand {
                Value::Number(right) if operand.fits_value_constraint(ValueConstraint::Natural) => {
                    return Ok(Value::Number(factorial(right.re as u64).into()))
                }
                _ => return Err(EvaluationError::UnsupportedUnaryOperator { operator: operator, constraint: ValueConstraint::Natural  })
            },
            kind => panic!("Invalid token kind for unary operation: {:?}", kind),
        }
    }

    fn evaluate_grouping(
        paren: Token,
        kind: GroupingKind,
        expr: Box<Expr>,
        constants: &ValueMap<'a>,
        variables: &mut ValueMap<'a>
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match kind {
            GroupingKind::Grouping => expr.evaluate(constants, variables),
            GroupingKind::Absolute => {
                let result = expr.evaluate(constants, variables)?;
                match result {
                    Value::Number(result) => Ok(Value::Number(result.norm().into())),
                    _ => Err(EvaluationError::GroupingValueConstraintNotMet { paren: paren, kind: kind, constraint: ValueConstraint::Number }),
                }
            },
            GroupingKind::Ceil => {
                let result = expr.evaluate(constants, variables)?;
                match result {
                    Value::Number(num) if result.fits_value_constraint(ValueConstraint::Real) => {
                        return Ok(Value::Number(num.re.ceil().into()));
                    }
                    _ => Err(EvaluationError::GroupingValueConstraintNotMet { paren: paren, kind: kind, constraint: ValueConstraint::Real }),
                }
            },
            GroupingKind::Floor => {
                let result = expr.evaluate(constants, variables)?;
                match result {
                    Value::Number(num) if result.fits_value_constraint(ValueConstraint::Real) => {
                        return Ok(Value::Number(num.re.floor().into()));
                    }
                    _ => Err(EvaluationError::GroupingValueConstraintNotMet { paren: paren, kind: kind, constraint: ValueConstraint::Real }),
                }
            },
        }
    }

    fn evaluate_identifier(
        name: Token,
        constants: &ValueMap<'a>,
        variables: &mut ValueMap<'a>        
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match variables.get(name.kind.get_lexeme().as_str()) {
            Some(value) => return Ok(value.clone()),
            None => {},
        }
        match constants.get(name.kind.get_lexeme().as_str()) {
            Some(value) => return Ok(value.clone()),
            None => {}
        }
        Err(EvaluationError::UnknownVariable { name: name })
    }

    fn matches_signature(
        signature: &Vec<UserDefinedFunctionArgType>,
        arguments: &Vec<Value>,
    ) -> bool {
        if signature.len() != arguments.len() {
            return false;
        }
    
        for (param, arg) in signature.iter().zip(arguments.iter()) {
            match param {
                UserDefinedFunctionArgType::Identifier(_) => continue, // Identifier matches any number
                UserDefinedFunctionArgType::Number(expected) => {
                    match arg {
                        Value::Function(_) => return false,
                        Value::Number(num) => if num != expected {
                            return false
                        }
                    };
                }
            }
        }
    
        true
    }

    fn are_signatures_equivalent(
        signature1: &Vec<UserDefinedFunctionArgType>,
        signature2: &Vec<UserDefinedFunctionArgType>,
    ) -> bool {
        if signature1.len() != signature2.len() {
            return false
        }
        for (arg_type_1, arg_type_2) in signature1.iter().zip(signature2.iter()) {
            if std::mem::discriminant(arg_type_1) != std::mem::discriminant(arg_type_2) {
                return false;
            }
            match (arg_type_1, arg_type_2) {
                (UserDefinedFunctionArgType::Number(one), UserDefinedFunctionArgType::Number(two)) => if one != two {
                    return false
                }
                _ => {}
            }
        }
        true
    }
    
    fn evaluate_call(
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
        constants: &ValueMap<'a>,
        variables: &mut ValueMap<'a>
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        let callee = callee.evaluate(constants, variables)?;
        match callee {
            Value::Function(function) => {
                let mut evaluated_arguments = Vec::with_capacity(arguments.len());
                for argument in arguments {
                    evaluated_arguments.push(argument.evaluate(constants, variables)?);
                }
                let function = function.borrow();
                match & *function {
                    Function::NativeFunction(func) => {
                        if func.arity != evaluated_arguments.len() {
                            return Err(EvaluationError::IncorrectFunctionArgumentCount {
                                paren: paren,
                                name: func.name,
                                received: evaluated_arguments.len(),
                                required: func.arity,
                            });
                        }
                        Ok((func.function)(paren.col, evaluated_arguments)?)
                    },
                    Function::UserDefinedFunction(func) => {
                        for (signature, expr) in func.signatures.iter() {
                            if Self::matches_signature(&signature, &evaluated_arguments) {
                                // Copy  
                                let mut inputs = variables.clone();
                                for (arg_type, val) in signature.into_iter().zip(evaluated_arguments.into_iter()) {
                                    if let UserDefinedFunctionArgType::Identifier(identifier) = arg_type {
                                        inputs.insert(identifier.clone(), val);
                                    }
                                }
                                return expr.clone().evaluate(constants, &mut inputs)
                            }
                        }
                        Err(EvaluationError::IncorrectFunctionArgumentSignature { paren: paren, name: func.name.clone() })
                    }
                }
            } 
            _ => return Err(EvaluationError::InvalidCallable { paren: paren })

        }
    } 
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Assign { name, new_value } => write!(f, "{}={}", name.kind.get_lexeme(), new_value),
            Expr::FunctionAssign { name, signature, body } => {
                let args: Vec<String> = signature.iter().map(|arg| format!("{}", arg)).collect();
                let args_str = args.join(", ");
                write!(f, "{}({}) = {}", &name.kind.get_lexeme(), args_str, body)
            }
            Expr::Binary { left, operator, right } => write!(f, "{left}{}{right}", &operator.kind.get_lexeme()),
            Expr::Unary { operator, operand } => match &operator.kind {
                TokenKind::Bang => write!(f, "{operand}{}", &operator.kind.get_lexeme()),
                TokenKind::Minus => write!(f, "{}{operand}", &operator.kind.get_lexeme()),
                kind => panic!("Invalid operator for unary expression {:?}", kind)
            }
            Expr::Grouping { paren: _, kind, expr } => match kind {
                GroupingKind::Grouping => write!(f, "({expr})"),
                GroupingKind::Absolute => write!(f, "|{expr}|"),
                GroupingKind::Ceil => write!(f, "⌈{expr}⌉"),
                GroupingKind::Floor => write!(f, "⌊{expr}⌋"),
            },
            Expr::Number { number } => write!(f, "{}", complex_to_string(number)),
            Expr::Identifier { name } => write!(f, "{}", name.kind.get_lexeme()),
            Expr::Call { callee, paren: _, arguments } => {
                let args: Vec<String> = arguments.iter().map(|arg| format!("{}", arg)).collect();
                let args_str = args.join(", ");
                write!(f, "{}({})", callee, args_str)
            },
        }
    }
}

pub fn complex_to_string(num: &Complex64) -> String {
    let has_real = num.re != 0.0;
    let has_imaginary = num.im != 0.0;

    if has_real && has_imaginary {
        if num.im == 1.0 {
            return format!("{} + i", num.re);
        } else if num.im == -1.0 {
            return format!("{} - i", num.re);
        } else {
            return format!("{} + {}i", num.re, num.im);
        }
    } else if has_real {
        return format!("{}", num.re);
    } else if num.im == 1.0 {
        return "i".to_string();
    } else if num.im == -1.0 {
        return "-i".to_string();
    } else if num.im == 0.0 {
        return String::from("0")
    } else {
        return format!("{}i", num.im);
    }
}

// Computes factorial of a non-negative integer
fn factorial(n: u64) -> f64 {
    (1..=n).map(|x| x as f64).product()
}
