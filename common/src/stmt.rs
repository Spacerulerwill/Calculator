use std::{cell::RefCell, rc::Rc};

use crate::{
    expr::{EvaluationError, Expr},
    function::{Function, UserDefinedFunction, UserDefinedFunctionArgType},
    tokenizer::Token,
    value::Value,
    variable::{Variable, VariableMap},
};

#[derive(Debug)]
pub enum Statement {
    ExpressionStatement(Expr),
    SimplifyStatement(Expr),
    Assignment {
        identifier: Token,
        expr: Expr,
    },
    FunctionDeclaration {
        name: Token,
        signature: Vec<UserDefinedFunctionArgType>,
        expr: Expr,
    },
}

fn are_signatures_equivalent(
    signature1: &Vec<UserDefinedFunctionArgType>,
    signature2: &Vec<UserDefinedFunctionArgType>,
) -> bool {
    if signature1.len() != signature2.len() {
        return false;
    }
    for (arg_type_1, arg_type_2) in signature1.iter().zip(signature2.iter()) {
        if std::mem::discriminant(arg_type_1) != std::mem::discriminant(arg_type_2) {
            return false;
        }
        match (arg_type_1, arg_type_2) {
            (UserDefinedFunctionArgType::Number(one), UserDefinedFunctionArgType::Number(two)) => {
                if one != two {
                    return false;
                }
            }
            _ => {}
        }
    }
    true
}

impl Statement {
    pub fn interpret<'a>(self, variables: &mut VariableMap<'a>) -> Result<(), EvaluationError<'a>> {
        match self {
            Statement::ExpressionStatement(expr) => {
                match expr.evaluate(variables) {
                    Ok(result) => println!("{}", &result),
                    Err(err) => println!("{}", &err),
                };
                Ok(())
            }
            Statement::SimplifyStatement(expr) => {
                println!("{}", expr.simplify());
                Ok(())
            }
            Statement::Assignment { identifier, expr } => {
                if let Some(variable) = variables.get(&identifier.lexeme) {
                    if variable.constant {
                        return Err(EvaluationError::ConstantAssignment { name: identifier });
                    }
                }
                let new_value = expr.evaluate(variables)?;
                variables.insert(identifier.lexeme, Variable::as_variable(new_value));
                Ok(())
            }
            Statement::FunctionDeclaration {
                name,
                signature,
                expr,
            } => {
                if let Some(variable) = variables.get(&name.lexeme) {
                    if variable.constant {
                        return Err(EvaluationError::ConstantAssignment { name: name });
                    }
                }
                if let Some(variable) = variables.get(&name.lexeme).cloned() {
                    if let Value::Function(func) = variable.value.clone() {
                        let mut func = func.borrow_mut();
                        match &mut *func {
                            Function::UserDefinedFunction(ref mut func) => {
                                for sig in func.signatures.iter_mut() {
                                    if are_signatures_equivalent(&sig.0, &signature) {
                                        *sig = (signature, expr);
                                        return Ok(());
                                    }
                                }
                                func.signatures.push((signature, expr));
                                return Ok(());
                            }
                            _ => {}
                        }
                    }
                }

                // otherwise just completely replace as new user defined function
                let function = Value::Function(Rc::new(RefCell::new(
                    Function::UserDefinedFunction(UserDefinedFunction {
                        name: name.lexeme.clone(),
                        signatures: vec![(signature, expr)],
                    }),
                )));
                variables.insert(name.lexeme, Variable::as_variable(function));
                Ok(())
            }
        }
    }
}
