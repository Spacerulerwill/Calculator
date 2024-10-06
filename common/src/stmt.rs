use std::{cell::RefCell, rc::Rc};

use crate::{
    expr::{
        ConstantAssignment, ConstantDeletion, EvaluationError, Expr, InvalidCallable,
        NativeFunctionCantAddSignature, NativeFunctionCantDeleteSignature, UnknownVariable,
        UserDefinedFunctionNoMatchingSignature,
    },
    tokenizer::Token,
    variable::{
        value::{
            function::{Function, Signature, UserDefinedFunction},
            Value,
        },
        Variable, VariableMap,
    },
};

#[derive(Debug)]
pub enum Statement {
    ExpressionStatement(Expr),
    DeleteVariable(Token),
    DeleteFunctionSignature {
        name: Token,
        signature: Signature,
    },
    Assignment {
        identifier: Token,
        expr: Expr,
    },
    FunctionDeclaration {
        name: Token,
        signature: Signature,
        expr: Expr,
    },
    Clear,
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
            Statement::DeleteVariable(name) => {
                if let Some(variable) = variables.get(&name.lexeme) {
                    if variable.constant {
                        return Err(EvaluationError::ConstantDeletion(Box::new(
                            ConstantDeletion { name: name },
                        )));
                    } else {
                        variables.remove(&name.lexeme);
                    }
                } else {
                    return Err(EvaluationError::UnknownVariable(Box::new(
                        UnknownVariable { name: name },
                    )));
                }
                Ok(())
            }
            Statement::DeleteFunctionSignature { name, signature } => {
                let variable_option = variables.get(&name.lexeme).cloned();

                if let Some(variable) = variable_option {
                    if variable.constant {
                        return Err(EvaluationError::ConstantDeletion(Box::new(
                            ConstantDeletion { name: name },
                        )));
                    }
                    if let Value::Function(func) = &variable.value {
                        let mut func = func.borrow_mut();
                        match &mut *func {
                            Function::NativeFunction(_) => {
                                return Err(EvaluationError::NativeFunctionCantDeleteSignature(
                                    Box::new(NativeFunctionCantDeleteSignature { name: name }),
                                ))
                            }
                            Function::UserDefinedFunction(func) => {
                                // Remove the signature if it exists
                                let length_before_removal = func.signatures.len();
                                func.signatures.retain(|(sig, _)| *sig != signature);
                                let length_after_removal = func.signatures.len();

                                // We couldn't find the signature to remove
                                if length_after_removal == length_before_removal {
                                    return Err(
                                        EvaluationError::UserDefinedFunctionNoMatchingSignature(
                                            Box::new(UserDefinedFunctionNoMatchingSignature {
                                                line: name.line,
                                                col: name.col,
                                                name: func.name.clone(),
                                            }),
                                        ),
                                    );
                                }

                                // If there are no signatures left, remove the function from variables
                                if func.signatures.is_empty() {
                                    variables.remove(&name.lexeme);
                                }
                                return Ok(());
                            }
                        }
                    } else {
                        // Not a function, cannot delete signature
                        return Err(EvaluationError::InvalidCallable(Box::new(
                            InvalidCallable {
                                line: name.line,
                                col: name.col,
                            },
                        )));
                    }
                } else {
                    return Err(EvaluationError::UnknownVariable(Box::new(
                        UnknownVariable { name: name },
                    )));
                }
            }
            Statement::Assignment { identifier, expr } => {
                if let Some(variable) = variables.get(&identifier.lexeme) {
                    if variable.constant {
                        return Err(EvaluationError::ConstantAssignment(Box::new(
                            ConstantAssignment { name: identifier },
                        )));
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
                if let Some(variable) = variables.get(&name.lexeme).cloned() {
                    if variable.constant {
                        return Err(EvaluationError::ConstantAssignment(Box::new(
                            ConstantAssignment { name: name },
                        )));
                    }
                    if let Value::Function(func) = variable.value.clone() {
                        let mut func = func.borrow_mut();
                        match &mut *func {
                            Function::UserDefinedFunction(ref mut func) => {
                                for sig in func.signatures.iter_mut() {
                                    if sig.0 == signature {
                                        *sig = (signature, expr);
                                        return Ok(());
                                    }
                                }
                                func.signatures.push((signature, expr));
                                return Ok(());
                            }
                            Function::NativeFunction(_) => {
                                return Err(EvaluationError::NativeFunctionCantAddSignature(
                                    Box::new(NativeFunctionCantAddSignature { name: name }),
                                ))
                            }
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
            Statement::Clear => {
                variables.retain(|_, val| val.constant);
                Ok(())
            }
        }
    }
}
