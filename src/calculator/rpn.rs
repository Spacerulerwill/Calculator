use std::collections::VecDeque;

use crate::calculator::tokens::{Token, Parenthesis, Associativity, Number, UnaryOp, get_binary_operator_precedence, get_binary_operator_associativity};

pub fn infix_to_rpn(tokens: &Vec<Token>) -> VecDeque<Token> {
    let mut queue: VecDeque<Token> = VecDeque::new();
    let mut operation_stack: Vec<Token> = Vec::new();
    let mut token_iter = tokens.iter().peekable();
    let mut parenthesis_unary_stacks: Vec<Vec<Token>> = Vec::new();

    while let Some(token) = token_iter.next() {
        match token {
            Token::Number(_) | Token::Constant(_) => {
                queue.push_back(*token);
            },
            Token::BinaryOp(op1) => 'binaryop: {
                if operation_stack.is_empty() || operation_stack.last().unwrap().clone() == Token::Parenthesis(Parenthesis::OPEN) {
                    operation_stack.push(*token);
                    break 'binaryop;
                }

                let op1_precedence = get_binary_operator_precedence(*op1);
                let op1_associativity = get_binary_operator_associativity(*op1);
                let mut next_op = operation_stack.last().unwrap();
                if let Token::BinaryOp(op2) = next_op {
                    let op2_precedence = get_binary_operator_precedence(*op2);
                    if op1_precedence > op2_precedence || (op1_precedence == op2_precedence && op1_associativity == Associativity::RIGHT) {
                        operation_stack.push(*token);
                        break 'binaryop;
                    }
                } else {
                    break 'binaryop;
                }

                while !operation_stack.is_empty() {
                    next_op = operation_stack.last().unwrap();
                    if let Token::BinaryOp(op2) = next_op.clone() {
                        let op2_precedence = get_binary_operator_precedence(op2);
                        if op1_precedence < op2_precedence || (op1_precedence == op2_precedence && op1_associativity == Associativity::LEFT) {
                            queue.push_back(operation_stack.pop().unwrap());
                        } else {
                            break 'binaryop;
                        }
                    }  
                    else {
                        break 'binaryop;
                    }
                }
                operation_stack.push(*token);
            },
            Token::UnaryOp(op) => {
                let mut unary_stack: Vec<Token> = Vec::new();
                let mut terminator_token = Token::UnaryOp(UnaryOp::NEGATE); // CANT THINK OF A BETTER DEFAULT VALUE

                unary_stack.push(Token::UnaryOp(*op));
                while let Some(token) = token_iter.peek() {
                    if let Token::UnaryOp(_) = token {
                        unary_stack.push(**token);
                        token_iter.next();
                    } else {
                        terminator_token = **token;
                        break;
                    }
                };

                match terminator_token {
                    Token::Parenthesis(Parenthesis::OPEN) => {
                        parenthesis_unary_stacks.push(unary_stack);
                    },
                    Token::Number(Number::Float(f)) => {
                        token_iter.next();
                        queue.push_back(Token::Number(Number::Float(f)));
                        while !unary_stack.is_empty() {
                            queue.push_back(unary_stack.pop().unwrap());
                        }
                    },
                    Token::Number(Number::Integer(i)) => {
                        token_iter.next();
                        queue.push_back(Token::Number(Number::Integer(i)));
                        while !unary_stack.is_empty() {
                            queue.push_back(unary_stack.pop().unwrap());
                        }
                    },
                    _ => panic!("Unary op terminated on a token that was not a number or an open parenthesis. This should never happen! BUG!")
                }
            },
            Token::Parenthesis(Parenthesis::OPEN) => operation_stack.push(*token),
            Token::Parenthesis(Parenthesis::CLOSED) => {
                while !operation_stack.is_empty() {
                    let next_op = operation_stack.last().unwrap().clone();
                    if next_op != Token::Parenthesis(Parenthesis::OPEN) {
                        queue.push_back(operation_stack.pop().unwrap());
                    } else {
                        operation_stack.pop();
                        break;     
                    }
                }

                if parenthesis_unary_stacks.len() != 0 {
                    let stack = parenthesis_unary_stacks.pop().unwrap();
                    for op in stack {
                        queue.push_back(op);
                    }
                }
            },
        }
    }

    while operation_stack.len() > 0 {
        let op = operation_stack.pop().unwrap();
        if op != Token::Parenthesis(Parenthesis::OPEN){
            queue.push_back(op);
        }
    }

    return queue
}