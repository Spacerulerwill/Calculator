/*
Unit tests for the evaluate_rpn function, which takes in tokens in reverse polish notation and evaluates the express to an answer
*/

use std::collections::VecDeque;
use calculator::calculator::{evaluate_rpn, Operator, Token};

#[cfg(test)]
#[test]
fn evaluate_rpn_expr_1() {
    let mut rpn = VecDeque::from(vec![
        Token::Number(3),
        Token::Number(5),
        Token::Op(Operator::Add), 
    ]);

    assert_eq!(evaluate_rpn(&mut rpn), 8);
}

#[test]
fn evaluate_rpn_expr_2() {
    let mut rpn = VecDeque::from(vec![
        Token::Number(7),
        Token::Number(2),
        Token::Op(Operator::Sub), 
    ]);

    assert_eq!(evaluate_rpn(&mut rpn), 5);
}

#[test]
fn evaluate_rpn_expr_3() {
    let mut rpn = VecDeque::from(vec![
        Token::Number(4),
        Token::Number(6),
        Token::Op(Operator::Mul), 
    ]);

    assert_eq!(evaluate_rpn(&mut rpn), 24);
}

#[test]
fn evaluate_rpn_expr_4() {
    let mut rpn = VecDeque::from(vec![
        Token::Number(15),
        Token::Number(3),
        Token::Op(Operator::Div), 
    ]);

    assert_eq!(evaluate_rpn(&mut rpn), 5);
}

#[test]
fn evaluate_rpn_expr_5() {
    let mut rpn = VecDeque::from(vec![
        Token::Number(2),
        Token::Number(4),
        Token::Number(7),
        Token::Op(Operator::Add), 
        Token::Op(Operator::Mul), 
    ]);

    assert_eq!(evaluate_rpn(&mut rpn), 22);
}

#[test]
fn evaluate_rpn_expr_6() {
    let mut rpn = VecDeque::from(vec![
        Token::Number(10),
        Token::Number(3),
        Token::Op(Operator::Sub), 
        Token::Number(8),
        Token::Number(2),
        Token::Op(Operator::Div),
        Token::Op(Operator::Add),
    ]);

    assert_eq!(evaluate_rpn(&mut rpn), 11);
}

#[test]
fn evaluate_rpn_expr_7() {
    let mut rpn = VecDeque::from(vec![
        Token::Number(6),
        Token::Number(4),
        Token::Number(2),
        Token::Op(Operator::Sub), 
        Token::Op(Operator::Mul), 
        Token::Number(5),
        Token::Op(Operator::Add),
    ]);

    assert_eq!(evaluate_rpn(&mut rpn), 17);
}

#[test]
fn evaluate_rpn_expr_8() {
    let mut rpn = VecDeque::from(vec![
        Token::Number(15),
        Token::Number(4),
        Token::Op(Operator::Div), 
        Token::Number(2),
        Token::Number(1),
        Token::Op(Operator::Add),
        Token::Op(Operator::Mul),
    ]);

    assert_eq!(evaluate_rpn(&mut rpn), 9);
}

#[test]
fn evaluate_rpn_expr_9() {
    let mut rpn = VecDeque::from(vec![
        Token::Number(7),
        Token::Number(2),
        Token::Op(Operator::Sub), 
        Token::Number(4),
        Token::Number(3),
        Token::Op(Operator::Add),
        Token::Op(Operator::Mul), 
        Token::Number(10),
        Token::Op(Operator::Sub)
    ]);

    assert_eq!(evaluate_rpn(&mut rpn), 25);
}

#[test]
fn evaluate_rpn_expr_10() {
    let mut rpn = VecDeque::from(vec![
        Token::Number(4),
        Token::Number(6),
        Token::Number(2),
        Token::Op(Operator::Div), 
        Token::Number(1),
        Token::Op(Operator::Add), 
        Token::Op(Operator::Mul), 
        Token::Number(5),
        Token::Op(Operator::Sub),  
    ]);

    assert_eq!(evaluate_rpn(&mut rpn), 11);
}