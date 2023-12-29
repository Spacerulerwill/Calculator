/*
Unit tests for the infix_to_rpn function, which takes in tokens in infix and returns them in reverse polish notation
*/

use calculator::calculator::{infix_to_rpn, Operator, Token, Parenthesis};

#[cfg(test)]
#[test]
fn infix_to_rpn_expr_1() {
    let infix = vec![
        Token::Number(3), 
        Token::Op(Operator::Add), 
        Token::Number(5)
    ];

    let rpn = vec![
        Token::Number(3),
        Token::Number(5),
        Token::Op(Operator::Add), 
    ];

    assert_eq!(infix_to_rpn(&infix), rpn);
}

#[test]
fn infix_to_rpn_expr_2() {
    let infix = vec![
        Token::Number(7), 
        Token::Op(Operator::Sub), 
        Token::Number(2)
    ];

    let rpn = vec![
        Token::Number(7),
        Token::Number(2),
        Token::Op(Operator::Sub), 
    ];

    assert_eq!(infix_to_rpn(&infix), rpn);
}

#[test]
fn infix_to_rpn_expr_3() {
    let infix = vec![
        Token::Number(4), 
        Token::Op(Operator::Mul), 
        Token::Number(6)
    ];

    let rpn = vec![
        Token::Number(4),
        Token::Number(6),
        Token::Op(Operator::Mul), 
    ];

    assert_eq!(infix_to_rpn(&infix), rpn);
}

#[test]
fn infix_to_rpn_expr_4() {
    let infix = vec![
        Token::Number(15), 
        Token::Op(Operator::Div), 
        Token::Number(3)
    ];

    let rpn = vec![
        Token::Number(15),
        Token::Number(3),
        Token::Op(Operator::Div), 
    ];

    assert_eq!(infix_to_rpn(&infix), rpn);
}

#[test]
fn infix_to_rpn_expr_5() {
    let infix = vec![
        Token::Number(2), 
        Token::Op(Operator::Mul), 
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(4), 
        Token::Op(Operator::Add), 
        Token::Number(7), 
        Token::Parenthesis(Parenthesis::CLOSED),
    ];

    let rpn = vec![
        Token::Number(2),
        Token::Number(4),
        Token::Number(7),
        Token::Op(Operator::Add), 
        Token::Op(Operator::Mul), 
    ];

    assert_eq!(infix_to_rpn(&infix), rpn);
}

#[test]
fn infix_to_rpn_expr_6() {
    let infix = vec![
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(10),
        Token::Op(Operator::Sub), 
        Token::Number(3),
        Token::Parenthesis(Parenthesis::CLOSED),
        Token::Op(Operator::Add), 
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(8),
        Token::Op(Operator::Div), 
        Token::Number(2),
        Token::Parenthesis(Parenthesis::CLOSED),
    ];

    let rpn = vec![
        Token::Number(10),
        Token::Number(3),
        Token::Op(Operator::Sub), 
        Token::Number(8),
        Token::Number(2),
        Token::Op(Operator::Div),
        Token::Op(Operator::Add),
    ];

    assert_eq!(infix_to_rpn(&infix), rpn);
}

#[test]
fn infix_to_rpn_expr_7() {
    let infix = vec![
        Token::Number(6), 
        Token::Op(Operator::Mul), 
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(4),
        Token::Op(Operator::Sub),
        Token::Number(2),
        Token::Parenthesis(Parenthesis::CLOSED),
        Token::Op(Operator::Add),
        Token::Number(5)
    ];

    let rpn = vec![
        Token::Number(6),
        Token::Number(4),
        Token::Number(2),
        Token::Op(Operator::Sub), 
        Token::Op(Operator::Mul), 
        Token::Number(5),
        Token::Op(Operator::Add), 
    ];

    assert_eq!(infix_to_rpn(&infix), rpn);
}

#[test]
fn infix_to_rpn_expr_8() {
    let infix = vec![
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(15),
        Token::Op(Operator::Div), 
        Token::Number(4),
        Token::Parenthesis(Parenthesis::CLOSED),
        Token::Op(Operator::Mul), 
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(2),
        Token::Op(Operator::Add), 
        Token::Number(1),
        Token::Parenthesis(Parenthesis::CLOSED),
    ];

    let rpn = vec![
        Token::Number(15),
        Token::Number(4),
        Token::Op(Operator::Div), 
        Token::Number(2),
        Token::Number(1),
        Token::Op(Operator::Add),
        Token::Op(Operator::Mul),
    ];

    assert_eq!(infix_to_rpn(&infix), rpn);
}

#[test]
fn infix_to_rpn_expr_9() {
    let infix = vec![
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(7),
        Token::Op(Operator::Sub), 
        Token::Number(2),
        Token::Parenthesis(Parenthesis::CLOSED),
        Token::Op(Operator::Mul), 
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(4),
        Token::Op(Operator::Add), 
        Token::Number(3),
        Token::Parenthesis(Parenthesis::CLOSED),
        Token::Op(Operator::Sub),
        Token::Number(10)
    ];

    let rpn = vec![
        Token::Number(7),
        Token::Number(2),
        Token::Op(Operator::Sub), 
        Token::Number(4),
        Token::Number(3),
        Token::Op(Operator::Add),
        Token::Op(Operator::Mul), 
        Token::Number(10),
        Token::Op(Operator::Sub)
    ];

    assert_eq!(infix_to_rpn(&infix), rpn);
}

#[test]
fn infix_to_rpn_expr_10() {
    let infix = vec![
        Token::Number(4), 
        Token::Op(Operator::Mul), 
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(6), 
        Token::Op(Operator::Div), 
        Token::Number(2), 
        Token::Parenthesis(Parenthesis::CLOSED),
        Token::Op(Operator::Add), 
        Token::Number(1), 
        Token::Parenthesis(Parenthesis::CLOSED),
        Token::Op(Operator::Sub), 
        Token::Number(5), 
    ];

    let rpn = vec![
        Token::Number(4),
        Token::Number(6),
        Token::Number(2),
        Token::Op(Operator::Div), 
        Token::Number(1),
        Token::Op(Operator::Add), 
        Token::Op(Operator::Mul), 
        Token::Number(5),
        Token::Op(Operator::Sub), 
    ];

    assert_eq!(infix_to_rpn(&infix), rpn);
}
