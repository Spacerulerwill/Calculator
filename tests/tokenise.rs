/*
Unit tests for testing the tokenise function, which parses and converts a mathematical expression into tokens
*/

use calculator::calculator::{tokenise, Operator, Token, Parenthesis};

#[cfg(test)]
#[test]
fn tokenise_expr_1() {
    let input = "3 + 5";
    let result = tokenise(input).unwrap();
    let expected_tokens = vec![
        Token::Number(3), 
        Token::Op(Operator::Add), 
        Token::Number(5)
    ];
    assert_eq!(result, expected_tokens);
}

#[test]
fn tokenise_expr_2() {
    let input = "7 - 2";
    let result = tokenise(input).unwrap();
    let expected_tokens = vec![
        Token::Number(7), 
        Token::Op(Operator::Sub), 
        Token::Number(2)
    ];
    assert_eq!(result, expected_tokens);
}

#[test]
fn tokenise_expr_3() {
    let input = "4 * 6";
    let result = tokenise(input).unwrap();
    let expected_tokens = vec![
        Token::Number(4), 
        Token::Op(Operator::Mul), 
        Token::Number(6)
    ];
    assert_eq!(result, expected_tokens);
}

#[test]
fn tokenise_expr_4() {
    let input = "15 / 3";
    let result = tokenise(input).unwrap();
    let expected_tokens = vec![
        Token::Number(15), 
        Token::Op(Operator::Div), 
        Token::Number(3)
    ];
    assert_eq!(result, expected_tokens);
}

#[test]
fn tokenise_expr_5() {
    let input = "2 * (4 + 7)";
    let result = tokenise(input).unwrap();
    let expected_tokens = vec![
        Token::Number(2),
        Token::Op(Operator::Mul),
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(4),
        Token::Op(Operator::Add),
        Token::Number(7),
        Token::Parenthesis(Parenthesis::CLOSED),
    ];
    assert_eq!(result, expected_tokens);
}

#[test]
fn tokenise_expr_6() {
    let input = "(10 - 3) + (8 / 2)";
    let result = tokenise(input).unwrap();
    let expected_tokens = vec![
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
    assert_eq!(result, expected_tokens);
}

#[test]
fn tokenise_expr_7() {
    let input = "6 * (4 - 2) + 5";
    let result = tokenise(input).unwrap();
    let expected_tokens = vec![
        Token::Number(6),
        Token::Op(Operator::Mul),
        Token::Parenthesis(Parenthesis::OPEN),
        Token::Number(4),
        Token::Op(Operator::Sub),
        Token::Number(2),
        Token::Parenthesis(Parenthesis::CLOSED),
        Token::Op(Operator::Add),
        Token::Number(5),
    ];
    assert_eq!(result, expected_tokens);
}

#[test]
fn tokenise_expr_8() {
    let input = "(15 / 4) * (2 + 1)";
    let result = tokenise(input).unwrap();
    let expected_tokens = vec![
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
    assert_eq!(result, expected_tokens);
}

#[test]
fn tokenise_expr_9() {
    let input = "(7 - 2) * (4 + 3) - 10";
    let result = tokenise(input).unwrap();
    let expected_tokens = vec![
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
        Token::Number(10),
    ];
    assert_eq!(result, expected_tokens);
}

#[test]
fn tokenise_expr_10() {
    let input = "4 * ((6 / 2) + 1) - 5";
    let result = tokenise(input).unwrap();
    let expected_tokens = vec![
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
    assert_eq!(result, expected_tokens);
}