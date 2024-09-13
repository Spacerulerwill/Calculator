mod builtin_math;

use builtin_math::get_starting_variables;
use clap::Parser as ClapParser;
use common::{
    expr::{EvaluationError, GroupingKind},
    parser::{Parser, ParserError},
    tokenizer::{Tokenizer, TokenizerError},
    value::ValueConstraint,
    variable::Variable,
};
use std::{
    collections::HashMap,
    io::{self, Write},
};

const DEFAULT_TAB_SIZE: u8 = 4;

#[derive(ClapParser)]
#[command(name = "calculator")]
#[command(version = "0.0")]
#[command(about = "Evaluate a mathematical expression", long_about = None)]
struct Args {
    #[arg()]
    expression: Option<String>,

    #[arg(short, long, default_value_t = DEFAULT_TAB_SIZE)]
    tabsize: u8,
}

fn main() {
    let args = Args::parse();
    let mut variables = get_starting_variables();
    if let Some(expression) = args.expression {
        process_expression(&expression.trim(), &mut variables, args.tabsize);
    } else {
        start_repl(args.tabsize, &mut variables);
    }
}

fn process_expression(expression: &str, variables: &mut HashMap<String, Variable>, tabsize: u8) {
    let tokens = match Tokenizer::tokenize(expression, tabsize) {
        Ok(tokenizer) => tokenizer.tokens,
        Err(err) => {
            match err {
                TokenizerError::BadChar(char, char_pos) => {
                    eprintln!("Position {char_pos} :: Unexpected or invalid character: '{char}'");
                }
            }
            return;
        }
    };

    let expr = match Parser::parse(tokens) {
        Ok(expr) => expr,
        Err(err) => {
            match err {
                ParserError::ExpectedExpression { found } => {
                    if let Some(found) = found {
                        eprintln!(
                            "Position {} :: Expected expression next but found '{}'",
                            found.col,
                            &found.kind.get_lexeme()
                        );
                    } else {
                        eprintln!("Expected expression next but found EOF");
                    }
                }
                ParserError::ExpectedToken { expected, found } => {
                    if let Some(found) = found {
                        eprintln!(
                            "Column {} :: Expected {:?} but found '{}'",
                            found.col,
                            expected,
                            found.kind.get_lexeme()
                        );
                    } else {
                        eprintln!("Expected {} but found EOF", &expected.get_lexeme());
                    }
                }
                ParserError::ExpectedEOF { found } => {
                    eprintln!("Expected EOF but found '{}'", &found.kind.get_lexeme())
                }
                ParserError::InvalidAssignmentTarget { equal } => {
                    eprintln!("Column {} :: Invalid assignment target", equal.col)
                }
            }
            return;
        }
    };

    let result = match expr.evaluate(variables) {
        Ok(result) => result,
        Err(err) => {
            match err {
                EvaluationError::DivisionByZero { operator } => eprintln!(
                    "Column {} :: Divison by zero on right side of '{}'",
                    operator.col,
                    &operator.kind.get_lexeme()
                ),
                EvaluationError::IncorrectFunctionArgumentCount {
                    paren,
                    name,
                    received,
                    required,
                } => eprintln!(
                    "Column {} :: Function '{}' requires {} argument(s) but received {}",
                    paren.col, name, required, received,
                ),
                EvaluationError::UnsupportedBinaryOperator {
                    operator,
                    constraint
                } => eprintln!(
                    "Column {} :: Cannot apply binary operator '{}' as one or more operands does not meet value constraint '{}'",
                    operator.col,
                    &operator.kind.get_lexeme(),
                    constraint
                ),
                EvaluationError::UnsupportedUnaryOperator { operator, constraint} => eprintln!(
                    "Column {} :: Cannot apply unary operator '{}' as operand does not meet value constraint '{}'",
                    operator.col,
                    &operator.kind.get_lexeme(),
                    constraint
                ),
                EvaluationError::InvalidCallable { paren } => eprintln!(
                    "Column {} :: Callee does not meet value constraint '{}'",
                    paren.col,
                    ValueConstraint::Function
                ),
                EvaluationError::GroupingValueConstraintNotMet { paren, kind, constraint } => {
                    let grouping_str = match kind {
                        GroupingKind::Grouping => "grouping",
                        GroupingKind::Absolute => "absolute grouping",
                        GroupingKind::Ceil => "ceil grouping",
                        GroupingKind::Floor => "floor grouping"
                    };
                    eprintln!(
                        "Column {} :: Value in {grouping_str} does meet value constraint '{}'",
                        paren.col,
                        constraint
                    )
                },
                EvaluationError::IncorrectFunctionArgumentType {
                    function_name,
                    function_col,
                    idx,
                    name,
                    constraint
                } => eprintln!(
                    "Column {} :: Argument {} ({}) for function '{}' does not meet value constraint '{}'",
                    function_col,
                    idx,
                    name,
                    function_name,
                    constraint,
                ),
                EvaluationError::ConstantAssignment { name } => eprintln!(
                    "Column {} :: Cannot assign to '{}' as it is constant",
                    name.col,
                    &name.kind.get_lexeme()
                ),
                EvaluationError::UnknownVariable { name } => eprintln!(
                    "Column {} :: Unknown variable '{}'",
                    name.col,
                    &name.kind.get_lexeme()
                ),
            }
            return;
        }
    };

    println!("{}", &result);
}

fn start_repl(tabsize: u8, variables: &mut HashMap<String, Variable>) {
    let mut input = String::new();

    println!("Enter mathematical expressions to evaluate. Type 'exit' to quit.");

    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to flush stdout");

        input.clear();
        if io::stdin().read_line(&mut input).is_err() {
            eprintln!("Failed to read input");
            continue;
        }

        let trimmed_input = input.trim();
        if trimmed_input.eq_ignore_ascii_case("exit") {
            break;
        }

        process_expression(trimmed_input, variables, tabsize);
    }
}
