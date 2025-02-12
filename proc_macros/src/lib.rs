use common::quote::quote;
use proc_macro::TokenStream;
use std::str::FromStr;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token::Comma,
    Expr, Ident, Token,
};

// Define a struct to hold the parsed function input
struct FunctionInput {
    function_name: Ident,
    args: Punctuated<Argument, Comma>,
    body: Expr,
}

impl Parse for FunctionInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let function_name: Ident = input.parse()?;
        input.parse::<Token![,]>()?;

        // Parse arguments enclosed in parentheses
        let content;
        parenthesized!(content in input);
        let args = Punctuated::parse_terminated(&content)?;

        input.parse::<Token![,]>()?;
        let body: Expr = input.parse()?;

        Ok(FunctionInput {
            function_name,
            args,
            body,
        })
    }
}

struct Argument {
    name: Ident,
    arg_type: Ident,
}

impl Parse for Argument {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let arg_type: Ident = input.parse()?;
        Ok(Argument { name, arg_type })
    }
}

#[proc_macro]
pub fn define_calculator_native_function(input: TokenStream) -> TokenStream {
    let FunctionInput {
        function_name,
        args,
        body,
    } = parse_macro_input!(input as FunctionInput);

    let arity = args.len();

    // Generate argument checking code
    let unwrap_args = args.iter().enumerate().map(|(i, arg)| {
        let Argument { name, arg_type } = arg;
        let binding = arg_type.to_string();
        let constraint = common::variable::value::constraint::ValueConstraint::from_str(binding.as_str()).unwrap();
        let constraint_checking = quote! {
            let #name = args.get(#i).unwrap();
            if !#constraint.does_value_fit(&#name) {
                return Err(common::expr::error::EvaluationError::NativeFunctionIncorrectParameterType(Box::new(
                    common::expr::error::NativeFunctionIncorrectParameterType {
                        function_name: stringify!(#function_name).to_string(),
                        line: line,
                        col: col,
                        idx: #i + 1,
                        name: stringify!(#name).to_string(),
                        constraint: #constraint,
                    }
                )));
            }
        };

        let extract_variable = match constraint {
            common::variable::value::constraint::ValueConstraint::Function => quote! {
                let #name = match #name {
                    Value::Function(f) => f,
                    _ => panic!()
                };
            },
            common::variable::value::constraint::ValueConstraint::Number => quote! {
                let #name = match #name {
                    Value::Number(num) => num,
                    _ => panic!()
                };
            },
            common::variable::value::constraint::ValueConstraint::Real
            | common::variable::value::constraint::ValueConstraint::Natural
            | common::variable::value::constraint::ValueConstraint::PositiveInteger
            | common::variable::value::constraint::ValueConstraint::Integer => quote! {
                let #name = match #name {
                    Value::Number(num) => num.re,
                    _ => panic!()
                };
            },
            common::variable::value::constraint::ValueConstraint::Matrix
            | common::variable::value::constraint::ValueConstraint::SquareMatrix => quote! {
                let #name = match #name {
                    Value::Matrix(matrix) => matrix,
                    _ => panic!()
                };
            }
        };

        quote! {
            #constraint_checking
            #extract_variable
        }
    });

    // Generate const and function implementation
    let expanded = quote! {
        #[allow(non_upper_case_globals)]
        pub const #function_name: common::variable::value::function::Function = {
            fn internal_builtin_function(line:usize, col: usize, args: Vec<common::variable::value::Value>) -> Result<common::variable::value::Value, common::expr::error::EvaluationError> {
                // TODO :: determine why this line is neccesary?
                use common::variable::value::constraint::ValueConstraint;
                use common::num_complex::ComplexFloat;
                #(#unwrap_args)*
                #body
            }

            common::variable::value::function::Function::NativeFunction(common::variable::value::function::NativeFunction {
                name: stringify!(#function_name),
                function: internal_builtin_function,
                arity: #arity,
            })
        };
    };

    TokenStream::from(expanded)
}
