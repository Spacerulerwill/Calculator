use proc_macro::TokenStream;
use quote::quote;
use syn::parenthesized;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Expr, Ident, Token,
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

        Ok(FunctionInput { function_name, args, body })
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
pub fn define_calculator_builtin_function(input: TokenStream) -> TokenStream {
    let FunctionInput { function_name, args, body } = parse_macro_input!(input as FunctionInput);

    let arity = args.len();

    // Generate argument checking code
    let unwrap_args = args.iter().enumerate().map(|(i, arg)| {
        let Argument { name, arg_type } = arg;
        let binding = arg_type.to_string();
        let arg_type = binding.as_str();
        match arg_type {
            "complex" => return quote!{
                let #name = match args.get(#i) {
                    Some(val) => match val {
                        common::value::Value::Number(val) => val,
                        val => return Err(common::expr::EvaluationError::IncorrectFunctionArgumentType{
                            function_name: stringify!(#function_name),
                            function_col: col,
                            idx: #i + 1,
                            name: stringify!(#name),
                            value: val.clone(),
                            expected_type: #arg_type
                        })
                    },
                    _ => panic!("Missing argument")
                };
            },
            "real" => return quote!{
                let #name = match args.get(#i) {
                    Some(val) => match val {
                        common::value::Value::Number(inner_val) => match inner_val.im {
                            0.0 => inner_val.re,
                            inner_val => return Err(common::expr::EvaluationError::IncorrectFunctionArgumentType{
                                function_name: stringify!(#function_name),
                                function_col: col,
                                idx: #i + 1,
                                name: stringify!(#name),
                                value: val.clone(),
                                expected_type: #arg_type
                            })
                        },
                        _ => return Err(common::expr::EvaluationError::IncorrectFunctionArgumentType{
                            function_name: stringify!(#function_name),
                            function_col: col,
                            idx: #i + 1,
                            name: stringify!(#name),
                            value: val.clone(),
                            expected_type: #arg_type
                        })
                    },
                    _ => panic!("Missing argument")
                };
            },
            "function" => return quote!{
                let #name = match args.get(#i) {
                    Some(val) => match val {
                        common::value::Value::Function(val) => val,
                        _ => return Err(common::expr::EvaluationError::IncorrectFunctionArgumentType{
                            function_name: stringify!(#function_name),
                            function_col: col,
                            idx: #i + 1,
                            name: stringify!(#name),
                            value: val.clone(),
                            expected_type: #arg_type
                        })
                    },
                    _ => panic!("Missing argument")
                };
            },
            _ => panic!("Invalid argument type")
        }
    });

    // Generate const and function implementation
    let expanded = quote! {
        pub const #function_name: common::value::Value = {
            fn internal_builtin_function(col: usize, args: Vec<common::value::Value>) -> Result<common::value::Value, common::expr::EvaluationError> {
                #(#unwrap_args)*
                #body
            }

            common::value::Value::Function(common::function::Function {
                name: stringify!(#function_name),
                function: internal_builtin_function,
                arity: #arity,
            })
        };
    };

    TokenStream::from(expanded)
}
