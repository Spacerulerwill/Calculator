use proc_macro::TokenStream;
use common::quote::quote;
use syn::{
    parenthesized,
    punctuated::Punctuated,
    token::Comma,
    parse::{Parse, ParseStream},
    parse_macro_input, Expr, Ident, Token,
};
use std::str::FromStr;

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
        let constraint = common::value::ValueConstraint::from_str(binding.as_str()).unwrap();
        let constraint_checking = quote! {
            let #name = args.get(#i).unwrap();
            if !#name.fits_value_constraint(#constraint) {
                return Err(common::expr::EvaluationError::IncorrectFunctionArgumentType {
                    function_name: stringify!(#function_name),
                    function_col: col,
                    idx: #i + 1,
                    name: stringify!(#name),
                    constraint: #constraint,
                });
            }
        };

        let extract_variable = match constraint {
            common::value::ValueConstraint::Function => quote!{
                let #name = match #name {
                    Value::Function(f) => f,
                    _ => panic!()
                };
            },
            common::value::ValueConstraint::Number => quote!{
                let #name = match #name {
                    Value::Number(num) => num,
                    _ => panic!()
                };
            },
            common::value::ValueConstraint::Real 
            | common::value::ValueConstraint::Natural 
            | common::value::ValueConstraint::Integer => quote!{
                let #name = match #name {
                    Value::Number(num) => num.re,
                    _ => panic!()
                };
            },
        };

        quote!{
            #constraint_checking
            #extract_variable
        }
    });

    // Generate const and function implementation
    let expanded = quote! {
        #[allow(non_upper_case_globals)]
        pub const #function_name: common::value::Value = {
            fn internal_builtin_function(col: usize, args: Vec<common::value::Value>) -> Result<common::value::Value, common::expr::EvaluationError> {
                // TODO :: determine why this line is neccesary?
                use common::value::ValueConstraint;
                use common::num_complex::ComplexFloat;
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
