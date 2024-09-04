use paste::paste;
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
    name: Ident,
    args: Punctuated<Ident, Comma>,
    body: Expr,
}

impl Parse for FunctionInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![,]>()?;

        // Parse arguments enclosed in parentheses
        let content;
        parenthesized!(content in input);
        let args = Punctuated::parse_terminated(&content)?;

        input.parse::<Token![,]>()?;
        let body: Expr = input.parse()?;

        Ok(FunctionInput { name, args, body })
    }
}

#[proc_macro]
pub fn define_calculator_builtin_function(input: TokenStream) -> TokenStream {
    let FunctionInput { name, args, body } = parse_macro_input!(input as FunctionInput);

    let arity = args.len();

    let unwrap_args = args.iter().enumerate().map(|(i, arg)| {
        quote! {
            let #arg = match args.get(#i) {
                Some(common::value::Value::Number(num)) => num,
                _ => todo!()
            };
        }
    });

    let expanded = quote! {
        paste::paste!{
            fn [<internal_builtin_ #name>](args: Vec<common::value::Value>) -> common::value::Value {
                #(#unwrap_args)*
                #body
            }

            pub const #name: common::value::Value = common::value::Value::Function(common::function::Function {
                function: [<internal_builtin_ #name>],
                arity: #arity,
            });
        }
    };

    TokenStream::from(expanded)
}
