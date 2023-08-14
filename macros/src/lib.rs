use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    parse::Parse, parse_macro_input, DeriveInput, Fields, Ident, LitStr, PathSegment, Result,
    Token, TypePath,
};

#[proc_macro_derive(Table, attributes(rizzle))]
pub fn table(s: TokenStream) -> TokenStream {
    let input = parse_macro_input!(s as DeriveInput);
    match table_macro(input) {
        Ok(s) => s.to_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[derive(Default)]
struct RizzleAttr {
    table_name: Option<LitStr>,
    primary_key: bool,
    not_null: bool,
    default_value: Option<LitStr>,
}

impl Parse for RizzleAttr {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let mut rizzle_attr = RizzleAttr::default();
        while !input.is_empty() {
            let next = input.parse::<Ident>()?.to_string();
            match next.as_str() {
                "table_name" => {
                    let _ = input.parse::<Token![=]>()?;
                    let table_name: LitStr = input.parse()?;
                    rizzle_attr.table_name = Some(table_name);
                }
                "primary_key" => {
                    rizzle_attr.primary_key = true;
                }
                _ => {}
            }
        }
        Ok(rizzle_attr)
    }
}

fn table_macro(input: DeriveInput) -> Result<TokenStream2> {
    let table_str = input
        .attrs
        .iter()
        .filter_map(|attr| attr.parse_args::<RizzleAttr>().ok())
        .filter(|attr| attr.table_name.is_some())
        .last()
        .expect("define #![rizzle(table_name = \"your table name here\")] on struct")
        .table_name
        .unwrap();
    let struct_name = input.ident;
    let table_name = table_str.value();
    let fields = match input.data {
        syn::Data::Struct(ref data) => &data.fields,
        _ => unimplemented!(),
    };
    let columns = columns(&table_name, &fields);
    let attrs = attrs(&fields);

    Ok(quote! {
        impl Table for #struct_name {
            fn new() -> Self {
                Self { #(#attrs,)* }
            }

            fn name(&self) -> String {
                String::from(#table_str)
            }

            fn columns(&self) -> Vec<Column> {
                vec![#(#columns,)*]
            }

            fn indexes(&self) -> Vec<Index> {
                vec![]
            }

            fn create_table_sql(&self) -> String {
                let columns_sql = self.columns()
                    .iter()
                    .map(|c| c.definition_sql())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("create table {} ({})", self.name(), columns_sql)
            }

            fn drop_table_sql(&self) -> String {
                format!("drop table {}", self.name())
            }
        }
    })
}

fn attrs(fields: &Fields) -> Vec<TokenStream2> {
    fields
        .iter()
        .map(|field| {
            let ident = &field
                .ident
                .as_ref()
                .expect("Struct fields should have names");
            let value = ident.to_string();
            quote! {
                #ident: #value
            }
        })
        .collect::<Vec<_>>()
}

fn columns(table_name: &String, fields: &Fields) -> Vec<TokenStream2> {
    fields
        .iter()
        .map(|field| {
            let primary_key = field
                .attrs
                .iter()
                .filter_map(|attr| attr.parse_args::<RizzleAttr>().ok())
                .any(|attr| attr.primary_key);
            let ident = &field
                .ident
                .as_ref()
                .expect("Struct fields should have names")
                .to_string();
            let ty = match &field.ty {
                syn::Type::Path(TypePath { path, .. }) => match path.segments.last() {
                    Some(PathSegment { ident, .. }) => match ident.to_string().as_str() {
                        "Real" => quote! { sqlite::DataType::Real },
                        "Integer" => quote! { sqlite::DataType::Integer },
                        "Text" => quote! { sqlite::DataType::Text },
                        _ => quote! { sqlite::DataType::Blob },
                    },
                    None => todo!(),
                },
                _ => todo!(),
            };
            quote! {
                Column {
                    table_name: #table_name.to_string(),
                    name: #ident.to_string(),
                    data_type: #ty,
                    primary_key: #primary_key,
                    ..Default::default()
                }
            }
        })
        .collect::<Vec<_>>()
}
