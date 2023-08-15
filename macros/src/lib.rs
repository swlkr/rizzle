use proc_macro::{Span, TokenStream};
use proc_macro2::Span as Span2;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::Field;
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
    columns: Option<LitStr>,
}

impl Parse for RizzleAttr {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let mut rizzle_attr = RizzleAttr::default();
        while !input.is_empty() {
            let next = input.parse::<Ident>()?.to_string();
            match next.as_str() {
                "table" => {
                    let _ = input.parse::<Token![=]>()?;
                    let table_name: LitStr = input.parse()?;
                    rizzle_attr.table_name = Some(table_name);
                }
                "primary_key" => {
                    rizzle_attr.primary_key = true;
                }
                "not_null" => {
                    rizzle_attr.not_null = true;
                }
                "default" => {
                    let _ = input.parse::<Token![=]>()?;
                    let default_value: LitStr = input.parse()?;
                    rizzle_attr.default_value = Some(default_value);
                }
                "columns" => {
                    let _ = input.parse::<Token![=]>()?;
                    let columns: LitStr = input.parse()?;
                    rizzle_attr.columns = Some(columns);
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
    let indexes = indexes(&table_name, &fields);

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
                vec![#(#indexes,)*]
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

            fn create_indexes_sql(&self) -> String {
                self.indexes()
                    .iter()
                    .map(|i| i.create_index_sql())
                    .collect::<Vec<_>>()
                    .join(";")
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

fn data_type_from_field(field: &Field) -> TokenStream2 {
    match &field.ty {
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
    }
}

fn columns(table_name: &String, fields: &Fields) -> Vec<TokenStream2> {
    fields
        .iter()
        .filter(|field| match &field.ty {
            syn::Type::Path(TypePath { path, .. }) => match path.segments.last() {
                Some(PathSegment { ident, .. }) => match ident.to_string().as_str() {
                    "Real" | "Integer" | "Text" | "Blob" => true,
                    _ => false,
                },
                None => false,
            },
            _ => false,
        })
        .map(|field| {
            let attr = field
                .attrs
                .iter()
                .filter_map(|attr| attr.parse_args::<RizzleAttr>().ok())
                .last()
                .unwrap_or_default();
            let ident = &field
                .ident
                .as_ref()
                .expect("Struct fields should have names")
                .to_string();
            let RizzleAttr {
                primary_key,
                not_null,
                default_value,
                ..
            } = attr;
            let default_value = match default_value {
                Some(default) => quote! { Some(#default) },
                None => quote! { None },
            };
            let ty = data_type_from_field(&field);
            quote! {
                Column {
                    table_name: #table_name.to_string(),
                    name: #ident.to_string(),
                    data_type: #ty,
                    primary_key: #primary_key,
                    not_null: #not_null,
                    default_value: #default_value
                }
            }
        })
        .collect::<Vec<_>>()
}

fn indexes(table_name: &String, fields: &Fields) -> Vec<TokenStream2> {
    fields
        .iter()
        .filter(|field| match &field.ty {
            syn::Type::Path(TypePath { path, .. }) => match path.segments.last() {
                Some(PathSegment { ident, .. }) => match ident.to_string().as_str() {
                    "Index" | "UniqueIndex" => true,
                    _ => false,
                },
                None => false,
            },
            _ => false,
        })
        .map(|field| {
            let attr = field
                .attrs
                .iter()
                .filter_map(|attr| attr.parse_args::<RizzleAttr>().ok())
                .last()
                .unwrap_or_default();
            let ident = &field
                .ident
                .as_ref()
                .expect("Struct fields should have names")
                .to_string();
            let ty = match &field.ty {
                syn::Type::Path(TypePath { path, .. }) => match path.segments.last() {
                    Some(PathSegment { ident, .. }) => match ident.to_string().as_str() {
                        "Index" => quote! { sqlite::IndexType::Plain },
                        "UniqueIndex" => quote! { sqlite::IndexType::Unique },
                        _ => unimplemented!(),
                    },
                    None => unimplemented!(),
                },
                _ => unimplemented!(),
            };
            let column_names = match attr.columns {
                Some(lit) => lit,
                None => LitStr::new("", Span2::call_site()),
            };
            quote! {
                Index {
                    table_name: #table_name.to_string(),
                    name: #ident.to_string(),
                    index_type: #ty,
                    column_names: #column_names,
                }
            }
        })
        .collect::<Vec<_>>()
}
