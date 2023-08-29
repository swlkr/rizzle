use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    parse::Parse, parse_macro_input, Data, DeriveInput, Expr, ExprAssign, ExprLit, ExprPath, Field,
    GenericArgument, Ident, Lit, LitStr, Path, PathArguments, PathSegment, Result, Type, TypePath,
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
    references: Option<LitStr>,
    many: Option<LitStr>,
    from: Option<LitStr>,
    to: Option<LitStr>,
}

impl Parse for RizzleAttr {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let mut rizzle_attr = RizzleAttr::default();
        let args_parsed =
            syn::punctuated::Punctuated::<Expr, syn::Token![,]>::parse_terminated(input)?;
        for expr in args_parsed.iter() {
            match expr {
                Expr::Assign(ExprAssign { left, right, .. }) => match (&**left, &**right) {
                    (Expr::Path(ExprPath { path, .. }), Expr::Lit(ExprLit { lit, .. })) => {
                        if let (Some(PathSegment { ident, .. }), Lit::Str(lit_str)) =
                            (path.segments.last(), lit)
                        {
                            match ident.to_string().as_ref() {
                                "table" => {
                                    rizzle_attr.table_name = Some(lit_str.clone());
                                }
                                "default" => {
                                    rizzle_attr.default_value = Some(lit_str.clone());
                                }
                                "columns" => {
                                    rizzle_attr.columns = Some(lit_str.clone());
                                }
                                "references" => {
                                    rizzle_attr.references = Some(lit_str.clone());
                                }
                                "many" => {
                                    rizzle_attr.many = Some(lit_str.clone());
                                }
                                "from" => {
                                    rizzle_attr.from = Some(lit_str.clone());
                                }
                                "to" => {
                                    rizzle_attr.to = Some(lit_str.clone());
                                }
                                _ => unimplemented!(),
                            }
                        }
                    }
                    _ => unimplemented!(),
                },
                Expr::Path(path) => match path.path.segments.len() {
                    1 => match path
                        .path
                        .segments
                        .first()
                        .unwrap()
                        .ident
                        .to_string()
                        .as_ref()
                    {
                        "not_null" => rizzle_attr.not_null = true,
                        "primary_key" => rizzle_attr.primary_key = true,
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            }
        }

        Ok(rizzle_attr)
    }
}

struct RizzleField {
    ident: String,
    field: Field,
    attrs: Vec<RizzleAttr>,
    type_string: String,
    vec_type: Option<Type>,
}

fn table_macro(input: DeriveInput) -> Result<TokenStream2> {
    let table_str = input
        .attrs
        .iter()
        .filter_map(|attr| attr.parse_args::<RizzleAttr>().ok())
        .last()
        .expect("define #![rizzle(table = \"your table name here\")] on struct")
        .table_name
        .unwrap();
    let struct_name = input.ident;
    let table_name = table_str.value();
    let rizzle_fields = match input.data {
        syn::Data::Struct(ref data) => data
            .fields
            .iter()
            .map(|field| {
                let ident = field
                    .ident
                    .as_ref()
                    .expect("Struct fields should have names")
                    .to_string();
                RizzleField {
                    ident: ident.to_owned(),
                    field: field.clone(),
                    attrs: field
                        .attrs
                        .iter()
                        .filter_map(|attr| attr.parse_args::<RizzleAttr>().ok())
                        .collect::<Vec<_>>(),
                    type_string: string_type_from_field(&field),
                    vec_type: generic_from_vec(&field.ty),
                }
            })
            .collect::<Vec<_>>(),
        _ => unimplemented!(),
    };
    let columns = columns(&table_name, &rizzle_fields);
    let attrs = struct_attrs(&table_name, &rizzle_fields);
    let indexes = indexes(&table_name, &rizzle_fields);
    let references = references(&table_name, &rizzle_fields);

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

            fn references(&self) -> Vec<Reference> {
                vec![#(#references,)*]
            }

            fn create_sql(&self) -> String {
                let columns_sql = self.columns()
                    .iter()
                    .map(|c| c.definition_sql())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("create table {} ({})", self.name(), columns_sql)
            }
        }
    })
}

fn references(table_name: &String, fields: &Vec<RizzleField>) -> Vec<TokenStream2> {
    fields
        .iter()
        .filter(|field| match field.type_string.as_str() {
            "Real" | "Integer" | "Text" | "Blob" | "Many" => true,
            _ => false,
        })
        .filter(|field| match field.attrs.last() {
            Some(attr) => attr.references.is_some(),
            None => false,
        })
        .map(|field| {
            let RizzleAttr { references, .. } = field.attrs.last().unwrap();
            let many = field.type_string == "Many";
            quote! {
                Reference {
                    table: #table_name.to_owned(),
                    clause: #references.to_owned(),
                    many: #many,
                    ..Default::default()
                }
            }
        })
        .collect()
}

fn string_type_from_field(field: &Field) -> String {
    match &field.ty {
        syn::Type::Path(TypePath { path, .. }) => match path.segments.last() {
            Some(PathSegment { ident, .. }) => ident.to_string(),
            None => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn struct_attrs(table_name: &String, fields: &Vec<RizzleField>) -> Vec<TokenStream2> {
    fields
        .iter()
        .map(|f| {
            let ident = &f
                .field
                .ident
                .as_ref()
                .expect("Struct fields should have names");
            let value = format!("{}.{}", table_name, ident.to_string());
            quote! {
                #ident: #value
            }
        })
        .collect::<Vec<_>>()
}

fn data_type(string_type: &String) -> TokenStream2 {
    match string_type.as_str() {
        "Real" => quote! { sqlite::DataType::Real },
        "Integer" => quote! { sqlite::DataType::Integer },
        "Text" => quote! { sqlite::DataType::Text },
        _ => quote! { sqlite::DataType::Blob },
    }
}

fn columns(table_name: &String, fields: &Vec<RizzleField>) -> Vec<TokenStream2> {
    fields
        .iter()
        .filter(|field| match field.type_string.as_ref() {
            "Real" | "Integer" | "Text" | "Blob" => true,
            _ => false,
        })
        .map(|field| {
            let ty = data_type(&field.type_string);
            let ident = &field.ident;
            if let Some(RizzleAttr {
                primary_key,
                not_null,
                default_value,
                references,
                ..
            }) = field.attrs.last()
            {
                let default_value = match default_value {
                    Some(default) => quote! { Some(#default) },
                    None => quote! { None },
                };
                let references = match references {
                    Some(references) => quote! { Some(#references.to_owned()) },
                    None => quote! { None },
                };
                quote! {
                    Column {
                        table_name: #table_name.to_string(),
                        name: #ident.to_string(),
                        data_type: #ty,
                        primary_key: #primary_key,
                        not_null: #not_null,
                        default_value: #default_value,
                        references: #references,
                        ..Default::default()
                    }
                }
            } else {
                quote! {
                    Column {
                        table_name: #table_name.to_string(),
                        name: #ident.to_string(),
                        data_type: #ty,
                        ..Default::default()
                    }
                }
            }
        })
        .collect::<Vec<_>>()
}

fn indexes(table_name: &String, fields: &Vec<RizzleField>) -> Vec<TokenStream2> {
    fields
        .iter()
        .filter(|f| match f.type_string.as_ref() {
            "Index" | "UniqueIndex" => true,
            _ => false,
        })
        .filter(|f| match f.attrs.last() {
            Some(attr) => attr.columns.is_some(),
            None => false,
        })
        .map(|f| {
            let ty = match f.type_string.as_ref() {
                "Index" => quote! { sqlite::IndexType::Plain },
                "UniqueIndex" => quote! { sqlite::IndexType::Unique },
                _ => unimplemented!(),
            };
            let name = &f.ident;
            let RizzleAttr { columns, .. } = f.attrs.last().unwrap();
            let column_names = match columns {
                Some(lit_str) => quote! { #lit_str.to_string() },
                None => quote! { "".to_string() },
            };
            quote! {
                Index {
                    table_name: #table_name.to_string(),
                    name: #name.to_string(),
                    index_type: #ty,
                    column_names: #column_names
                }
            }
        })
        .collect::<Vec<_>>()
}

fn fields(data: &Data) -> Vec<(&Ident, &Type)> {
    match data {
        syn::Data::Struct(ref data) => data
            .fields
            .iter()
            .map(|field| {
                let ident = field
                    .ident
                    .as_ref()
                    .expect("Struct fields should have names");
                let ty = &field.ty;
                (ident, ty)
            })
            .collect::<Vec<_>>(),
        _ => unimplemented!(),
    }
}

#[proc_macro_derive(Select, attributes(rizzle))]
pub fn select(s: TokenStream) -> TokenStream {
    let input = parse_macro_input!(s as DeriveInput);
    match select_macro(input) {
        Ok(s) => s.to_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn select_macro(input: DeriveInput) -> Result<TokenStream2> {
    let struct_name = input.ident;
    let fields = fields(&input.data);
    let column_names = fields
        .iter()
        .map(|(ident, _)| ident.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    Ok(quote! {
        impl Select for #struct_name {
            fn select_sql(&self) -> String {
                format!("select {}", #column_names)
            }
        }
    })
}

#[proc_macro_derive(Insert, attributes(rizzle))]
pub fn insert(s: TokenStream) -> TokenStream {
    let input = parse_macro_input!(s as DeriveInput);
    match insert_macro(input) {
        Ok(s) => s.to_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn insert_macro(input: DeriveInput) -> Result<TokenStream2> {
    let struct_name = input.ident;
    let fields = match input.data {
        syn::Data::Struct(ref data) => data
            .fields
            .iter()
            .map(|field| {
                let ident = field
                    .ident
                    .as_ref()
                    .expect("Struct fields should have names");
                let ty = &field.ty;
                (ident, ty)
            })
            .collect::<Vec<_>>(),
        _ => unimplemented!(),
    };
    let column_names = fields
        .iter()
        .map(|(ident, _)| ident.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    let sql_placeholders = &fields.iter().map(|_| "?").collect::<Vec<_>>();
    let placeholders = &fields.iter().map(|_| "{}").collect::<Vec<_>>().join(", ");
    let data_values = &fields
        .iter()
        .map(|(ident, _)| quote! { self.#ident.clone().into() })
        .collect::<Vec<_>>();
    Ok(quote! {
        impl Insert for #struct_name {
            fn insert_values(&self) -> Vec<DataValue> {
                vec![
                    #(#data_values,)*
                ]
            }

            fn insert_sql(&self) -> String {
                let values_sql = format!(#placeholders, #(#sql_placeholders,)*);
                format!("({}) values ({})", #column_names, values_sql)
            }
        }
    })
}

#[proc_macro_derive(New, attributes(rizzle))]
pub fn new(s: TokenStream) -> TokenStream {
    let input = parse_macro_input!(s as DeriveInput);
    match new_macro(input) {
        Ok(s) => s.to_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn new_macro(input: DeriveInput) -> Result<TokenStream2> {
    let fields = match input.data {
        syn::Data::Struct(ref data) => data
            .fields
            .iter()
            .map(|field| {
                let ident = field
                    .ident
                    .as_ref()
                    .expect("Struct fields should have names");
                let ty = &field.ty;
                (ident, ty)
            })
            .collect::<Vec<_>>(),
        _ => unimplemented!(),
    };
    let attrs = &fields
        .iter()
        .map(|(ident, ty)| {
            quote! {
                #ident: #ty::default()
            }
        })
        .collect::<Vec<_>>();
    let struct_name = input.ident;
    Ok(quote! {
        impl New for #struct_name {
           fn new() -> Self {
               Self { #(#attrs,)* }
           }
        }
    })
}

#[proc_macro_derive(Update, attributes(rizzle))]
pub fn update(s: TokenStream) -> TokenStream {
    let input = parse_macro_input!(s as DeriveInput);
    match update_macro(input) {
        Ok(s) => s.to_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn update_macro(input: DeriveInput) -> Result<TokenStream2> {
    let struct_name = input.ident;
    let fields = match input.data {
        syn::Data::Struct(ref data) => data
            .fields
            .iter()
            .map(|field| {
                let ident = field
                    .ident
                    .as_ref()
                    .expect("Struct fields should have names");
                let ty = &field.ty;
                (ident, ty)
            })
            .collect::<Vec<_>>(),
        _ => unimplemented!(),
    };
    let placeholders = &fields
        .iter()
        .map(|(ident, _)| format!("{} = ?", ident))
        .collect::<Vec<_>>()
        .join(", ");
    let data_values = &fields
        .iter()
        .map(|(ident, _)| quote! { self.#ident.clone().into() })
        .collect::<Vec<_>>();
    Ok(quote! {
        impl Update for #struct_name {
            fn update_values(&self) -> Vec<DataValue> {
                vec![
                    #(#data_values,)*
                ]
            }

            fn update_sql(&self) -> String {
                format!("set {}", #placeholders)
            }
        }
    })
}

#[proc_macro_derive(Row, attributes(rizzle))]
pub fn row(s: TokenStream) -> TokenStream {
    let input = parse_macro_input!(s as DeriveInput);
    match row_macro(input) {
        Ok(s) => s.to_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn row_macro(input: DeriveInput) -> Result<TokenStream2> {
    let insert_token_stream = insert_macro(input.clone())?;
    let update_token_stream = update_macro(input.clone())?;
    let select_token_stream = select_macro(input.clone())?;
    let struct_name = input.ident;
    let fields = fields(&input.data);
    let attrs = &fields.iter().map(|(ident, _)| ident).collect::<Vec<_>>();
    let gets = &fields
        .iter()
        .map(|(ident, _)| {
            let lit_str = ident.to_string();
            return quote! { let #ident = row.try_get(#lit_str)?; };
        })
        .collect::<Vec<_>>();
    Ok(quote! {
        #insert_token_stream
        #update_token_stream
        #select_token_stream
        impl<'r> FromRow<'r, sqlite::SqliteRow> for #struct_name {
            fn from_row(row: &'r sqlite::SqliteRow) -> Result<Self, sqlx::Error> {
                #(#gets)*

                Ok(#struct_name { #(#attrs,)* })
            }
        }
    })
}

#[proc_macro_derive(Pull, attributes(rizzle))]
pub fn pull(s: TokenStream) -> TokenStream {
    let input = parse_macro_input!(s as DeriveInput);
    match pull_macro(input) {
        Ok(s) => s.to_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn pull_macro(input: DeriveInput) -> Result<TokenStream2> {
    let struct_name = input.ident;
    let struct_fields = rizzle_fields(&input.data);
    let json_object = struct_fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let attr = field.attrs.first();
            match attr {
                Some(RizzleAttr { many, from, to , ..}) => {
                    let many= match many.as_ref() {
                        Some(lit_str) => lit_str.value(),
                        None => ident.to_string(),
                    };
                    let from = from.as_ref().expect("A pull struct requires a #[rizzle(from =\"table_name.foreign_key_column\")] attribute").token();
                    let to = to.as_ref().expect("A pull struct requires a #[rizzle(to = \"foreign_table_name.primary_key\")] attribute").token();
                    let type_generic = field.vec_type.as_ref().expect("Pulls structs only support Vec<T> of other structs which implement Pull");
                    let vec_ident= last_segment_from_type(&type_generic).unwrap();
                    quote! {
                        format!(
                            "'{}', (select json_group_array({}) from {} where {} = {})",
                            #ident,
                            #vec_ident::default().json_object_sql(),
                            #many,
                            #from,
                            #to
                        )
                    }
                }
                _ => quote! { format!("'{}', {}", #ident, #ident) },
            }
        })
        .collect::<Vec<_>>();
    Ok(quote! {
        impl FromRow<'_, sqlite::SqliteRow> for #struct_name {
            fn from_row(row: &sqlite::SqliteRow) -> Result<Self, sqlx::Error> {
                let json = &row.try_get::<String, &str>("__pull__")?;
                let row = serde_json::from_str(json).map_err(|e| sqlx::Error::ColumnNotFound(format!("{}", e)))?;
                Ok(row)
            }
        }

        impl Pull for #struct_name {
            fn json_object_sql(&self) -> String {
                format!("json_object({})", vec![#(#json_object,)*].join(", "))
            }
        }
    })
}

fn rizzle_fields(data: &Data) -> Vec<RizzleField> {
    match data {
        syn::Data::Struct(ref data) => data
            .fields
            .iter()
            .map(|field| {
                let ident = field
                    .ident
                    .as_ref()
                    .expect("Struct fields should have names")
                    .to_string();
                RizzleField {
                    ident: ident.to_owned(),
                    field: field.clone(),
                    attrs: field
                        .attrs
                        .iter()
                        .filter_map(|attr| attr.parse_args::<RizzleAttr>().ok())
                        .collect::<Vec<_>>(),
                    type_string: string_type_from_field(&field),
                    vec_type: generic_from_vec(&field.ty),
                }
            })
            .collect::<Vec<_>>(),
        _ => unimplemented!(),
    }
}

fn is_path_generic(path: &Path, container: &str) -> bool {
    path.segments.len() == 1 && path.segments.last().unwrap().ident == container
}

fn is_path_vec(path: &Path) -> bool {
    is_path_generic(path, "Vec")
}

fn generic_from_vec(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(TypePath { qself, path }) if qself.is_none() && is_path_vec(&path) => {
            let path_arguments = &path.segments.last().unwrap().arguments;
            let generic_arg = match path_arguments {
                PathArguments::AngleBracketed(params) => params.args.last().unwrap(),
                _ => return None,
            };
            match generic_arg {
                GenericArgument::Type(ty) => Some(ty.clone()),
                _ => return None,
            }
        }
        _ => return None,
    }
}

fn last_segment_from_type(ty: &Type) -> Option<Ident> {
    match ty {
        Type::Path(TypePath { path, .. }) => Some(path.segments.last()?.ident.clone()),
        _ => None,
    }
}
