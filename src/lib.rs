#![allow(unused)]
pub use rizzle_macros::*;
pub use sqlite::{DataValue, Database, DatabaseOptions};
use sqlx::{query_as, Arguments, Encode, Executor, IntoArguments};
pub use sqlx::{Error as SqlxError, FromRow, Row};
use std::{collections::HashSet, fmt::Display, rc::Rc, time::Duration};

#[derive(FromRow)]
struct ColumnDef {
    table_name: String,
    name: String,
    data_type: String,
    not_null: bool,
    primary_key: bool,
    default_value: String,
    fk_table: String,
    fk_from: String,
    fk_to: String,
    fk_on_update: String,
}

#[derive(Default, PartialEq, Debug)]
pub struct Column {
    pub table_name: String,
    pub default_value: Option<String>,
    pub name: String,
    pub not_null: bool,
    pub primary_key: bool,
    pub data_type: sqlite::DataType,
    pub references: Option<String>,
}

impl Column {
    pub fn definition_sql(&self) -> String {
        let not_null = match self.not_null {
            true => Some("not null".into()),
            false => None,
        };
        let primary_key = match self.primary_key {
            true => Some("primary key".into()),
            false => None,
        };
        let data_type = self.data_type.to_string();
        let default_value = match &self.default_value {
            Some(s) => Some(format!("default {}", s)),
            None => None,
        };
        let references = match &self.references {
            Some(rf) => Some(format!("references {}", rf)),
            None => None,
        };
        vec![
            Some(self.name.clone()),
            Some(data_type),
            primary_key,
            not_null,
            default_value,
            references,
        ]
        .into_iter()
        .filter_map(|s| s)
        .collect::<Vec<_>>()
        .join(" ")
    }

    pub fn add_sql(&self) -> String {
        format!(
            "alter table {} add column {};",
            self.table_name,
            self.definition_sql()
        )
    }

    pub fn full_name(&self) -> String {
        format!("{} {}", self.table_name, self.name)
    }

    pub fn drop_sql(&self) -> String {
        format!("alter table {} drop column {}", self.table_name, self.name)
    }
}

impl From<ColumnDef> for Column {
    fn from(value: ColumnDef) -> Self {
        let ColumnDef {
            table_name,
            name,
            data_type,
            not_null,
            primary_key,
            default_value,
            fk_table,
            fk_from,
            fk_to,
            fk_on_update,
        } = value;
        let default_value = match default_value.is_empty() {
            true => None,
            false => Some(default_value),
        };
        let data_type = match data_type.to_ascii_lowercase().as_ref() {
            "text" => sqlite::DataType::Text,
            "integer" => sqlite::DataType::Integer,
            "real" => sqlite::DataType::Real,
            _ => sqlite::DataType::Blob,
        };
        let references = match !fk_table.is_empty() {
            true => Some(format!("{}({})", fk_table, fk_to)),
            false => None,
        };
        Column {
            table_name,
            name,
            data_type,
            not_null,
            primary_key,
            default_value,
            references,
        }
    }
}

#[derive(Default)]
pub struct Index {
    pub table_name: String,
    pub name: String,
    pub index_type: sqlite::IndexType,
    pub column_names: String,
}

impl Index {
    pub fn create_sql(&self) -> String {
        let unique = match &self.index_type {
            sqlite::IndexType::Plain => " ",
            sqlite::IndexType::Unique => " unique ",
        };
        format!(
            "create{}index {} on {}({})",
            unique, self.name, self.table_name, self.column_names
        )
    }
}

#[derive(FromRow, Default)]
pub struct Reference {
    clause: String,
    id: i64,
    seq: i64,
    many: bool,
    table: String,
    from: String,
    to: String,
    on_update: String,
    on_delete: String,
    r#match: String,
}

pub trait RizzleSchema {
    fn new() -> Self;
    fn tables<'a>(&'a self) -> Vec<&'a dyn Table>;
    fn sql(&self) -> String;
}

pub trait Table {
    fn new() -> Self
    where
        Self: Sized;
    fn name(&self) -> String;
    fn columns(&self) -> Vec<Column>;
    fn indexes(&self) -> Vec<Index>;
    fn references(&self) -> Vec<Reference>;
    fn create_sql(&self) -> String;
}

pub mod sqlite {
    use super::*;
    pub use sqlx::{
        sqlite::{
            SqliteConnectOptions as ConnectOptions, SqliteJournalMode as JournalMode,
            SqlitePoolOptions as PoolOptions, SqliteQueryResult as QueryResult, SqliteRow,
            SqliteSynchronous as Synchronous,
        },
        Sqlite as Driver,
    };
    use sqlx::{Execute, QueryBuilder, SqlitePool as Pool, Type};
    pub type Integer = &'static str;
    pub type Text = &'static str;
    pub type Blob = &'static str;
    pub type Real = &'static str;
    pub type Index = &'static str;
    pub type UniqueIndex = &'static str;
    pub type Many = &'static str;

    #[derive(Default, PartialEq, Debug)]
    pub enum DataType {
        #[default]
        Blob,
        Integer,
        Real,
        Text,
    }

    #[derive(Debug, Clone)]
    pub enum DataValue {
        Blob(Vec<u8>),
        Integer(i64),
        Real(f64),
        Text(String),
    }

    impl From<i64> for DataValue {
        fn from(value: i64) -> Self {
            Self::Integer(value)
        }
    }

    impl From<&str> for DataValue {
        fn from(value: &str) -> Self {
            Self::Text(value.to_owned())
        }
    }

    impl From<String> for DataValue {
        fn from(value: String) -> Self {
            Self::Text(value)
        }
    }

    impl From<f64> for DataValue {
        fn from(value: f64) -> Self {
            Self::Real(value)
        }
    }

    impl From<Vec<u8>> for DataValue {
        fn from(value: Vec<u8>) -> Self {
            Self::Blob(value)
        }
    }

    impl std::fmt::Display for DataType {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let result = match self {
                DataType::Blob => "blob",
                DataType::Integer => "integer",
                DataType::Real => "real",
                DataType::Text => "text",
            };
            f.write_str(result)
        }
    }

    #[derive(Default, PartialEq, Debug)]
    pub enum IndexType {
        #[default]
        Plain,
        Unique,
    }

    #[derive(Clone, Default, Debug)]
    pub struct DatabaseOptions {
        connection_string: String,
        max_connection: u32,
        create_if_missing: bool,
        journal_mode: JournalMode,
        synchronous: Synchronous,
        busy_timeout: Duration,
    }

    impl DatabaseOptions {
        pub fn new(filename: &str) -> Self {
            Self {
                connection_string: filename.to_owned(),
                max_connection: 10,
                create_if_missing: true,
                journal_mode: JournalMode::Wal,
                synchronous: Synchronous::Normal,
                busy_timeout: Duration::from_secs(30),
            }
        }

        pub fn max_connections(mut self, max: u32) -> Self {
            self.max_connection = max;
            self
        }

        pub fn create_if_missing(mut self, create: bool) -> Self {
            self.create_if_missing = create;
            self
        }

        pub fn journal_mode(mut self, mode: JournalMode) -> Self {
            self.journal_mode = mode;
            self
        }

        pub fn synchronous(mut self, synchronous: Synchronous) -> Self {
            self.synchronous = synchronous;
            self
        }

        pub fn busy_timeout(mut self, duration: Duration) -> Self {
            self.busy_timeout = duration;
            self
        }

        pub fn pool_options(&self) -> PoolOptions {
            PoolOptions::new().max_connections(self.max_connection)
        }

        pub fn connect_options(&self) -> ConnectOptions {
            let options: ConnectOptions = self.connection_string.parse().unwrap();
            options
                .create_if_missing(self.create_if_missing)
                .journal_mode(self.journal_mode)
                .synchronous(self.synchronous)
        }
    }

    #[derive(Clone, Debug)]
    pub struct Database {
        pub pool: Pool,
    }

    impl Database {
        pub async fn new(options: DatabaseOptions) -> Result<Self, RizzleError> {
            match Self::pool(options).await {
                Ok(pool) => Ok(Self { pool }),
                Err(err) => Err(err),
            }
        }

        pub async fn connect(filename: &str) -> Result<Self, RizzleError> {
            let options = DatabaseOptions::new(filename);
            match Self::pool(options).await {
                Ok(pool) => Ok(Self { pool }),
                Err(err) => Err(err),
            }
        }

        async fn pool(options: DatabaseOptions) -> Result<Pool, RizzleError> {
            let maybe_pool = options
                .pool_options()
                .connect_with(options.connect_options())
                .await;
            match maybe_pool {
                Ok(p) => Ok(p),
                Err(e) => {
                    return Err(RizzleError::PoolClosed);
                }
            }
        }

        pub async fn table_names(&self) -> Vec<TableName> {
            let sql = "select name from sqlite_schema where type = 'table'";
            match query_as(sql).fetch_all(&self.pool).await {
                Ok(table_names) => table_names,
                Err(_) => vec![],
            }
        }

        pub async fn index_names(&self) -> Vec<IndexName> {
            let sql = "select name from sqlite_schema where type = 'index'";
            match query_as(sql).fetch_all(&self.pool).await {
                Ok(names) => names,
                Err(_) => vec![],
            }
        }

        pub async fn columns(&self) -> Vec<Column> {
            let sql = r#"
                select
                    s.name as table_name,
                    pti.name as name,
                    pti.type as data_type,
                    pti.pk as primary_key,
                    pti.dflt_value as default_value,
                    pti.[notnull] as not_null,
                    fkl.[table] as fk_table,
                    fkl.[from] as fk_from,
                    fkl.[to] as fk_to,
                    fkl.[on_update] as fk_on_update,
                    fkl.[on_delete] as fk_on_delete
                from sqlite_schema s
                left outer join pragma_table_info((s.name)) pti on pti.name <> s.name
                left outer join pragma_foreign_key_list((s.name)) fkl on fkl.[table] <> s.name and fkl.[from] = pti.name
                where s.type = 'table';
            "#;
            let column_defs: Vec<ColumnDef> =
                sqlx::query_as(sql).fetch_all(&self.pool).await.unwrap();
            column_defs
                .into_iter()
                .map(|cd| cd.into())
                .collect::<Vec<_>>()
        }

        async fn references(&self) -> Vec<Reference> {
            vec![]
        }

        pub async fn schema_sql(&self) -> String {
            sqlx::query("select * from sqlite_schema")
                .fetch_all(&self.pool)
                .await
                .unwrap()
                .iter()
                .map(|row| row.get::<String, usize>(0))
                .collect::<Vec<_>>()
                .join("\n")
        }

        async fn execute(&self, sql: &str) -> Result<(), sqlx::Error> {
            let _ = &self.pool.execute(sql).await?;
            Ok(())
        }

        /// sync given a vector of impl Table will migrate the database to a new state
        pub async fn sync(&self, tables: Vec<&dyn Table>) -> Result<(), RizzleError> {
            // 6 parts:
            // create tables
            // drop tables
            // add columns
            // drop columns
            // create indexes
            // drop indexes

            let table_names = self.table_names().await;
            let create_tables_sql = create_tables_sql(&table_names, &tables);
            let _ = self.execute(&create_tables_sql).await?;

            let table_names = self.table_names().await;
            let drop_tables_sql = drop_tables_sql(&table_names, &tables);
            let _ = self.execute(&drop_tables_sql).await?;

            let db_columns = self.columns().await;
            let new_columns: Vec<_> = tables.iter().flat_map(|t| t.columns()).collect();
            let columns_to_add = columns_to_add(db_columns, new_columns);
            let add_columns_sql = add_columns_sql(columns_to_add);
            let _ = self.execute(&add_columns_sql).await?;

            let db_columns = self.columns().await;
            let new_columns: Vec<_> = tables.iter().flat_map(|t| t.columns()).collect();
            let drop_columns_sql = drop_columns_sql(db_columns, new_columns);
            let _ = self.execute(&drop_columns_sql).await?;

            let index_names = self
                .index_names()
                .await
                .into_iter()
                .map(|idx| idx.0)
                .collect::<Vec<_>>();
            let create_indexes_sql = create_indexes_sql(&tables, index_names);
            let _ = self.execute(&create_indexes_sql).await?;

            let index_names = self
                .index_names()
                .await
                .into_iter()
                .map(|idx| idx.0)
                .collect::<Vec<_>>();
            let drop_indexes_sql = drop_indexes_sql(tables, index_names);
            let _ = self.execute(&drop_indexes_sql).await?;

            Ok(())
        }

        pub fn select(&self) -> Query {
            Query::new(self.pool.clone()).select()
        }

        pub fn select_with(&self, select: impl Select) -> Query {
            Query::new(self.pool.clone()).select_with(select)
        }

        pub fn insert(&self, insert: impl Table) -> Query {
            Query::new(self.pool.clone()).insert(insert.name())
        }

        pub fn update(&self, update: impl Table) -> Query {
            Query::new(self.pool.clone()).update(update.name())
        }

        pub fn delete(&self, delete: impl Table) -> Query {
            Query::new(self.pool.clone()).delete(delete.name())
        }

        pub fn pull(&self, st: impl Pull) -> Query {
            Query::new(self.pool.clone()).pull(st)
        }
    }

    pub struct Bind {
        sql: String,
        value: DataValue,
    }

    pub struct PreparedAs<'a, T>
    where
        T: for<'r> FromRow<'r, sqlx::sqlite::SqliteRow> + Send + Unpin,
    {
        pool: Pool,
        prepared: sqlx::query::QueryAs<'a, Driver, T, sqlx::sqlite::SqliteArguments<'a>>,
    }

    impl<'a, T> PreparedAs<'a, T>
    where
        T: for<'r> FromRow<'r, sqlx::sqlite::SqliteRow> + Send + Unpin,
    {
        pub async fn rows_affected(self) -> Result<Vec<T>, RizzleError> {
            let result = self.prepared.fetch_all(&self.pool).await?;
            Ok(result)
        }

        pub async fn all(self) -> Result<Vec<T>, RizzleError>
        where
            T: for<'r> FromRow<'r, sqlx::sqlite::SqliteRow> + Send + Unpin,
        {
            let result = self.prepared.fetch_all(&self.pool).await?;
            Ok(result)
        }
    }

    pub struct Prepared<'a> {
        pool: Pool,
        prepared: sqlx::query::Query<'a, Driver, sqlx::sqlite::SqliteArguments<'a>>,
    }

    impl<'a> Prepared<'a> {
        pub async fn rows_affected(self) -> Result<u64, RizzleError> {
            let result = self.prepared.execute(&self.pool).await?.rows_affected();
            Ok(result)
        }

        pub async fn last_insert_rowid(self) -> Result<i64, RizzleError> {
            let result = self.prepared.execute(&self.pool).await?.last_insert_rowid();
            Ok(result)
        }
    }

    pub struct Query {
        pool: Pool,
        sql: String,
        values: Vec<DataValue>,
    }

    impl Query {
        fn new(pool: Pool) -> Self {
            Self {
                pool,
                sql: String::new(),
                values: vec![],
            }
        }

        fn push(&mut self, sql: impl Display) {
            self.sql.push_str(&format!("{} ", sql));
        }

        fn push_bind(&mut self, bind: Bind) {
            self.sql.push_str(&format!("{} ", bind.sql));
            self.values.push(bind.value);
        }

        pub fn select(mut self) -> Self {
            self.push("select *");
            self
        }

        pub fn select_with(mut self, sel: impl Select) -> Self {
            self.push(sel.select_sql());
            self
        }

        pub fn from(mut self, table: impl Table) -> Self {
            let sql = format!("from {}", table.name());
            self.push(sql);
            self
        }

        pub fn r#where(mut self, bind: Bind) -> Self {
            self.push("where");
            self.push_bind(bind);
            self
        }

        pub fn inner_join(mut self, table: impl Table, sql: String) -> Self {
            self.push(format!("inner join {} on {}", table.name(), sql));
            self
        }

        pub fn left_join(mut self, table: impl Table, sql: String) -> Self {
            self.push(format!("left join {} on {}", table.name(), sql));
            self
        }

        pub fn right_join(mut self, table: impl Table, sql: String) -> Self {
            self.push(format!("right join {} on {}", table.name(), sql));
            self
        }

        pub fn full_join(mut self, table: impl Table, sql: String) -> Self {
            self.push(format!("full join {} on {}", table.name(), sql));
            self
        }

        pub fn limit(mut self, num: u64) -> Self {
            self.push(format!("limit {}", num));
            self
        }

        pub fn offset(mut self, num: u64) -> Self {
            self.push(format!("offset {}", num));
            self
        }

        pub fn order_by(mut self, sql: String) -> Self {
            self.push(format!("order by {}", sql));
            self
        }

        pub fn sql(&self) -> String {
            self.sql.trim_end().to_owned()
        }

        pub fn insert(mut self, table_name: String) -> Self {
            let sql = format!("insert into {} ", table_name.as_str());
            self.sql.push_str(sql.as_str());
            self
        }

        pub fn values(mut self, insert: impl Insert) -> Self {
            self.push(insert.insert_sql());
            self.values.extend(insert.insert_values().into_iter());
            self
        }

        pub async fn execute(&self) -> Result<QueryResult, RizzleError> {
            let sql = self.sql();
            let mut query = sqlx::query(&sql);
            for value in &self.values {
                query = match value {
                    DataValue::Blob(b) => query.bind(b),
                    DataValue::Integer(integer) => query.bind(integer),
                    DataValue::Real(real) => query.bind(real),
                    DataValue::Text(text) => query.bind(text),
                };
            }
            let result = query.execute(&self.pool).await?;
            Ok(result)
        }

        fn build<'a>(
            &'a self,
        ) -> sqlx::query::Query<'a, Driver, sqlx::sqlite::SqliteArguments<'a>> {
            let mut query = sqlx::query::<Driver>(&self.sql);
            for value in &self.values {
                query = match value {
                    DataValue::Blob(b) => query.bind(b),
                    DataValue::Integer(integer) => query.bind(integer),
                    DataValue::Real(real) => query.bind(real),
                    DataValue::Text(text) => query.bind(text),
                };
            }
            query
        }

        fn build_as<'a, T>(
            &'a self,
        ) -> sqlx::query::QueryAs<Driver, T, sqlx::sqlite::SqliteArguments<'a>>
        where
            T: for<'r> FromRow<'r, sqlx::sqlite::SqliteRow>,
        {
            let mut query = sqlx::query_as::<Driver, T>(&self.sql);
            for value in &self.values {
                query = match value {
                    DataValue::Blob(b) => query.bind(b),
                    DataValue::Integer(integer) => query.bind(integer),
                    DataValue::Real(real) => query.bind(real),
                    DataValue::Text(text) => query.bind(text),
                };
            }
            query
        }

        pub fn prepare_as<T>(&self) -> PreparedAs<T>
        where
            T: for<'r> FromRow<'r, sqlx::sqlite::SqliteRow> + Send + Unpin,
        {
            let pool = self.pool.clone();
            let prepared = self.build_as::<T>();
            PreparedAs { pool, prepared }
        }

        pub fn prepare(&self) -> Prepared {
            let pool = self.pool.clone();
            let prepared = self.build();
            Prepared { pool, prepared }
        }

        pub async fn rows_affected(&self) -> Result<u64, RizzleError> {
            Ok(self.execute().await?.rows_affected())
        }

        pub async fn last_insert_rowid(&self) -> Result<i64, RizzleError> {
            Ok(self.execute().await?.last_insert_rowid())
        }

        pub async fn all<T>(&self) -> Result<Vec<T>, RizzleError>
        where
            T: for<'r> FromRow<'r, sqlx::sqlite::SqliteRow> + Send + Unpin,
        {
            Ok(self.build_as::<T>().fetch_all(&self.pool).await?)
        }

        pub async fn returning<T>(mut self) -> Result<T, RizzleError>
        where
            T: for<'r> FromRow<'r, sqlx::sqlite::SqliteRow> + Send + Unpin,
        {
            self.push("returning *");
            let rows = self.build_as::<T>().fetch_all(&self.pool).await?;
            Ok(rows.into_iter().nth(0).ok_or(RizzleError::RowNotFound)?)
        }

        pub fn update(mut self, table_name: String) -> Self {
            self.push(format!("update {}", table_name));
            self
        }

        pub fn set(mut self, table: impl Update) -> Self {
            self.push(table.update_sql());
            self.values.extend(table.update_values());
            self
        }

        pub fn delete(mut self, table_name: String) -> Self {
            self.push(format!("delete from {}", table_name));
            self
        }

        fn pull(mut self, st: impl Pull) -> Query {
            self.push(format!("select {} as '__pull__'", st.json_object_sql()));
            self
        }
    }

    pub fn eq(left: &str, right: impl Into<DataValue>) -> Bind {
        Bind {
            sql: format!("{} = ?", left),
            value: right.into(),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum RizzleError {
    #[error("database error: `{0}`")]
    Database(String),
    #[error("database connection error")]
    Connection,
    #[error("database pool closed")]
    PoolClosed,
    #[error("row not found")]
    RowNotFound,
}

impl From<sqlx::Error> for RizzleError {
    fn from(value: sqlx::Error) -> Self {
        match value {
            sqlx::Error::Configuration(_) => todo!(),
            sqlx::Error::Database(err) => RizzleError::Database(err.message().to_owned()),
            sqlx::Error::Io(_) => todo!(),
            sqlx::Error::Tls(_) => todo!(),
            sqlx::Error::Protocol(_) => todo!(),
            sqlx::Error::RowNotFound => RizzleError::RowNotFound,
            sqlx::Error::TypeNotFound { type_name } => todo!(),
            sqlx::Error::ColumnIndexOutOfBounds { index, len } => todo!(),
            sqlx::Error::ColumnNotFound(e) => RizzleError::Database(e),
            sqlx::Error::ColumnDecode { index, source } => todo!(),
            sqlx::Error::Decode(_) => todo!(),
            sqlx::Error::AnyDriverError(_) => todo!(),
            sqlx::Error::PoolTimedOut => todo!(),
            sqlx::Error::PoolClosed => RizzleError::PoolClosed,
            sqlx::Error::WorkerCrashed => todo!(),
            sqlx::Error::Migrate(_) => todo!(),
            _ => unimplemented!(),
        }
    }
}

#[derive(FromRow, Debug)]
pub struct TableName(String);

#[derive(FromRow, Debug)]
pub struct IndexName(String);

pub trait New {
    fn new() -> Self;
}

pub trait Select {
    fn select_sql(&self) -> String;
}

pub trait Pull {
    fn json_object_sql(&self) -> String;
}

pub trait Insert {
    fn insert_values(&self) -> Vec<DataValue>;
    fn insert_sql(&self) -> String;
}

pub trait Update {
    fn update_values(&self) -> Vec<DataValue>;
    fn update_sql(&self) -> String;
}

fn on(left: &str, right: &str) -> String {
    format!("{} = {}", left, right)
}

macro_rules! asc {
    ($($columns:tt)*) => {{
        let cols: Vec<&str> = vec![$($columns)*];
        cols.iter().map(|c| c.to_string()).collect::<Vec<_>>().join(", ")
    }}
}

macro_rules! desc {
    ($($columns:tt)*) => {{
        let cols: Vec<&str> = vec![$($columns)*];
        cols.iter().map(|c| format!("{} desc", c)).collect::<Vec<_>>().join(", ")
    }}
}

fn drop_indexes_sql(tables: Vec<&dyn Table>, index_names: Vec<String>) -> String {
    let index_arr = tables
        .into_iter()
        .flat_map(|t| t.indexes())
        .map(|i| i.name)
        .collect::<Vec<_>>();
    index_names
        .iter()
        .filter(|name| !index_arr.contains(&name))
        .map(|i| format!("drop index {}", i))
        .collect::<Vec<_>>()
        .join(";")
}

fn create_indexes_sql(tables: &Vec<&dyn Table>, index_names: Vec<String>) -> String {
    tables
        .iter()
        .flat_map(|t| t.indexes())
        .filter(|i| !index_names.contains(&i.name))
        .map(|i| i.create_sql())
        .collect::<Vec<_>>()
        .join(";")
}

fn create_tables_sql(table_names: &Vec<TableName>, tables: &Vec<&dyn Table>) -> String {
    let table_names = table_names.into_iter().map(|tn| &tn.0).collect::<Vec<_>>();
    tables
        .iter()
        .filter(|t| !table_names.contains(&&t.name()))
        .map(|table| table.create_sql())
        .collect::<Vec<_>>()
        .join(";")
}

fn drop_tables_sql(table_names: &Vec<TableName>, tables: &Vec<&dyn Table>) -> String {
    let mig_table_names = tables.iter().map(|table| table.name()).collect::<Vec<_>>();
    table_names
        .iter()
        .filter(|table_name| !mig_table_names.contains(&table_name.0))
        .map(|t| format!("drop table {};", t.0))
        .collect::<Vec<_>>()
        .join("")
}

fn columns_to_add(db_columns: Vec<Column>, code_columns: Vec<Column>) -> Vec<Column> {
    let db_column_names = db_columns.iter().map(|c| c.full_name()).collect::<Vec<_>>();
    let code_column_names = &code_columns
        .iter()
        .map(|c| c.full_name())
        .collect::<Vec<_>>();
    let result = code_columns
        .into_iter()
        .filter(|c| !db_column_names.contains(&c.full_name()))
        .collect();
    result
}

fn add_columns_sql(columns: Vec<Column>) -> String {
    columns
        .iter()
        .map(|c| c.add_sql())
        .collect::<Vec<_>>()
        .join("\n")
}

fn drop_columns_sql(db_columns: Vec<Column>, new_columns: Vec<Column>) -> String {
    db_columns
        .iter()
        .filter(|col| !new_columns.contains(&col))
        .map(|col| col.drop_sql())
        .collect::<Vec<_>>()
        .join(";")
}

pub async fn rizzle(
    options: DatabaseOptions,
    schema: impl RizzleSchema,
) -> Result<Database, RizzleError> {
    let db = Database::new(options).await?;
    let _ = db.sync(schema.tables()).await?;
    Ok(db)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sqlite::{eq, DataValue, Database, DatabaseOptions, Text};
    use rizzle_macros::{Insert, New, Pull, RizzleSchema, Row, Select, Table, Update};
    use serde::de::DeserializeOwned;
    use serde::Deserialize;
    use std::sync::OnceLock;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[derive(Table, Clone, Copy)]
    #[rizzle(table = "users")]
    struct Users {
        #[rizzle(primary_key)]
        id: sqlite::Integer,

        #[rizzle(not_null)]
        name: sqlite::Text,

        #[rizzle(not_null)]
        created_at: sqlite::Real,

        #[rizzle(not_null)]
        updated_at: sqlite::Real,

        #[rizzle(columns = "name")]
        name_index: sqlite::UniqueIndex,
    }

    #[derive(Table, Clone, Copy)]
    #[rizzle(table = "posts")]
    struct Posts {
        #[rizzle(primary_key)]
        id: sqlite::Integer,

        #[rizzle(not_null)]
        body: sqlite::Text,

        #[rizzle(not_null)]
        created_at: sqlite::Real,

        #[rizzle(not_null)]
        updated_at: sqlite::Real,

        #[rizzle(not_null, references = "users(id)")]
        user_id: sqlite::Integer,
    }

    #[derive(Table, Clone, Copy)]
    #[rizzle(table = "comments")]
    struct Comments {
        #[rizzle(primary_key)]
        id: sqlite::Integer,

        #[rizzle(not_null)]
        body: sqlite::Text,

        #[rizzle(references = "users(id)")]
        author_id: sqlite::Integer,

        #[rizzle(references = "posts(id)")]
        post_id: sqlite::Integer,
    }

    #[derive(RizzleSchema, Clone, Copy)]
    struct Schema {
        users: Users,
        posts: Posts,
        comments: Comments,
    }

    #[derive(Row, Default, Debug)]
    struct User {
        id: i64,
        name: String,
        created_at: f64,
        updated_at: f64,
    }

    #[derive(Row, Default, Debug)]
    struct Post {
        id: i64,
        body: String,
        created_at: f64,
        updated_at: f64,
        user_id: i64,
    }

    #[derive(Row, Debug)]
    struct Comment {
        id: i64,
        body: String,
    }

    fn db_options() -> DatabaseOptions {
        DatabaseOptions::new("sqlite://:memory:")
    }

    fn schema() -> Schema {
        Schema::new()
    }

    async fn db() -> Database {
        rizzle(db_options(), schema()).await.unwrap()
    }

    #[tokio::test]
    async fn sync_new_tables_works() {
        let db = db().await;
        assert!(db
            .table_names()
            .await
            .iter()
            .map(|tn| &tn.0)
            .any(|name| name == "users"));
    }

    #[tokio::test]
    async fn sync_new_tables_is_idempotent_works() {
        let db0 = db().await;
        let tables_len0 = db0.table_names().await.len();
        let db1 = db().await;
        let tables_len1 = db0.table_names().await.len();
        assert_eq!(tables_len0, tables_len1)
    }

    #[tokio::test]
    async fn sync_with_dropped_tables_works() {
        let db = db().await;
        let u = Users::new();
        let p = Posts::new();
        db.sync(vec![&u, &p]).await;
        assert_eq!(2, db.table_names().await.len());
        db.sync(vec![&u]).await;
        assert_eq!(1, db.table_names().await.len());
    }

    #[tokio::test]
    async fn sync_with_added_columns_works() -> Result<(), RizzleError> {
        #[derive(Table, Clone, Copy)]
        #[rizzle(table = "table_a")]
        struct A {
            a: sqlite::Text,
        }

        #[derive(Table, Clone, Copy)]
        #[rizzle(table = "table_b")]
        struct B {
            #[rizzle(primary_key)]
            id: sqlite::Integer,
        }

        #[derive(Table, Clone, Copy)]
        #[rizzle(table = "table_a")]
        struct A2 {
            a: sqlite::Text,
            #[rizzle(not_null)]
            b: sqlite::Text,
        }

        #[derive(RizzleSchema, Clone, Copy)]
        struct Schema {
            a: A,
            b: B,
        }

        #[derive(RizzleSchema, Clone, Copy)]
        struct NewSchema {
            a: A2,
            b: B,
        }

        let schema = Schema::new();
        let new_schema = NewSchema::new();
        let db0 = rizzle(db_options(), schema).await?;
        let db1 = rizzle(db_options(), new_schema).await?;
        assert_eq!(2, db0.columns().await.len());
        assert_eq!(3, db1.columns().await.len());
        Ok(())
    }

    #[test]
    fn rizzle_table_name_works() {
        let users = Users::new();
        assert!(users.create_sql().contains("create table users"))
    }

    #[tokio::test]
    async fn drop_table_works() -> Result<(), RizzleError> {
        #[derive(Table)]
        #[rizzle(table = "a")]
        struct A {
            id: sqlite::Blob,
        }

        #[derive(Table)]
        #[rizzle(table = "b")]
        struct B {
            id: sqlite::Blob,
        }

        #[derive(RizzleSchema)]
        struct Schema {
            a: A,
            b: B,
        }

        #[derive(RizzleSchema)]
        struct NewSchema {
            a: A,
        }

        let db0 = rizzle(db_options(), Schema::new()).await?;
        let db1 = rizzle(db_options(), NewSchema::new()).await?;

        assert_eq!(2, db0.table_names().await.len());
        assert_eq!(1, db1.table_names().await.len());

        Ok(())
    }

    #[tokio::test]
    async fn create_indexes_works() -> Result<(), RizzleError> {
        #[derive(Table)]
        #[rizzle(table = "index_table")]
        struct I {
            #[rizzle(not_null)]
            a: sqlite::Text,

            #[rizzle(not_null)]
            b: sqlite::Text,

            #[rizzle(columns = "a,b")]
            a_b_index: sqlite::UniqueIndex,
        }

        #[derive(RizzleSchema)]
        struct Schema {
            i: I,
        }

        let db0 = rizzle(db_options(), Schema::new()).await?;
        assert_eq!(1, db0.index_names().await.len());

        assert_eq!(
            vec!["a_b_index".to_owned()],
            db0.index_names()
                .await
                .into_iter()
                .map(|ind| ind.0)
                .collect::<Vec<_>>()
        );

        Ok(())
    }

    #[tokio::test]
    async fn drop_indexes_works() -> Result<(), RizzleError> {
        #[derive(Table)]
        #[rizzle(table = "index_table")]
        struct I {
            #[rizzle(not_null)]
            a: sqlite::Text,

            #[rizzle(not_null)]
            b: sqlite::Text,

            #[rizzle(columns = "a,b")]
            a_b_index: sqlite::UniqueIndex,
        }

        #[derive(RizzleSchema)]
        struct Schema {
            i: I,
        }

        let db0 = rizzle(db_options(), Schema::new()).await?;
        assert_eq!(1, db0.index_names().await.len());

        #[derive(Table)]
        #[rizzle(table = "index_table")]
        struct I2 {
            #[rizzle(not_null)]
            a: sqlite::Text,

            #[rizzle(not_null)]
            b: sqlite::Text,
        }

        #[derive(RizzleSchema)]
        struct NewSchema {
            i: I2,
        }

        let db1 = rizzle(db_options(), NewSchema::new()).await?;

        assert_eq!(0, db1.index_names().await.len());

        Ok(())
    }

    #[tokio::test]
    async fn drop_columns_works() -> Result<(), RizzleError> {
        #[derive(Table)]
        #[rizzle(table = "a")]
        struct A {
            col1: sqlite::Text,
            col2: sqlite::Text,
        }

        #[derive(Table)]
        #[rizzle(table = "a")]
        struct A2 {
            col1: sqlite::Text,
        }

        #[derive(RizzleSchema)]
        struct Schema {
            a: A,
        }

        #[derive(RizzleSchema)]
        struct NewSchema {
            a: A2,
        }

        let db0 = rizzle(db_options(), Schema::new()).await?;
        let db1 = rizzle(db_options(), NewSchema::new()).await?;
        assert_eq!(2, db0.columns().await.len());
        assert_eq!(1, db1.columns().await.len());

        Ok(())
    }

    #[test]
    fn create_table_sql_works() {
        let users = Users::new();
        let posts = Posts::new();
        assert_eq!("create table users (id integer primary key, name text not null, created_at real not null, updated_at real not null)", users.create_sql());
        assert_eq!("create table posts (id integer primary key, body text not null, created_at real not null, updated_at real not null, user_id integer not null references users(id))", posts.create_sql())
    }

    #[tokio::test]
    async fn select_star_works() {
        let db = db().await;
        let users = Users::new();
        assert_eq!("select * from users", db.select().from(users).sql())
    }

    #[tokio::test]
    async fn partial_select_works() -> Result<(), RizzleError> {
        let Schema { users, .. } = Schema::new();
        let db = db().await;

        #[derive(New, Row)]
        struct PartialUser {
            id: i64,
            name: String,
        }

        let partial_user = PartialUser::new();
        let query = db.select_with(partial_user).from(users);

        assert_eq!("select id, name from users", query.sql());
        let partial_users: Vec<PartialUser> = query.all().await?;
        assert_eq!(partial_users.len(), 0);
        Ok(())
    }

    #[tokio::test]
    async fn inner_join_works() -> Result<(), RizzleError> {
        let schema = Schema::new();
        let db = rizzle(db_options(), schema).await?;
        let Schema { users, posts, .. } = schema;
        let rows = db
            .insert(users)
            .values(User {
                id: 1,
                ..Default::default()
            })
            .rows_affected()
            .await?;
        let rows = db
            .insert(posts)
            .values(Post {
                id: 1,
                user_id: 1,
                ..Default::default()
            })
            .rows_affected()
            .await?;
        let rows: Vec<Post> = db
            .select()
            .from(posts)
            .inner_join(users, on(users.id, posts.user_id))
            .all()
            .await?;
        assert_eq!(1, rows.first().unwrap().user_id);
        Ok(())
    }

    #[test]
    fn asc_works() {
        let users = Users::new();
        assert_eq!("users.id, users.name", asc!(users.id, users.name))
    }

    #[tokio::test]
    async fn order_by_works() {
        let db = db().await;
        let users = Users::new();
        assert_eq!(
            "select * from users order by users.id desc, users.name desc",
            db.select()
                .from(users)
                .order_by(desc!(users.id, users.name))
                .sql()
        )
    }

    #[tokio::test]
    async fn insert_one_row_works() -> Result<(), RizzleError> {
        #[derive(Insert, Default)]
        struct NewUser {
            name: String,
            created_at: f64,
            updated_at: f64,
        }

        let schema = schema();
        let db = rizzle(db_options(), schema).await?;
        let Schema { users, .. } = schema;
        let new_user = NewUser::default();
        let rows_affected = db.insert(users).values(new_user).rows_affected().await?;
        assert_eq!(1, rows_affected);

        Ok(())
    }

    #[tokio::test]
    async fn insert_one_row_with_returning_works() -> Result<(), RizzleError> {
        #[derive(Insert, Default)]
        struct NewUser {
            name: String,
            created_at: f64,
            updated_at: f64,
        }

        let schema = schema();
        let db = rizzle(db_options(), schema).await?;
        let Schema { users, .. } = schema;
        let new_user = NewUser::default();
        let user = db
            .insert(users)
            .values(new_user)
            .returning::<User>()
            .await?;
        assert_eq!(1, user.id);
        assert_eq!("", user.name);
        Ok(())
    }

    #[tokio::test]
    async fn where_with_equals_works() -> Result<(), RizzleError> {
        #[derive(Insert, Default)]
        struct NewUser {
            name: String,
            created_at: f64,
            updated_at: f64,
        }

        let schema = schema();
        let db = rizzle(db_options(), schema).await?;
        let Schema { users, .. } = schema;
        let _ = db
            .insert(users)
            .values(NewUser::default())
            .rows_affected()
            .await?;
        let query = db.select().from(users).r#where(eq(users.id, 1));
        let users = query.all::<User>().await?;
        assert_eq!(1, users.len());
        Ok(())
    }

    fn now() -> f64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
    }

    #[tokio::test]
    async fn update_works() -> Result<(), RizzleError> {
        #[derive(Insert, Default)]
        struct NewUser {
            name: String,
            created_at: f64,
            updated_at: f64,
        }

        let schema = schema();
        let db = rizzle(db_options(), schema).await?;
        let Schema { users, .. } = schema;

        let user = db
            .insert(users)
            .values(NewUser::default())
            .returning::<User>()
            .await?;

        assert_eq!(user.name, "");

        let updated_user = db
            .update(users)
            .set(User {
                name: "new name".to_owned(),
                updated_at: now(),
                ..user
            })
            .r#where(eq(users.id, 1))
            .returning::<User>()
            .await?;

        assert_eq!(updated_user.id, user.id);
        assert_eq!(updated_user.name, "new name");

        Ok(())
    }

    #[tokio::test]
    async fn delete_works() -> Result<(), RizzleError> {
        #[derive(Insert, Default)]
        struct NewUser {
            name: String,
            created_at: f64,
            updated_at: f64,
        }

        let schema = schema();
        let db = rizzle(db_options(), schema).await?;
        let Schema { users, .. } = schema;

        let user = db
            .insert(users)
            .values(NewUser::default())
            .returning::<User>()
            .await?;
        let user_rows: Vec<User> = db.select().from(users).all().await?;
        assert_eq!(user_rows.len(), 1);
        let deleted_rows = db
            .delete(users)
            .r#where(eq(users.id, user.id))
            .rows_affected()
            .await?;
        assert_eq!(deleted_rows, 1);
        let user_rows: Vec<User> = db.select().from(users).all().await?;
        assert_eq!(user_rows.len(), 0);
        Ok(())
    }

    #[tokio::test]
    async fn derive_row_for_basic_crud() -> Result<(), RizzleError> {
        let schema = schema();
        let db = rizzle(db_options(), schema).await?;
        let Schema {
            users,
            posts,
            comments,
        } = schema;

        let inserted_comment: Comment = db
            .insert(comments)
            .values(Comment {
                id: 1,
                body: "".to_owned(),
            })
            .returning()
            .await?;
        assert_eq!(inserted_comment.id, 1);
        assert_eq!(inserted_comment.body, "");
        let comment_rows: Vec<Comment> = db.select().from(comments).all().await?;
        assert_eq!(comment_rows.len(), 1);
        let updated_comment: Comment = db
            .update(comments)
            .set(Comment {
                body: "comment".to_owned(),
                ..inserted_comment
            })
            .returning()
            .await?;
        assert_eq!(updated_comment.id, 1);
        assert_eq!(updated_comment.body, "comment");
        let comment_rows: Vec<Comment> = db.select().from(comments).all().await?;
        assert_eq!(comment_rows.len(), 1);
        let deleted_comment: Comment = db
            .delete(comments)
            .r#where(eq(comments.id, 1))
            .returning()
            .await?;
        assert_eq!(deleted_comment.id, 1);
        let comment_rows: Vec<Comment> = db.select().from(comments).all().await?;
        assert_eq!(comment_rows.len(), 0);
        Ok(())
    }

    #[tokio::test]
    async fn prepare_works() -> Result<(), RizzleError> {
        let schema = schema();
        let db = rizzle(db_options(), schema).await?;
        let Schema {
            users,
            posts,
            comments,
        } = schema;

        let insert = db.insert(comments).values(Comment {
            id: 1,
            body: "".to_owned(),
        });
        let prepared_insert = insert.prepare();
        let rows_affected = prepared_insert.rows_affected().await?;
        assert_eq!(1, rows_affected);
        let query = db.select().from(comments);
        let prepared = query.prepare_as::<Comment>();
        let rows = prepared.all().await?;
        Ok(())
    }

    #[tokio::test]
    async fn pull_sql_works() -> Result<(), RizzleError> {
        #[derive(Pull, Deserialize, Default)]
        struct PullUser {
            id: i64,
            body: String,
        }
        let users = Users::new();
        let db = Database::connect("sqlite://:memory:").await?;
        let sql = db.pull(PullUser::default()).from(users).sql();
        assert_eq!(
            "select json_object('id', id, 'body', body) as '__pull__' from users",
            sql
        );
        Ok(())
    }

    #[tokio::test]
    async fn pull_top_level_works() -> Result<(), RizzleError> {
        #[derive(Pull, Deserialize, Default)]
        struct PullUser {
            id: i64,
            name: String,
        }
        let schema = schema();
        let db = rizzle(db_options(), schema).await?;
        let Schema { users, .. } = schema;

        let _ = db
            .insert(users)
            .values(User {
                id: 1,
                name: "name".to_string(),
                created_at: 0.0,
                updated_at: 0.0,
            })
            .rows_affected()
            .await?;
        let rows: Vec<PullUser> = db.pull(PullUser::default()).from(users).all().await?;
        assert_eq!(rows.len(), 1);
        assert_eq!(rows.first().unwrap().id, 1);
        assert_eq!(rows.first().unwrap().name, "name");
        Ok(())
    }

    #[tokio::test]
    async fn pull_one_level_vec_sql_works() -> Result<(), RizzleError> {
        #[derive(Pull, Deserialize, Default)]
        struct PullPost {
            id: i64,
            body: String,
        }

        #[derive(Pull, Deserialize, Default)]
        struct PullUser {
            id: i64,
            name: String,
            #[rizzle(many = "posts", from = "posts.user_id", to = "users.id")]
            posts: Vec<PullPost>,
        }

        let schema = schema();
        let db = rizzle(db_options(), schema).await?;
        let Schema { users, posts, .. } = schema;

        let user: User = db
            .insert(users)
            .values(User {
                id: 1,
                name: "user1".to_owned(),
                ..Default::default()
            })
            .returning()
            .await?;

        let post: Post = db
            .insert(posts)
            .values(Post {
                id: 1,
                body: "body1".to_owned(),
                user_id: 1,
                ..Default::default()
            })
            .returning()
            .await?;

        let post1: Post = db
            .insert(posts)
            .values(Post {
                id: 2,
                body: "body2".to_owned(),
                user_id: 1,
                ..Default::default()
            })
            .returning()
            .await?;

        let query = db.pull(PullUser::default()).from(users);
        let sql = query.sql();
        assert_eq!(
            r#"select json_object('id', id, 'name', name, 'posts', (select json_group_array(json_object('id', id, 'body', body)) from posts where posts.user_id = users.id)) as '__pull__' from users"#,
            sql
        );

        let rows: Vec<PullUser> = query.all().await?;
        let row = rows.first().unwrap();
        let posts = &row.posts;
        let pull_post1 = row.posts.first().unwrap();
        let pull_post2 = row.posts.iter().nth(1).unwrap();
        assert_eq!(row.id, user.id);
        assert_eq!(row.name, user.name);
        assert_eq!(posts.len(), 2);
        assert_eq!(pull_post1.id, post.id);
        assert_eq!(pull_post2.id, post1.id);
        assert_eq!(pull_post1.body, post.body);
        assert_eq!(pull_post2.body, post1.body);

        Ok(())
    }

    #[tokio::test]
    async fn pull_one_works() -> Result<(), RizzleError> {
        #[derive(Pull, Deserialize, Default)]
        struct PullPost {
            id: i64,
            body: String,
            #[rizzle(one = "users", from = "users.id", to = "posts.id")]
            user: PullUser,
        }

        #[derive(Pull, Deserialize, Default)]
        struct PullUser {
            id: i64,
            name: String,
        }

        let schema = schema();
        let db = rizzle(db_options(), schema).await?;
        let Schema { users, posts, .. } = schema;

        let user: User = db
            .insert(users)
            .values(User {
                id: 1,
                name: "user1".to_owned(),
                ..Default::default()
            })
            .returning()
            .await?;

        let post: Post = db
            .insert(posts)
            .values(Post {
                id: 1,
                body: "body1".to_owned(),
                user_id: 1,
                ..Default::default()
            })
            .returning()
            .await?;

        let query = db.pull(PullPost::default()).from(posts);
        let sql = query.sql();
        assert_eq!(
            r#"select json_object('id', id, 'body', body, 'user', (select json_object('id', id, 'name', name) from users where users.id = posts.id)) as '__pull__' from posts"#,
            sql
        );

        let rows: Vec<PullPost> = query.all().await?;
        let row = rows.first().unwrap();
        let user = &row.user;
        assert_eq!(row.user.id, user.id);
        assert_eq!(row.user.name, user.name);

        Ok(())
    }
}
