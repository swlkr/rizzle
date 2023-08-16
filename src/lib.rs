#![allow(unused)]
use sqlx::{
    query_as,
    sqlite::{SqliteConnectOptions, SqliteJournalMode, SqlitePoolOptions, SqliteSynchronous},
    Executor, FromRow, SqlitePool,
};
use std::{collections::HashSet, time::Duration};

#[derive(FromRow)]
struct ColumnDef {
    table_name: String,
    name: String,
    data_type: String,
    not_null: bool,
    primary_key: bool,
    default_value: String,
}

#[derive(Default, PartialEq, Debug)]
struct Column {
    table_name: String,
    default_value: Option<String>,
    name: String,
    not_null: bool,
    primary_key: bool,
    data_type: sqlite::DataType,
    references: Option<String>,
}

impl Column {
    fn definition_sql(&self) -> String {
        let not_null = match self.not_null {
            true => Some("not null"),
            false => None,
        };
        let primary_key = match self.primary_key {
            true => Some("primary key"),
            false => None,
        };
        let data_type = self.data_type.to_string();
        let default_value = match &self.default_value {
            Some(s) => Some(s.as_str()),
            None => None,
        };
        let references = match &self.references {
            Some(rf) => Some(format!("references {}", rf)),
            None => None,
        };
        vec![
            Some(self.name.as_ref()),
            Some(data_type.as_ref()),
            primary_key,
            not_null,
            default_value,
            references.as_deref(),
        ]
        .into_iter()
        .filter_map(|s| s)
        .collect::<Vec<_>>()
        .join(" ")
    }

    fn add_sql(&self) -> String {
        format!(
            "alter table {} add column {};",
            self.table_name,
            self.definition_sql()
        )
    }

    fn full_name(&self) -> String {
        format!("{} {}", self.table_name, self.name)
    }

    fn drop_sql(&self) -> String {
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
        } = value;
        let default_value = if default_value.is_empty() {
            None
        } else {
            Some(default_value)
        };
        let data_type = match data_type.to_ascii_lowercase().as_ref() {
            "text" => sqlite::DataType::Text,
            "integer" => sqlite::DataType::Integer,
            "real" => sqlite::DataType::Real,
            _ => sqlite::DataType::Blob,
        };
        Column {
            table_name,
            name,
            data_type,
            not_null,
            primary_key,
            default_value,
            ..Default::default()
        }
    }
}

#[derive(Default)]
struct Index {
    table_name: String,
    name: String,
    index_type: sqlite::IndexType,
    column_names: String,
}

impl Index {
    fn create_sql(&self) -> String {
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
struct Reference {
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

trait Table {
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
}

#[derive(FromRow, Debug)]
struct TableName(String);

#[derive(FromRow, Debug)]
struct IndexName(String);

#[derive(Clone)]
struct Database {
    pool: SqlitePool,
}

impl Database {
    pub async fn new(filename: String) -> Self {
        Self {
            pool: Self::pool(&filename).await,
        }
    }

    async fn pool(filename: &str) -> SqlitePool {
        SqlitePoolOptions::new()
            .max_connections(5)
            .connect_with(Self::connection_options(filename))
            .await
            .unwrap()
    }

    fn connection_options(filename: &str) -> SqliteConnectOptions {
        let options: SqliteConnectOptions = filename.parse().unwrap();
        options
            .create_if_missing(true)
            .journal_mode(SqliteJournalMode::Wal)
            .synchronous(SqliteSynchronous::Normal)
            .busy_timeout(Duration::from_secs(30))
    }

    async fn table_names(&self) -> Vec<TableName> {
        if let Ok(table_names) = query_as("select name from sqlite_schema where type = 'table'")
            .fetch_all(&self.pool)
            .await
        {
            table_names
        } else {
            vec![]
        }
    }

    async fn index_names(&self) -> Vec<IndexName> {
        if let Ok(names) = query_as("select name from sqlite_schema where type = 'index'")
            .fetch_all(&self.pool)
            .await
        {
            names
        } else {
            vec![]
        }
    }

    async fn columns(&self) -> Vec<Column> {
        let column_defs: Vec<ColumnDef> = sqlx::query_as("select s.name as table_name, pti.name as name, pti.type as data_type, pti.pk as primary_key, pti.dflt_value as default_value, pti.[notnull] as not_null from sqlite_schema s left outer join pragma_table_info((s.name)) pti on pti.name <> s.name where s.type = 'table'").fetch_all(&self.pool).await.unwrap();
        column_defs
            .into_iter()
            .map(|cd| cd.into())
            .collect::<Vec<_>>()
    }

    async fn references(&self) -> Vec<Reference> {
        vec![]
    }

    async fn execute(&self, sql: &str) -> Result<(), sqlx::Error> {
        let _ = self.pool.execute(sql).await?;
        Ok(())
    }

    /// sync given a vector of impl Table will migrate the database to a new state
    async fn sync(&self, tables: Vec<&dyn Table>) -> Result<(), sqlx::Error> {
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
        let _ = self.execute(&drop_indexes_sql);

        Ok(())
    }
}

/// sync! macro helper for db migrations
///
/// Example:
///
/// let db = Database::new("sqlite://:memory:").await;
/// #[derive(Table)]
/// #[rizzle(table = "posts")]
/// struct Posts {
///   #[rizzle(primary_key)]
///   id: sqlite::Integer,
/// }
/// let posts = Posts::new();
/// if let Ok(_) = sync!(db, posts).await {}
macro_rules! sync {
    ($db:ident $(, $tables:expr)*) => {{
        $db.sync(vec![$(&$tables)*])
    }};
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
    // dbg!(&code_column_names);
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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::sqlite::Text;
    use macros::Table;

    async fn db() -> Database {
        Database::new("sqlite://:memory:".to_string()).await
    }

    #[tokio::test]
    async fn sync_new_tables_works() {
        let db = db().await;
        let a = A::new();
        assert_eq!(0, db.table_names().await.len());
        db.sync(vec![&a]).await;
        assert_eq!(1, db.table_names().await.len())
    }

    #[tokio::test]
    async fn sync_new_tables_is_idempotent_works() {
        let db = db().await;
        let a = A::new();
        assert_eq!(0, db.table_names().await.len());
        db.sync(vec![&a]).await;
        assert_eq!(1, db.table_names().await.len());
        db.sync(vec![&a]).await;
        assert_eq!(1, db.table_names().await.len())
    }

    #[tokio::test]
    async fn sync_with_dropped_tables_works() {
        let db = db().await;
        let a = A::new();
        let b = B::new();
        db.sync(vec![&a, &b]).await;
        assert_eq!(2, db.table_names().await.len());
        db.sync(vec![&b]).await;
        assert_eq!(1, db.table_names().await.len());
    }

    #[derive(Table)]
    #[rizzle(table = "table_a")]
    struct A {
        a: sqlite::Text,
    }

    #[derive(Table)]
    #[rizzle(table = "table_b")]
    struct B {
        #[rizzle(primary_key)]
        id: sqlite::Integer,
    }

    #[derive(Table)]
    #[rizzle(table = "table_a")]
    struct A2 {
        a: sqlite::Text,
        #[rizzle(not_null)]
        b: sqlite::Text,
    }

    #[tokio::test]
    async fn sync_with_added_columns_works() -> Result<(), sqlx::Error> {
        let a = A::new();
        let db = db().await;
        let _ = sync!(db, a).await?;
        let table_names = db.table_names().await;
        assert_eq!(1, table_names.len());
        let a2 = A2::new();
        let _ = sync!(db, a2).await?;
        let table_names = db.table_names().await;
        assert_eq!(1, table_names.len());
        let columns = db.columns().await;
        assert_eq!(2, columns.len());
        Ok(())
    }

    #[test]
    fn rizzle_table_name_works() {
        let a = A::new();
        assert_eq!("create table table_a (a text)", a.create_sql())
    }

    #[derive(Table)]
    #[rizzle(table = "index_table")]
    struct IndexTable {
        #[rizzle(primary_key)]
        id: sqlite::Integer,
        #[rizzle(not_null)]
        name: sqlite::Text,
        #[rizzle(columns = "name")]
        name_index: sqlite::UniqueIndex,
    }

    #[tokio::test]
    async fn drop_table_works() -> Result<(), sqlx::Error> {
        let db = db().await;
        assert_eq!(0, db.table_names().await.len());

        let a = A::new();
        let _ = sync!(db, a).await?;
        assert_eq!(1, db.table_names().await.len());

        let _ = sync!(db).await?;
        assert_eq!(0, db.table_names().await.len());

        Ok(())
    }

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

    #[tokio::test]
    async fn create_indexes_works() -> Result<(), sqlx::Error> {
        let db = db().await;
        assert_eq!(0, db.index_names().await.len());

        let it = IndexTable::new();
        let _ = sync!(db, it).await?;
        assert_eq!(
            vec!["name_index".to_owned()],
            db.index_names()
                .await
                .into_iter()
                .map(|ind| ind.0)
                .collect::<Vec<_>>()
        );

        Ok(())
    }

    #[tokio::test]
    async fn drop_indexes_works() -> Result<(), sqlx::Error> {
        let db = db().await;
        assert_eq!(0, db.index_names().await.len());

        let it = IndexTable::new();
        let _ = sync!(db, it).await?;

        assert_eq!(1, db.index_names().await.len());

        let _ = sync!(db).await?;
        assert_eq!(0, db.index_names().await.len());

        Ok(())
    }

    #[tokio::test]
    async fn drop_columns_works() -> Result<(), sqlx::Error> {
        let db = db().await;
        assert_eq!(0, db.columns().await.len());

        let a = A2::new();
        sync!(db, a).await?;

        assert_eq!(2, db.columns().await.len());

        let a = A::new();
        let _ = sync!(db, a).await?;
        assert_eq!(1, db.columns().await.len());

        Ok(())
    }

    #[derive(Table)]
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

        #[rizzle(references = "posts(user_id)")]
        posts: sqlite::Many,
    }

    #[derive(Table)]
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

    #[test]
    fn create_table_sql_works() {
        let users = Users::new();
        let posts = Posts::new();
        assert_eq!("create table users (id integer primary key, name text not null, created_at real not null, updated_at real not null)", users.create_sql());
        assert_eq!("create table posts (id integer primary key, body text not null, created_at real not null, updated_at real not null, user_id integer not null references users(id))", posts.create_sql())
    }
}
