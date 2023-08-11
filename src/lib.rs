use std::time::Duration;

use sqlx::{
    query_as,
    sqlite::{SqliteConnectOptions, SqliteJournalMode, SqlitePoolOptions, SqliteSynchronous},
    Executor, FromRow, SqlitePool,
};

#[derive(Default)]
struct Column {
    default_value: Option<String>,
    name: String,
    not_null: bool,
    primary_key: bool,
    data_type: sqlite::DataType,
}

impl Column {
    fn to_sql(&self) -> String {
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
        vec![
            Some(self.name.as_ref()),
            Some(data_type.as_ref()),
            primary_key,
            not_null,
            default_value,
        ]
        .into_iter()
        .filter_map(|s| s)
        .collect::<Vec<_>>()
        .join(" ")
    }
}

#[derive(Default)]
struct Index {
    name: String,
    columns: Vec<Column>,
}

trait Table {
    fn new() -> Self;
    fn name(&self) -> String;
    fn columns(&self) -> Vec<Column>;
    fn indexes(&self) -> Vec<Index>;
    fn create_table_sql(&self) -> String;
    fn drop_table_sql(&self) -> String;
    fn add_column_sql(&self, column: Column) -> String;
    fn drop_column_sql(&self, column: Column) -> String;
    fn create_index_sql(&self, index: Index) -> String;
    fn drop_index_sql(&self, index: Index) -> String;
}

pub mod sqlite {
    pub type Integer = &'static str;
    pub type Text = &'static str;
    pub type Blob = &'static str;
    pub type Real = &'static str;
    pub type Index = &'static str;
    pub type UniqueIndex = &'static str;

    #[derive(Default)]
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
}

// #[derive(Table)]
struct Users {
    // #[column(pk)]
    id: sqlite::Integer,
    // #[column(not_null)]
    name: sqlite::Text,
    // #[column(not_null)]
    created_at: sqlite::Real,
    // #[column(not_null)]
    updated_at: sqlite::Real,
    // #[index(name)]
    name_idx: sqlite::UniqueIndex,
}

impl Table for Users {
    fn new() -> Self {
        Self {
            id: "id",
            name: "name",
            created_at: "created_at",
            updated_at: "updated_at",
            name_idx: "name_idx",
        }
    }

    fn name(&self) -> String {
        "users".to_string()
    }

    fn columns(&self) -> Vec<Column> {
        vec![
            Column {
                name: "id".to_string(),
                primary_key: true,
                data_type: sqlite::DataType::Integer,
                ..Default::default()
            },
            Column {
                name: "name".to_string(),
                not_null: true,
                data_type: sqlite::DataType::Text,
                ..Default::default()
            },
            Column {
                name: "created_at".to_string(),
                data_type: sqlite::DataType::Real,
                not_null: true,
                ..Default::default()
            },
            Column {
                name: "updated_at".to_string(),
                data_type: sqlite::DataType::Real,
                not_null: true,
                ..Default::default()
            },
        ]
    }

    fn indexes(&self) -> Vec<Index> {
        vec![Index {
            name: "name_idx".to_string(),
            columns: vec![Column {
                name: "name".to_string(),
                not_null: false,
                data_type: sqlite::DataType::Text,
                ..Default::default()
            }],
        }]
    }

    fn create_table_sql(&self) -> String {
        let columns_sql = self
            .columns()
            .iter()
            .map(|c| c.to_sql())
            .collect::<Vec<_>>()
            .join(", ");
        format!("create table {} ({});", self.name(), columns_sql)
    }

    fn drop_table_sql(&self) -> String {
        todo!()
    }

    fn add_column_sql(&self, column: Column) -> String {
        todo!()
    }

    fn drop_column_sql(&self, column: Column) -> String {
        todo!()
    }

    fn create_index_sql(&self, index: Index) -> String {
        todo!()
    }

    fn drop_index_sql(&self, index: Index) -> String {
        todo!()
    }
}

#[derive(FromRow)]
struct TableName(String);

fn tables_to_create(db_table_names: Vec<TableName>, tables: Vec<impl Table>) -> Vec<impl Table> {
    let sql_table_names: Vec<_> = db_table_names.iter().map(|table| &table.0).collect();
    tables
        .into_iter()
        .filter(|t| !sql_table_names.contains(&&t.name()))
        .collect::<Vec<_>>()
}

fn create_tables_sql(db_table_names: Vec<TableName>, tables: Vec<impl Table>) -> String {
    let tables = tables_to_create(db_table_names, tables);
    tables
        .iter()
        .map(|table| table.create_table_sql())
        .collect::<Vec<_>>()
        .join("\n")
}

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

    async fn execute(&self, sql: &str) -> Result<(), sqlx::Error> {
        let _ = self.pool.execute(sql).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_tables_sql_works() {
        let sql = create_tables_sql(vec![], vec![Users::new()]);
        assert_eq!("create table users (id integer primary key, name text not null, created_at real not null, updated_at real not null);", sql)
    }

    #[test]
    fn create_tables_sql_with_existing_table_works() {
        let sql = create_tables_sql(vec![TableName("users".to_string())], vec![Users::new()]);
        assert!(sql.is_empty())
    }

    #[tokio::test]
    async fn db_table_names_works() {
        let db = Database::new("sqlite://:memory:".to_string()).await;
        let db_table_names = db.table_names().await;
        assert!(db_table_names.is_empty())
    }

    #[tokio::test]
    async fn db_table_names_with_existing_table_works() {
        let db = Database::new("sqlite://:memory:".to_string()).await;
        let sql = create_tables_sql(vec![], vec![Users::new()]);
        let _ = db.execute(&sql).await;
        let names: Vec<_> = db.table_names().await.into_iter().map(|tn| tn.0).collect();
        assert_eq!(vec!["users".to_string()], names)
    }
}
