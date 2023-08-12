use sqlx::{
    query_as,
    sqlite::{SqliteConnectOptions, SqliteJournalMode, SqlitePoolOptions, SqliteSynchronous},
    Executor, FromRow, SqlitePool,
};
use std::time::Duration;

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

    fn add_column_sql(&self) -> String {
        format!(
            "alter table {} add column {};",
            self.table_name,
            self.definition_sql()
        )
    }

    fn full_name(&self) -> String {
        format!("{} {}", self.table_name, self.name)
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
        }
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
}

pub mod sqlite {
    pub type Integer = &'static str;
    pub type Text = &'static str;
    pub type Blob = &'static str;
    pub type Real = &'static str;
    pub type Index = &'static str;
    pub type UniqueIndex = &'static str;

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
                table_name: self.name(),
                ..Default::default()
            },
            Column {
                name: "name".to_string(),
                not_null: true,
                data_type: sqlite::DataType::Text,
                table_name: self.name(),
                ..Default::default()
            },
            Column {
                name: "created_at".to_string(),
                data_type: sqlite::DataType::Real,
                not_null: true,
                table_name: self.name(),
                ..Default::default()
            },
            Column {
                name: "updated_at".to_string(),
                data_type: sqlite::DataType::Real,
                not_null: true,
                table_name: self.name(),
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
            .map(|c| c.definition_sql())
            .collect::<Vec<_>>()
            .join(", ");
        format!("create table {} ({});", self.name(), columns_sql)
    }

    fn drop_table_sql(&self) -> String {
        todo!()
    }
}

#[derive(FromRow)]
struct TableName(String);

impl Table for TableName {
    fn new() -> Self {
        todo!()
    }

    fn name(&self) -> String {
        todo!()
    }

    fn columns(&self) -> Vec<Column> {
        todo!()
    }

    fn indexes(&self) -> Vec<Index> {
        todo!()
    }

    fn create_table_sql(&self) -> String {
        todo!()
    }

    fn drop_table_sql(&self) -> String {
        format!("drop table {};", self.0)
    }
}

fn tables_to_create<'a>(
    db_table_names: &'a Vec<TableName>,
    tables: &'a Vec<impl Table>,
) -> Vec<&'a impl Table> {
    let db_table_names: Vec<_> = db_table_names.iter().map(|table| &table.0).collect();
    tables
        .iter()
        .filter(|t| !db_table_names.contains(&&t.name()))
        .collect::<Vec<_>>()
}

fn create_tables_sql(tables: Vec<&impl Table>) -> String {
    tables
        .iter()
        .map(|table| table.create_table_sql())
        .collect::<Vec<_>>()
        .join("\n")
}

fn tables_to_drop<'a>(
    db_table_names: &'a Vec<TableName>,
    tables: &'a Vec<impl Table>,
) -> Vec<&'a TableName> {
    let table_names: Vec<_> = tables.iter().map(|t| t.name()).collect();
    db_table_names
        .iter()
        .filter(|t| !table_names.contains(&t.0))
        .collect::<Vec<_>>()
}

fn drop_tables_sql(table_names: Vec<&TableName>) -> String {
    table_names
        .iter()
        .map(|tn| tn.drop_table_sql())
        .collect::<Vec<_>>()
        .join("\n")
}

fn columns_to_add(db_columns: Vec<Column>, code_columns: Vec<Column>) -> Vec<Column> {
    let db_column_names = db_columns.iter().map(|c| c.full_name()).collect::<Vec<_>>();
    dbg!(&db_column_names);
    let code_column_names = &code_columns
        .iter()
        .map(|c| c.full_name())
        .collect::<Vec<_>>();
    dbg!(&code_column_names);
    let result = code_columns
        .into_iter()
        .filter(|c| !db_column_names.contains(&c.full_name()))
        .collect();
    result
}

fn add_columns_sql(columns: Vec<Column>) -> String {
    columns
        .iter()
        .map(|c| c.add_column_sql())
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

    async fn columns(&self) -> Vec<Column> {
        let column_defs: Vec<ColumnDef> = sqlx::query_as("select s.name as table_name, pti.name as name, pti.type as data_type, pti.pk as primary_key, pti.dflt_value as default_value, pti.[notnull] as not_null from sqlite_schema s left outer join pragma_table_info((s.name)) pti on pti.name <> s.name where s.type = 'table'").fetch_all(&self.pool).await.unwrap();
        column_defs
            .into_iter()
            .map(|cd| cd.into())
            .collect::<Vec<_>>()
    }

    async fn execute(&self, sql: &str) -> Result<(), sqlx::Error> {
        let _ = self.pool.execute(sql).await?;
        Ok(())
    }

    async fn sync(&self, tables: Vec<impl Table>) -> Result<(), sqlx::Error> {
        let table_names = self.table_names().await;
        let tables_to_create = tables_to_create(&table_names, &tables);
        let create_tables_sql = create_tables_sql(tables_to_create);
        let _ = self.execute(&create_tables_sql).await?;
        let tables_to_drop = tables_to_drop(&table_names, &tables);
        let drop_tables_sql = drop_tables_sql(tables_to_drop);
        let _ = self.execute(&drop_tables_sql).await?;
        let db_columns = self.columns().await;
        let new_columns = tables.iter().flat_map(|t| t.columns()).collect::<Vec<_>>();
        let columns_to_add = columns_to_add(db_columns, new_columns);
        let add_columns_sql = add_columns_sql(columns_to_add);
        let _ = self.execute(&add_columns_sql).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    async fn db() -> Database {
        Database::new("sqlite://:memory:".to_string()).await
    }

    #[test]
    fn create_tables_sql_works() {
        let sql = create_tables_sql(vec![&Users::new()]);
        assert_eq!("create table users (id integer primary key, name text not null, created_at real not null, updated_at real not null);", sql)
    }

    #[test]
    fn create_tables_sql_with_existing_table_works() {
        let table_names = vec![TableName("users".to_string())];
        let users = Users::new();
        let tables = vec![users];
        let tables_to_create = tables_to_create(&table_names, &tables);
        let sql = create_tables_sql(tables_to_create);
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
        let db = db().await;
        let sql = create_tables_sql(vec![&Users::new()]);
        let _ = db.execute(&sql).await;
        let names: Vec<_> = db.table_names().await.into_iter().map(|tn| tn.0).collect();
        assert_eq!(vec!["users".to_string()], names)
    }

    #[test]
    fn tables_to_drop_works() {
        let expected = vec![TableName("users".to_string())];
        let tables: Vec<TableName> = vec![];
        let table_name: Vec<_> = tables_to_drop(&expected, &tables)
            .iter()
            .map(|tn| tn.0.clone())
            .collect();
        assert_eq!(table_name, vec!["users".to_string()])
    }

    #[test]
    fn drop_tables_sql_works() {
        let users = TableName("users".to_string());
        let bobby_tables = vec![&users];
        let drop_tables_sql = drop_tables_sql(bobby_tables);
        assert_eq!("drop table users;", drop_tables_sql)
    }

    fn empty_tables() -> Vec<impl Table> {
        let mut tables: Vec<_> = vec![TableName("".to_string())];
        let _ = tables.pop();
        tables
    }

    #[tokio::test]
    async fn sync_with_drop_tables_works() -> Result<(), sqlx::Error> {
        let users = Users::new();
        let tables = vec![users];
        let db = db().await;
        let _ = db.sync(tables).await?;
        let table_names = db.table_names().await;
        assert_eq!(1, table_names.len());
        let _ = db.sync(empty_tables()).await?;
        let table_names = db.table_names().await;
        assert!(table_names.is_empty());
        Ok(())
    }

    // #[derive(Table)]
    // #[rizzle(table_name = "table_a")]
    struct TableA {
        a: sqlite::Text,
    }

    impl Table for TableA {
        fn new() -> Self {
            Self { a: "a" }
        }

        fn name(&self) -> String {
            "table_a".to_string()
        }

        fn columns(&self) -> Vec<Column> {
            vec![Column {
                table_name: self.name(),
                name: "a".to_owned(),
                data_type: sqlite::DataType::Text,
                ..Default::default()
            }]
        }

        fn indexes(&self) -> Vec<Index> {
            todo!()
        }

        fn create_table_sql(&self) -> String {
            format!(
                "create table {} ({});",
                self.name(),
                self.columns()
                    .iter()
                    .map(|c| c.definition_sql())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }

        fn drop_table_sql(&self) -> String {
            format!("drop table {}", self.name())
        }
    }

    //#[derive(Table)]
    // #[rizzle(table_name = "table_a")]
    struct TableA2 {
        a: sqlite::Text,
        b: sqlite::Text,
    }

    impl Table for TableA2 {
        fn new() -> Self {
            Self { a: "a", b: "b" }
        }

        fn name(&self) -> String {
            // this comes from rizzle(table_name = "table_a") attribute
            "table_a".to_string()
        }

        fn columns(&self) -> Vec<Column> {
            vec![
                Column {
                    table_name: self.name(),
                    name: "a".to_owned(),
                    data_type: sqlite::DataType::Text,
                    ..Default::default()
                },
                Column {
                    table_name: self.name(),
                    name: "b".to_owned(),
                    data_type: sqlite::DataType::Text,
                    ..Default::default()
                },
            ]
        }

        fn indexes(&self) -> Vec<Index> {
            todo!()
        }

        fn create_table_sql(&self) -> String {
            format!(
                "create table {} ({});",
                self.name(),
                self.columns()
                    .iter()
                    .map(|c| c.definition_sql())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }

        fn drop_table_sql(&self) -> String {
            format!("drop table {}", self.name())
        }
    }

    #[tokio::test]
    async fn sync_with_added_columns_works() -> Result<(), sqlx::Error> {
        let a = TableA::new();
        let tables = vec![a];
        let db = db().await;
        let _ = db.sync(tables).await?;
        let table_names = db.table_names().await;
        assert_eq!(1, table_names.len());
        let a2 = TableA2::new();
        let tables = vec![a2];
        let _ = db.sync(tables).await?;
        let table_names = db.table_names().await;
        assert_eq!(1, table_names.len());
        let columns = db.columns().await;
        assert_eq!(2, columns.len());
        Ok(())
    }
}
