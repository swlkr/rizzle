# rizzle [wip]

rizzle is an automatic migration generator and query builder for sqlite (*postgres coming at some point) for rust! 

*May or may not be inspired by [drizzle](https://github.com/drizzle-team/drizzle-orm)*

# Install

```sh
cargo add rizzle
```

# Connect to database

```rust
use rizzle::Database;

#[tokio::main]
fn main() -> Result<(), RizzleError> {
  let db = Database::connect("sqlite://:memory:").await?;

  // if you need more options, you can use DatabaseOptions
  use rizzle::sqlite::JournalMode;
  let options = DatabaseOptions::new("sqlite://:memory:").max_connections(10).create_if_missing().journal_mode(JournalMode::Wal);
  let db = Database::new(options).await?;

  Ok(())
}
```

# Auto migrations

```rust
use rizzle::{Database, Table, sync, sqlite::{Text, Integer}};

#[derive(Table)]
#[rizzle(table = "posts")]
struct Posts {
  #[rizzle(primary_key)]
  id: Integer,
  #[rizzle(not_null)]
  body: Text
}

fn main() -> Result<(), RizzleError> {
  let db = Database::connect("sqlite://:memory:").await?;
  let _ = sync!(db, posts).await;
  Ok(())
}
```

# Inserting rows

```rust
use rizzle::{Database, Table, sqlite::{Text, Integer}};

#[derive(Table)]
#[rizzle(table = "posts")]
struct Posts {
  #[rizzle(primary_key)]
  id: Integer,
  #[rizzle(not_null)]
  body: Text
}

#[derive(Row)]
struct Post {
  id: i64,
  body: String
}

fn main() -> Result<(), RizzleError> {
  let db = Database::connect("sqlite://:memory:").await?;
  // called sometime after sync!
  let post: Post = db.insert(posts).values(Post { id: 1, body: "".to_owned() }).returning().await?;
  Ok(())
}
```

# Quickstart

```rust
use rizzle::{Database, Table, Row, sqlite, eq};

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

#[derive(Row)]
struct User {
  id: i64,
  name: String,
  created_at: f64,
  updated_at: f64,
  posts: Vec<Post>
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

  #[rizzle(references = "users(id)")]
  user_id: sqlite::Integer
}

#[derive(Row)]
struct Post {
  id: i64,
  body: String,
  created_at: f64,
  updated_at: f64,
  user: User
}

#[tokio::main]
async fn main() {
  let db = Database::connect("sqlite://:memory:").await;
  let users = Users::new();
  let posts = Posts::new();

  // order matters here
  // tables are created in the same order they are passed in
  let _ = sync!(db, users, posts).await;

  let users = db.select().from(users).where(eq(users.id, 1)).collect().await;
  let posts = db.select().from(posts).join(users).limit(30).collect().await;
}
```

# Insert

```rust
use rizzle::{Database, Table, FromRow, sqlite, eq};

#[derive(Table)]
#[rizzle(table = "posts")]
struct Posts {
  #[rizzle(primary_key)]
  id: sqlite::Integer,

  #[rizzle(not_null)]
  body: sqlite::Text,
}

#[derive(Insert)]
struct NewPost {
  body: String
}

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
  // connect to the database, there are more options in DatabaseOptions
  let db = Database::connect("sqlite://:memory:").await;
  // grab a table
  let posts = Posts::new();
  // don't forget to migrate!
  let _ = sync!(db, users, posts).await;
  // create your new row
  let new_post = NewPost { body: "rizzle".to_string() }
  // insert the row, returning number of rows affected
  let rows_affected = db.insert(posts).values(new_post).rows_affected().await?;
}
```