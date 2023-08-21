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
async fn main() -> Result<(), RizzleError> {
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

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
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

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
  let db = Database::connect("sqlite://:memory:").await?;
  let _ = sync!(db, comments).await?;
  let post: Post = db.insert(posts).values(Post { id: 1, body: "".to_owned() }).returning().await?;
  Ok(())
}
```

# Updating rows

```rust
use rizzle::{Database, Table, sqlite, eq};

#[derive(Table, Clone, Copy)]
#[rizzle(table = "comments")]
struct Comments {
    #[rizzle(primary_key)]
    id: sqlite::Integer,
    #[rizzle(not_null)]
    body: sqlite::Text,
}

#[derive(Row)]
struct Comment {
    id: i64,
    body: String,
}

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
    let db = db().await;
    let comments = Comments::new();

    let _ = sync!(db, comments).await?;

    let inserted_comment: Comment = db
      .insert(comments)
      .values(Comment {
          id: 1,
          body: "".to_owned(),
      })
      .returning()
      .await?;

    let _ = db
        .update(comments)
        .set(Comment {
            body: "comment".to_owned(),
            ..inserted_comment
        })
        .rows_affected()
        .await?;

    Ok(())
}
```

# Deleting rows

```rust
use rizzle::{Database, Table, sqlite, eq};

#[derive(Table, Clone, Copy)]
#[rizzle(table = "comments")]
struct Comments {
    #[rizzle(primary_key)]
    id: sqlite::Integer,
    #[rizzle(not_null)]
    body: sqlite::Text,
}

#[derive(Row)]
struct Comment {
    id: i64,
    body: String,
}

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
    let db = db().await;
    let comments = Comments::new();
    // don't forget to sync!
    let comment: Comment = db
        .delete(comments)
        .where(eq(comments.id, 1))
        .returning()
        .await?;

    Ok(())
}
```