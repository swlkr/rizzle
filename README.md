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
    let db = Database::connect("sqlite://:memory:").await?;
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
    let db = Database::connect("sqlite://:memory:").await?;
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

# Selecting rows

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

// partial selects
#[derive(New, Select)]
struct PartialComment {
  body: String
}

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
    let db = Database::connect("sqlite://:memory:").await?;
    let comments = Comments::new();
    // don't forget to sync!
    let rows: Vec<Comment> = db.select().from(comments).all().await;
    let partial_comment = PartialComment::new();
    let partial_rows: Vec<PartialComment> = db.select_with(partial_comment).from(comments).all().await;

    Ok(())
}
```

# Joins

```rust
use rizzle::{Database, Table, sqlite, on};

#[derive(Table, Clone, Copy)]
#[rizzle(table = "comments")]
struct Comments {
    #[rizzle(primary_key)]
    id: sqlite::Integer,
    #[rizzle(not_null)]
    body: sqlite::Text,
    #[rizzle(not_null, references = "posts(id)")]
    post_id: sqlite::Integer
}

#[derive(Table, Clone, Copy)]
#[rizzle(table = "posts")]
struct Posts {
    #[rizzle(primary_key)]
    id: sqlite::Integer,
    #[rizzle(not_null)]
    body: sqlite::Text,
}

#[derive(Row)]
struct Comment {
    id: i64,
    body: String,
    post_id: i64,
}

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
    let db = Database::connect("sqlite://:memory:").await?;
    let posts = Posts::new();
    let comments = Comments::new();
    let _ = sync!(db, posts, comments).await?;
    let rows: Vec<Comment> = db.select().from(comments).inner_join(posts, on(posts.id, comments.post_id)).all().await;

    Ok(())
}
```
