# rizzle [wip]

rizzle is an automatic migration generator and query builder for sqlite (*postgres coming at some point) for rust! 

*May or may not be inspired by [drizzle](https://github.com/drizzle-team/drizzle-orm)*

# Install

```sh
cargo add rizzle
```

# Connect to database

```rust
use rizzle::prelude::*;

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

# Declare your schema

```rust
use rizzle::prelude::*;

#[derive(Table, Clone, Copy)]
#[rizzle(table = "posts")]
struct Posts {
  #[rizzle(primary_key)]
  id: Integer,

  #[rizzle(not_null)]
  body: Text
}

#[derive(Table, Clone, Copy)]
#[rizzle(table = "comments")]
struct Comments {
    #[rizzle(primary_key)]
    id: Integer,

    #[rizzle(not_null)]
    body: Text,

    #[rizzle(references = "posts(id)")]
    post_id: Integer,
}

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
  let db = Database::connect("sqlite://:memory:").await?;
  let _ = sync!(db, posts, comments).await?;
  Ok(())
}
```

# Inserting, updating, and deleting rows

```rust
use rizzle::prelude::*;

#[derive(Row)]
struct Post {
  id: i64,
  body: String
}

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
    let db = Database::connect("sqlite://:memory:").await?;
    let posts = Posts::new();

    // insert into posts (id, body) values (?, ?) returning *
    let inserted_post: Post = db
        .insert(posts)
        .values(Post {
            id: 1,
            body: "".to_owned(),
        })
        .returning()
        .await?;

    // update posts set body = ?, id = ? where id = ?
    let rows_affected = db
        .update(posts)
        .set(Post {
            body: "post".to_owned(),
            ..inserted_post
        })
        .where(eq(posts.id, 1))
        .rows_affected()
        .await?;

    // delete from posts where id = ? returning *
    let deleted_post = db.delete(posts).where(eq(posts.id, 1)).returning().await?;

    Ok(())
}
```

# Selecting rows with *

```rust
use rizzle::prelude::*;

#[derive(Row)]
struct Comment {
    id: i64,
    body: String,
    post_id: i64
}

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
    let db = Database::connect("sqlite://:memory:").await?;
    let comments = Comments::new();

    // select * from comments
    let rows: Vec<Comment> = db.select().from(comments).all().await;

    Ok(())
}
```

# Selecting specific columns

```rust
use rizzle::prelude::*;

#[derive(New, Select)]
struct PartialComment {
  body: String
}

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
    let db = Database::connect("sqlite://:memory:").await?;
    let comments = Comments::new();
    let partial_comment = PartialComment::new();

    // select body from comments
    let partial_rows: Vec<PartialComment> = db.select_with(partial_comment).from(comments).all().await;

    Ok(())
}
```

# Joins

```rust
use rizzle::prelude::*;

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
    let posts = Posts::new();
    let comments = Comments::new();

    let db = Database::connect("sqlite://:memory:").await?;

    // select * from comments inner join posts on posts.id = comments.post_id
    let rows: Vec<Comment> = db
        .select()
        .from(comments)
        .inner_join(posts, on(posts.id, comments.post_id))
        .all()
        .await;

    Ok(())
}
```

# Prepared statements

```rust
use rizzle::prelude::*;

#[tokio::main]
async fn main() -> Result<(), RizzleError> {
    let comments = Comments::new();
    let db = Database::connect("sqlite://:memory:").await?;

    // select * from comments
    let query = db.select().from(comments);

    // prepare the query store it in a once lock or something
    let prepared = query.prepare_as::<Comment>();

    // execute the prepared query later
    let rows = prepared.all().await?;

    Ok(())
}
```
