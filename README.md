# rizzle [wip]

rizzle is a database automatic migration generator and query builder in one for rust! 

*May or may not be inspired by [drizzle](https://github.com/drizzle-team/drizzle-orm)*

# Quickstart

```rust
use rizzle::{Database, Table, sqlite};

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

  #[rizzle(references = "posts.user_id")]
  posts: sqlite::Many,
}

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

  #[rizzle(references = "users.id")]
  user_id: sqlite::Integer
}

struct Post {
  id: i64,
  body: String,
  created_at: f64,
  updated_at: f64,
  user: User
}

#[tokio::main]
async fn main() {
  let db = Database::new("sqlite://:memory:").await;
  let users = Users::new();
  let posts = Posts::new();

  // order matters here
  // tables are created in the same order they are passed in
  db.sync!(&users, &posts);

  let users: User = db.select()
                      .from(&users)
                      .where(eq(&users.id, 1))
                      .limit(1)
                      .collect()
                      .first()
                      .expect("user expected with id 1");

  let posts_with_users: Vec<Post> = db.select()
                                      .from(&posts)
                                      .join(&users)
                                      .limit(30)
                                      .collect();

  let users_with_posts: Vec<User> = db.select()
                                      .from(&users)
                                      .with(&posts)
                                      .where(eq(&users.id, 1))
                                      .limit(1)
                                      .collect()
                                      .first()
                                      .expect("user expected with id 1");
}
```