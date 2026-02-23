# Associations and Preloading

So far our posts exist in isolation. In a real blog, posts belong to users and have comments. Kura supports `belongs_to`, `has_many`, `has_one`, and `many_to_many` associations with automatic preloading.

## Adding associations to schemas

### Post belongs to user

Update `src/schemas/post.erl` to add associations:

```erlang
-module(post).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0, associations/0, changeset/2]).

table() -> <<"posts">>.

primary_key() -> id.

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = title, type = string, nullable = false},
        #kura_field{name = body, type = text},
        #kura_field{name = status, type = {enum, [draft, published, archived]}, default = <<"draft">>},
        #kura_field{name = user_id, type = integer},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

associations() ->
    [
        #kura_assoc{name = author, type = belongs_to, schema = user, foreign_key = user_id},
        #kura_assoc{name = comments, type = has_many, schema = comment, foreign_key = post_id}
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(post, Data, Params, [title, body, status, user_id]),
    CS1 = kura_changeset:validate_required(CS, [title, body]),
    CS2 = kura_changeset:validate_length(CS1, title, [{min, 3}, {max, 200}]),
    CS3 = kura_changeset:validate_inclusion(CS2, status, [draft, published, archived]),
    kura_changeset:foreign_key_constraint(CS3, user_id).
```

The `associations/0` callback returns a list of `#kura_assoc{}` records:

- **`belongs_to`** — the foreign key (`user_id`) is on this table. `schema` is the associated module, `foreign_key` is the column.
- **`has_many`** — the foreign key (`post_id`) is on the other table.

We also added `foreign_key_constraint/2` to the changeset — if an insert fails because the user doesn't exist, Kura maps the PostgreSQL foreign key error to a friendly changeset error.

### Comment schema

Create `src/schemas/comment.erl`:

```erlang
-module(comment).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0, associations/0, changeset/2]).

table() -> <<"comments">>.

primary_key() -> id.

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = body, type = text, nullable = false},
        #kura_field{name = post_id, type = integer, nullable = false},
        #kura_field{name = user_id, type = integer, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

associations() ->
    [
        #kura_assoc{name = post, type = belongs_to, schema = post, foreign_key = post_id},
        #kura_assoc{name = author, type = belongs_to, schema = user, foreign_key = user_id}
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(comment, Data, Params, [body, post_id, user_id]),
    CS1 = kura_changeset:validate_required(CS, [body, post_id, user_id]),
    CS2 = kura_changeset:foreign_key_constraint(CS1, post_id),
    kura_changeset:foreign_key_constraint(CS2, user_id).
```

### User has many posts

Update `src/schemas/user.erl` to add the `has_many` side:

```erlang
-export([table/0, fields/0, primary_key/0, associations/0, changeset/2]).

%% ... fields() unchanged ...

associations() ->
    [
        #kura_assoc{name = posts, type = has_many, schema = post, foreign_key = user_id}
    ].

%% ... changeset/2 unchanged ...
```

## Generate the migration

Compile to generate the comments table migration:

```shell
rebar3 compile
```

```
===> [kura] Schema diff detected changes
===> [kura] Generated src/migrations/m20260223130000_create_comments.erl
```

The migration creates the `comments` table with foreign keys to `posts` and `users`.

## Preloading associations

By default, fetching a post returns only its own fields — associations are not loaded. Use `kura_query:preload/2` to eagerly load them.

### Preload via query

```erlang
Q = kura_query:from(post),
Q1 = kura_query:preload(Q, [author, comments]),
{ok, Posts} = blog_repo:all(Q1).
```

Each post in `Posts` now has `author` and `comments` keys:

```erlang
#{id => 1,
  title => <<"My First Post">>,
  author => #{id => 1, username => <<"alice">>, email => <<"alice@example.com">>, ...},
  comments => [
      #{id => 1, body => <<"Great post!">>, user_id => 2, ...},
      #{id => 2, body => <<"Thanks!">>, user_id => 1, ...}
  ],
  ...}
```

### Nested preloading

Load the author of each comment too:

```erlang
Q = kura_query:from(post),
Q1 = kura_query:preload(Q, [author, {comments, [author]}]),
{ok, Posts} = blog_repo:all(Q1).
```

Now each comment also has its `author` loaded.

### Standalone preload

If you already have records and want to preload associations after the fact:

```erlang
{ok, Post} = blog_repo:get(post, 1),
Post1 = blog_repo:preload(post, Post, [author, comments]).

%% Works with lists too
{ok, Posts} = blog_repo:all(kura_query:from(post)),
Posts1 = blog_repo:preload(post, Posts, [author]).
```

```admonish info
Kura uses `WHERE IN` queries for preloading — not JOINs. This means one extra query per association, which keeps things predictable and avoids N+1 problems.
```

## Creating with associations (cast_assoc)

You can create a post with comments in a single request using `cast_assoc`:

```erlang
Params = #{<<"title">> => <<"New Post">>,
           <<"body">> => <<"Content here">>,
           <<"comments">> => [
               #{<<"body">> => <<"First comment">>, <<"user_id">> => 2}
           ]},
CS = kura_changeset:cast(post, #{}, Params, [title, body, user_id]),
CS1 = kura_changeset:validate_required(CS, [title, body]),
CS2 = kura_changeset:cast_assoc(CS1, comments),
{ok, Post} = blog_repo:insert(CS2).
```

`cast_assoc` reads the `comments` key from the params, builds child changesets using `comment:changeset/2`, and wraps everything in a transaction. The parent is inserted first, then each child gets the parent's ID set as its foreign key.

### Custom cast function

If you need different validation for nested creates:

```erlang
CS2 = kura_changeset:cast_assoc(CS1, comments, #{
    with => fun(Data, ChildParams) ->
        comment:changeset(Data, ChildParams)
    end
}).
```

## API endpoint with preloading

Update the posts controller to return posts with their author and comments:

```erlang
show(#{bindings := #{<<"id">> := Id}}) ->
    case blog_repo:get(post, binary_to_integer(Id)) of
        {ok, Post} ->
            Post1 = blog_repo:preload(post, Post, [author, {comments, [author]}]),
            {json, post_with_assocs_to_json(Post1)};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"post not found">>}}
    end.

post_with_assocs_to_json(#{id := Id, title := Title, body := Body,
                            status := Status, author := Author,
                            comments := Comments}) ->
    #{id => Id,
      title => Title,
      body => Body,
      status => atom_to_binary(Status),
      author => #{id => maps:get(id, Author),
                  username => maps:get(username, Author)},
      comments => [#{id => maps:get(id, C),
                     body => maps:get(body, C),
                     author => #{id => maps:get(id, maps:get(author, C)),
                                 username => maps:get(username, maps:get(author, C))}}
                   || C <- Comments]}.
```

Test it:

```shell
curl -s localhost:8080/api/posts/1 | python3 -m json.tool
```

```json
{
  "id": 1,
  "title": "My First Post",
  "body": "Hello from Nova!",
  "status": "draft",
  "author": {
    "id": 1,
    "username": "alice"
  },
  "comments": [
    {
      "id": 1,
      "body": "Great post!",
      "author": {
        "id": 2,
        "username": "bob"
      }
    }
  ]
}
```

---

Next, let's add [tags, many-to-many relationships, and embedded schemas](advanced-data.md) for post metadata.
