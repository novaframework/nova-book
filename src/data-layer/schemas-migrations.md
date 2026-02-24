# Schemas and Migrations

In the previous chapter we set up the database connection and repo. Now let's define schemas — Erlang modules that describe your data — and watch Kura generate migrations automatically.

## Defining the user schema

Create `src/schemas/user.erl`:

```erlang
-module(user).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0]).

table() -> <<"users">>.

primary_key() -> id.

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = username, type = string, nullable = false},
        #kura_field{name = email, type = string, nullable = false},
        #kura_field{name = password_hash, type = string, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].
```

A schema module implements the `kura_schema` behaviour and exports three required callbacks:

- **`table/0`** — the PostgreSQL table name
- **`primary_key/0`** — the primary key field name
- **`fields/0`** — a list of `#kura_field{}` records describing each column

Each field has a `name` (atom), `type` (one of Kura's types), and optional properties like `nullable` and `default`.

### Kura field types

| Type | PostgreSQL | Erlang |
|---|---|---|
| `id` | `BIGSERIAL` | integer |
| `integer` | `INTEGER` | integer |
| `float` | `DOUBLE PRECISION` | float |
| `string` | `VARCHAR(255)` | binary |
| `text` | `TEXT` | binary |
| `boolean` | `BOOLEAN` | boolean |
| `date` | `DATE` | `{Y, M, D}` |
| `utc_datetime` | `TIMESTAMPTZ` | `{{Y,M,D},{H,Mi,S}}` |
| `uuid` | `UUID` | binary |
| `jsonb` | `JSONB` | map/list |
| `{enum, [atoms]}` | `VARCHAR(255)` | atom |
| `{array, Type}` | `Type[]` | list |

## Auto-generating migrations

With the `rebar3_kura` compile hook we added in the previous chapter, compile the project:

```shell
rebar3 compile
```

```
===> [kura] Schema diff detected changes
===> [kura] Generated src/migrations/m20260223120000_create_users.erl
===> Compiling blog
```

Kura compared your schema definitions against the current database state (no migrations yet = empty database) and generated a migration file.

## Walking through the migration

Open the generated file in `src/migrations/`:

```erlang
-module(m20260223120000_create_users).
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").

-export([up/0, down/0]).

up() ->
    [{create_table, <<"users">>, [
        #kura_column{name = id, type = id, primary_key = true, nullable = false},
        #kura_column{name = username, type = string, nullable = false},
        #kura_column{name = email, type = string, nullable = false},
        #kura_column{name = password_hash, type = string, nullable = false},
        #kura_column{name = inserted_at, type = utc_datetime},
        #kura_column{name = updated_at, type = utc_datetime}
    ]}].

down() ->
    [{drop_table, <<"users">>}].
```

The migration has two functions:
- **`up/0`** — returns operations to apply (create the table)
- **`down/0`** — returns operations to reverse (drop the table)

Migration files are named with a timestamp prefix so they run in order.

## Defining the post schema

Now let's add a post schema with an enum type for status. Create `src/schemas/post.erl`:

```erlang
-module(post).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0]).

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
```

The `status` field uses an enum type — Kura stores it as `VARCHAR(255)` in PostgreSQL but casts between atoms and binaries automatically. When you query a post, `status` comes back as an atom (`draft`, `published`, or `archived`).

Compile again:

```shell
rebar3 compile
```

```
===> [kura] Schema diff detected changes
===> [kura] Generated src/migrations/m20260223120100_create_posts.erl
===> Compiling blog
```

A second migration appears for the posts table.

## Running migrations

Kura runs migrations when the repo starts. On application boot, `blog_repo:start()` checks the `schema_migrations` table and runs any pending migrations in order.

Start the application:

```shell
rebar3 nova serve
```

Check the logs — you should see the migrations being applied:

```
[info] [kura] Running migration: m20260223120000_create_users
[info] [kura] Running migration: m20260223120100_create_posts
```

### The schema_migrations table

Kura creates a `schema_migrations` table to track which migrations have been applied:

```sql
blog_dev=# SELECT * FROM schema_migrations;
      version       |       inserted_at
--------------------+-------------------
 20260223120000     | 2026-02-23 12:00:00
 20260223120100     | 2026-02-23 12:01:00
```

Each row records a migration version (the timestamp from the filename). Kura only runs migrations that are not in this table.

## Modifying schemas

When you change a schema — add a field, remove one, or change a type — Kura detects the difference on the next compile and generates an `alter_table` migration.

For example, add a `bio` field to the user schema:

```erlang
fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = username, type = string, nullable = false},
        #kura_field{name = email, type = string, nullable = false},
        #kura_field{name = password_hash, type = string, nullable = false},
        #kura_field{name = bio, type = text},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].
```

Compile:

```shell
rebar3 compile
```

```
===> [kura] Schema diff detected changes
===> [kura] Generated src/migrations/m20260223120200_alter_users.erl
```

The generated migration adds the column:

```erlang
up() ->
    [{alter_table, <<"users">>, [
        {add_column, #kura_column{name = bio, type = text}}
    ]}].

down() ->
    [{alter_table, <<"users">>, [
        {drop_column, bio}
    ]}].
```

Define your schema, compile, migration appears. No SQL files to maintain.

---

Now that we have tables, let's learn about [changesets and validation](changesets.md) — how Kura validates and tracks data changes before they hit the database.
