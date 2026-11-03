# Schemas and Migrations

A Kura **schema** is an Erlang module that describes one table: its name, its columns and their types, its indexes and, later, its associations. You never write the SQL for that table. The `rebar3_kura` compile hook reads your schemas and generates migrations for whatever changed.

We start with the two tables everything else hangs off: users and posts.

## The user schema

Schemas live in `src/schemas/`. The blog's first version of `src/schemas/blog_user.erl`:

```erlang
-module(blog_user).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0]).

table() -> ~"users".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = email, type = string, nullable = false},
        #kura_field{name = username, type = string, nullable = false},
        #kura_field{name = hashed_password, type = string, nullable = false},
        #kura_field{
            name = role, type = {enum, [author, admin]}, nullable = false, default = author
        },
        #kura_field{name = confirmed_at, type = utc_datetime},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime},
        #kura_field{name = password, type = string, virtual = true},
        #kura_field{name = password_confirmation, type = string, virtual = true}
    ].

indexes() ->
    [
        {[email], #{unique => true}},
        {[username], #{unique => true}}
    ].
```

- `table/0` and `fields/0` are the required callbacks. `indexes/0` is optional.
- Each `#kura_field{}` has a `name` and a `type`. `primary_key`, `nullable` (default `true`) and `default` become column options in the migration.
- `inserted_at` and `updated_at` are special: Kura fills them in on insert and update.
- `virtual = true` fields exist only in changesets and never get a column. `password` and `password_confirmation` are virtual: the user types a password, we store only `hashed_password`. The [next chapter](changesets.md) does the hashing.
- `confirmed_at` and the email/password fields follow the shape `nova_auth` expects, so the [Authentication](../auth-sessions/authentication.md) chapter can build on this table. `role` drives [Authorization](../auth-sessions/authorization.md).

### Field types

| Kura type | PostgreSQL type | Erlang value |
|---|---|---|
| `id` | `BIGSERIAL` | integer |
| `integer`, `bigint`, `smallint` | `INTEGER`, `BIGINT`, `SMALLINT` | integer |
| `float` | `DOUBLE PRECISION` | float |
| `decimal` | `NUMERIC` | number |
| `string` | `VARCHAR(255)` | binary |
| `text` | `TEXT` | binary |
| `binary` | `BYTEA` | binary |
| `boolean` | `BOOLEAN` | `true` / `false` |
| `date`, `time` | `DATE`, `TIME` | `{Y, M, D}`, `{H, Mi, S}` |
| `utc_datetime` | `TIMESTAMPTZ` | `{{Y, M, D}, {H, Mi, S}}` |
| `naive_datetime` | `TIMESTAMP` | `{{Y, M, D}, {H, Mi, S}}` |
| `uuid` | `UUID` | binary |
| `jsonb` | `JSONB` | map or list |
| `{enum, [atom()]}` | `VARCHAR(255)` | atom |
| `{array, Type}` | `Type[]` | list |
| `{encrypted, Type}` | `BYTEA` | the inner type (see [Data Safety](data-safety.md)) |

```admonish tip
`string` is `VARCHAR(255)`. Use `text` for anything that can grow, like a post body.
```

## The post schema

`src/schemas/blog_post.erl`, first version:

```erlang
-module(blog_post).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0]).

table() -> ~"posts".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = title, type = string, nullable = false},
        #kura_field{name = body, type = text, nullable = false},
        #kura_field{
            name = status,
            type = {enum, [draft, published, archived]},
            nullable = false,
            default = draft
        },
        #kura_field{name = user_id, type = bigint, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

indexes() ->
    [
        {[user_id], #{}},
        {[status], #{}}
    ].
```

`user_id` points at the author. For now it is a plain `bigint`; the [Associations](associations.md) chapter declares the relationship and Kura adds the foreign key.

## Generating the migration

With the compile hook from the previous chapter in place, compiling is all it takes:

```shell
rebar3 compile
```

```
===> kura: generated src/migrations/m20261103072001_update_schema.erl
===> kura: migration generated
```

The migration, `src/migrations/m20261103072001_update_schema.erl`:

```erlang
-module(m20261103072001_update_schema).
-moduledoc false.
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").
-export([up/0, down/0]).

-spec up() -> [kura_migration:operation()].
up() ->
    [
        {create_table, ~"posts", [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{name = title, type = string, nullable = false},
            #kura_column{name = body, type = text, nullable = false},
            #kura_column{
                name = status,
                type = {enum, [draft, published, archived]},
                nullable = false,
                default = draft
            },
            #kura_column{name = user_id, type = bigint, nullable = false},
            #kura_column{name = inserted_at, type = utc_datetime},
            #kura_column{name = updated_at, type = utc_datetime}
        ]},
        {create_table, ~"users", [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            ...
        ]},
        {create_index, ~"posts", [user_id], #{}},
        {create_index, ~"posts", [status], #{}},
        {create_index, ~"users", [email], #{unique => true}},
        {create_index, ~"users", [username], #{unique => true}}
    ].

-spec down() -> [kura_migration:operation()].
down() ->
    [
        {drop_table, ~"posts"},
        {drop_table, ~"users"}
    ].
```

A migration is a `kura_migration` module whose `up/0` and `down/0` return a list of operations: `create_table`, `alter_table`, `create_index`, `drop_table`, `execute` and a few more. The file name starts with a UTC timestamp, which is also the version Kura records. The virtual fields got no column.

The hook does not generate anything when nothing changed:

```
===> kura: schemas up to date
```

```admonish warning title="Run rebar3 fmt after a migration is generated"
Generated migrations are not formatted with erlfmt. The blog's erlfmt configuration covers `src/**`, so `rebar3 fmt --check` fails right after a new migration appears. Run `rebar3 fmt` once and commit the formatted file. The listings in this book are formatted.
```

Commit migrations like any other source file. They are the history of your database.

## Running migrations

The blog runs `kura_migrator:migrate(blog_repo)` at boot (see [Database Setup](setup.md#running-migrations-at-boot)), so starting it applies the new migration:

```
[info] Kura: up migration 20261103072001 (m20261103072001_update_schema)
```

Kura keeps applied versions in `schema_migrations`:

```
blog_dev=# SELECT * FROM schema_migrations ORDER BY version;
    version     |        inserted_at
----------------+----------------------------
 20261103072001 | 2026-11-03 07:43:57.528022
```

## Changing a schema

Schemas change. Posts need to remember when they were published, so we add a field:

```erlang
#kura_field{name = user_id, type = bigint, nullable = false},
#kura_field{name = published_at, type = utc_datetime},
#kura_field{name = inserted_at, type = utc_datetime},
```

`rebar3 compile` replays the existing migrations, sees one column missing and generates `m20261103072100_alter_posts.erl`:

```erlang
up() ->
    [
        {alter_table, ~"posts", [
            {add_column, #kura_column{name = published_at, type = utc_datetime}}
        ]}
    ].

down() ->
    [
        {alter_table, ~"posts", [
            {drop_column, published_at}
        ]}
    ].
```

Removing a field generates a `drop_column`, a new index a `create_index`, and so on. Never edit a migration that has already run somewhere; change the schema and let the hook generate the next one.

## Tooling

`rebar3 kura` lists the plugin's commands:

```
kura <task>:
  check                  Fail if schemas have drifted from migrations
  compile                Auto-generate Kura migrations from schema changes
  gen_auth               Generate email/password authentication schemas
  gen_schemas            Generate kura schemas from an existing PostgreSQL database
  lint_migrations        Flag unsafe (destructive or blocking) migration operations
  setup                  Set up Kura in your project
```

### rebar3 kura check

`check` is the CI version of `compile`: it computes the same diff but writes nothing and fails if a migration is missing. On the blog:

```shell
rebar3 kura check
```

```
===> kura: no schema drift
```

Add a field to `blog_post` without compiling, and `check` catches it:

```
===> kura: blog has 1 missing migration op(s):
===>   - missing ALTER TABLE "posts" (1 ops)
```

followed by an error line telling you to run `rebar3 kura compile` and commit the result. Run it in CI so a schema change can never be merged without its migration.

### rebar3 kura lint_migrations

`lint_migrations` reads every migration and flags operations that are dangerous while old code is still running against the database: `drop_table`, `drop_column`, `rename_column` and `modify_column`. When you really mean it, list the operation in the migration's optional `safe/0` callback and the linter stops flagging it.

```shell
rebar3 kura lint_migrations
```

```
===> kura: no unsafe migration operations
```

### Status and rollback

`kura_migrator` also works from the shell. `status/1` lists every migration and whether it ran:

```erlang
1> kura_migrator:status(blog_repo).
[{20261103072001,m20261103072001_update_schema,up},
 {20261103072100,m20261103072100_alter_posts,up},
 ...]
```

`rollback/1` runs `down/0` of the latest migration and warns when that is destructive. Rolling back the audit table from the [Data Safety](data-safety.md) chapter:

```erlang
2> kura_migrator:rollback(blog_repo).
[warning] Kura: unsafe operation in m20261103073500_create_audit_log: drop_table <<"audit_log">> - Deploy code that stops using the table first, then drop in a later migration
[info] Kura: down migration 20261103073500 (m20261103073500_create_audit_log)
{ok,[20261103073500]}
3> lists:last(kura_migrator:status(blog_repo)).
{20261103073500,m20261103073500_create_audit_log,pending}
4> kura_migrator:migrate(blog_repo).
[info] Kura: up migration 20261103073500 (m20261103073500_create_audit_log)
{ok,[20261103073500]}
```

`rollback/2` takes a number of steps.

---

The tables exist. Next we control what goes into them with [Changesets and Validation](changesets.md).
