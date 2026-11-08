# Database Setup

Nova has no built-in database layer. You pick what fits the project. This book uses [Kura](https://github.com/Taure/kura), an Ecto-inspired data layer for Erlang: schemas, changesets, a query builder and migrations generated from your schemas. Kura itself is database-agnostic. The PostgreSQL backend lives in a separate package, `kura_postgres`, which talks to the database through the [minato](https://github.com/Taure/minato) driver.

In this Part the blog gets its data model: users, posts, comments and tags. By the end of the chapter the blog connects to PostgreSQL and runs migrations when it boots.

## Adding dependencies

Three things go into `rebar.config`: the `kura` library, the `kura_postgres` backend and the `rebar3_kura` plugin.

```erlang
{deps, [
    {nova, "~> 0.18"},
    {kura, "~> 2.30"},
    {kura_postgres, "~> 1.1"},
    %% Used for logging - Change if needed
    {flatlog, "0.1.2"}
]}.
```

```erlang
{project_plugins, [
    ...
    {rebar3_kura, "~> 0.18"},
    {erlfmt, "~>1.7"}
]}.
```

List both applications in `src/blog.app.src` so they start before the blog:

```erlang
{applications, [
    kernel,
    stdlib,
    nova,
    kura,
    kura_postgres
]},
```

You do not start minato yourself. `kura_postgres` depends on it, and Kura starts a connection pool for every configured repo when its application starts.

## Generating the repo with rebar3 kura setup

A **repo** is the module you call for every database operation. The plugin generates it:

```shell
rebar3 kura setup --name blog_repo
```

```
===>
==> Setting up Kura for blog

===> [ok] kura dependency declared in rebar.config
===> [ok] generated src/blog_repo.erl
===> [ok] created src/migrations/
===> [!!] provider hook missing. Add to rebar.config:
     {provider_hooks, [{pre, [{compile, {kura, compile}}]}]}.
===>
--- Next steps ---

1. Add a backend to rebar.config deps (kura 2.x split the backend
   into separate packages):

   {kura_postgres, "~> 1.0"}
...
3. Add database config to config/sys.config:

   [{kura, [
       {repos, #{
           blog_repo => #{
               backend   => kura_backend_postgres,
               host      => "localhost",
               port      => 5432,
               database  => "blog_dev",
               user      => "postgres",
               password  => "postgres",
               pool_size => 10
           }
       }}
   ]}].
...
4. Run migrations on app startup (kura starts the pool for you):

   kura_migrator:migrate(blog_repo),
...
```

The rest of this chapter walks through those steps. The generated `src/blog_repo.erl` looks like this (export list shortened):

```erlang
-module(blog_repo).
-behaviour(kura_repo).

-export([
    otp_app/0,
    start/0,
    all/1,
    get/2,
    ...
]).

otp_app() -> blog.

start() -> kura_repo_worker:start(?MODULE).
all(Q) -> kura_repo_worker:all(?MODULE, Q).
get(Schema, Id) -> kura_repo_worker:get(?MODULE, Schema, Id).
get_by(Schema, Clauses) -> kura_repo_worker:get_by(?MODULE, Schema, Clauses).
one(Q) -> kura_repo_worker:one(?MODULE, Q).
insert(CS) -> kura_repo_worker:insert(?MODULE, CS).
insert(CS, Opts) -> kura_repo_worker:insert(?MODULE, CS, Opts).
update(CS) -> kura_repo_worker:update(?MODULE, CS).
delete(CS) -> kura_repo_worker:delete(?MODULE, CS).
insert_all(Schema, Entries) -> kura_repo_worker:insert_all(?MODULE, Schema, Entries).
update_all(Q, Updates) -> kura_repo_worker:update_all(?MODULE, Q, Updates).
delete_all(Q) -> kura_repo_worker:delete_all(?MODULE, Q).
preload(Schema, Records, Assocs) -> kura_repo_worker:preload(?MODULE, Schema, Records, Assocs).
transaction(Fun) -> kura_repo_worker:transaction(?MODULE, Fun).
multi(Multi) -> kura_repo_worker:multi(?MODULE, Multi).
query(SQL, Params) -> kura_repo_worker:query(?MODULE, SQL, Params).
exists(Q) -> kura_repo_worker:exists(?MODULE, Q).
reload(Schema, Record) -> kura_repo_worker:reload(?MODULE, Schema, Record).
insert_all(Schema, Entries, Opts) -> kura_repo_worker:insert_all(?MODULE, Schema, Entries, Opts).
```

The only callback the `kura_repo` behaviour needs is `otp_app/0`. Everything else is a one-line delegation to `kura_repo_worker`, so the rest of the app can write `blog_repo:insert(CS)` instead of passing the repo name around. When a later chapter needs another `kura_repo_worker` function (soft deletes, counting) we add a line here.

```admonish note
The generated `start/0` is a leftover from older Kura versions. With the `{repos, #{...}}` configuration below, Kura starts the pool itself during application start, so nothing calls `blog_repo:start/0`.
```

## The compile hook

The setup output flagged the missing provider hook. Add it to `rebar.config` next to the ErlyDTL hook from Part I:

```erlang
{provider_hooks, [
    {pre, [
        {compile, {erlydtl, compile}},
        {compile, {kura, compile}}
    ]}
]}.
```

Before every `rebar3 compile`, `rebar3 kura compile` now compares your schema modules with the migrations you already have and writes a new migration for the difference. The [next chapter](schemas-migrations.md) is all about it.

## PostgreSQL with Docker Compose

The blog keeps PostgreSQL in a container. `docker-compose.yml` in the project root:

```yaml
services:
  postgres:
    image: postgres:17-alpine
    ports:
      - "5432:5432"
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: blog_dev
    volumes:
      - pgdata:/var/lib/postgresql/data

volumes:
  pgdata:
```

```shell
docker compose up -d
```

## Configuring the repo

Connection settings live in the `kura` application environment, keyed by repo module. In `config/dev_sys.config.src`:

```erlang
{kura, [
    {repos, #{
        blog_repo => #{
            backend => kura_backend_postgres,
            host => "localhost",
            port => 5432,
            database => "blog_dev",
            user => "postgres",
            password => "postgres",
            pool_size => 10
        }
    }}
]}
```

`backend` selects the database. `kura_backend_postgres` comes from the `kura_postgres` package and brings the PostgreSQL dialect, the minato pool and the driver. The keys are `host` and `user`; you may find `hostname`/`username` in older examples, which belong to the legacy per-app form.

`config/test_sys.config` has the same block with `database => "blog_test"` and a smaller pool, so tests never touch your development data. Production reads its settings from the environment; see the [Configuration](../production/configuration.md) chapter.

````admonish tip title="A second backend: SQLite"
The `repos` map can hold more than one repo, each with its own backend. With the `kura_sqlite` package (`{kura_sqlite, "~> 1.0"}`) you could add a local analytics store next to PostgreSQL:

```erlang
{repos, #{
    blog_repo => #{backend => kura_backend_postgres, ...},
    analytics_repo => #{
        backend => kura_backend_sqlite,
        database => ~":memory:"
    }
}}
```

`analytics_repo` is a second repo module generated the same way. Each repo compiles queries with its own SQL dialect. The blog only uses PostgreSQL.
````

## Running migrations at boot

The blog runs pending migrations when it starts, before the supervisor. `src/blog_app.erl`:

```erlang
start(_StartType, _StartArgs) ->
    {ok, _} = kura_migrator:migrate(blog_repo),
    ok = nova_handlers:register_handler(text, fun blog_text_handler:handle/3),
    blog_sup:start_link().
```

(The `register_handler` line is from the [Controllers](../foundations/controllers.md) chapter.) `kura_migrator:migrate/1` creates the database if it does not exist yet, then runs every migration in `src/migrations/` that has not been applied and records it in a `schema_migrations` table. Matching on `{ok, _}` means the blog refuses to start against a database it cannot migrate, which is what you want.

```admonish tip
Migrating on boot is convenient in development and on a single node. When several nodes boot at once, Kura takes a PostgreSQL advisory lock so only one of them migrates while the others wait. Set a finite `migration_lock_timeout` (`kura` application env, in milliseconds) in production so a stuck lock fails the boot instead of hanging it. You can also run migrations as a separate release step; see [Deployment](../production/deployment.md).
```

## Verifying the connection

Start a shell:

```shell
rebar3 shell
```

Among the boot messages you will see the migrator at work once there are migrations (we write the first one in the next chapter):

```
[info] Kura: up migration 20261103072001 (m20261103072001_update_schema)
```

Then ask PostgreSQL something:

```erlang
1> blog_repo:query(~"SELECT 1 AS result", []).
{ok,[#{result => 1}]}
```

`query/2` takes SQL with `$1`-style placeholders and a parameter list, and returns `{ok, Rows}` where each row is a map with atom keys. All of Kura's query functions return rows in this shape.

---

The blog is connected. Next we describe the data in [Schemas and Migrations](schemas-migrations.md) and let Kura write the SQL.
