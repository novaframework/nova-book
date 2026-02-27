# Database Setup

Nova does not include a built-in database layer — by design, you choose what fits your project. We will use [Kura](https://github.com/Taure/kura), an Ecto-inspired database abstraction for Erlang that targets PostgreSQL. Kura gives you schemas, changesets, a query builder, and migrations — no raw SQL required.

## Adding dependencies

Add `kura` and the `rebar3_kura` plugin to `rebar.config`:

```erlang
{deps, [
        nova,
        {flatlog, "0.1.2"},
        {kura, "~> 1.0"}
       ]}.

{plugins, [
    rebar3_nova,
    {rebar3_kura, "~> 0.5"}
]}.
```

Also add `kura` to your application dependencies in `src/blog.app.src`:

```erlang
{applications,
 [kernel,
  stdlib,
  nova,
  kura
 ]},
```

## Setting up the repository

The `rebar3_kura` plugin provides a setup command that generates a repository module:

```shell
rebar3 kura setup --name blog_repo
```

This creates `src/blog_repo.erl` — a module that wraps all database operations:

```erlang
-module(blog_repo).
-behaviour(kura_repo).

-export([config/0, start/0, all/1, get/2, get_by/2, one/1,
         insert/1, insert/2, update/1, delete/1,
         update_all/2, delete_all/1, insert_all/2,
         preload/3, transaction/1, multi/1, query/2]).

config() ->
    Database = application:get_env(blog, database, <<"blog_dev">>),
    #{pool => ?MODULE,
      database => Database,
      hostname => <<"localhost">>,
      port => 5432,
      username => <<"postgres">>,
      password => <<"postgres">>,
      pool_size => 10}.

start() -> kura_repo_worker:start(?MODULE).
all(Q) -> kura_repo_worker:all(?MODULE, Q).
get(Schema, Id) -> kura_repo_worker:get(?MODULE, Schema, Id).
get_by(Schema, Clauses) -> kura_repo_worker:get_by(?MODULE, Schema, Clauses).
one(Q) -> kura_repo_worker:one(?MODULE, Q).
insert(CS) -> kura_repo_worker:insert(?MODULE, CS).
insert(CS, Opts) -> kura_repo_worker:insert(?MODULE, CS, Opts).
update(CS) -> kura_repo_worker:update(?MODULE, CS).
delete(CS) -> kura_repo_worker:delete(?MODULE, CS).
update_all(Q, Updates) -> kura_repo_worker:update_all(?MODULE, Q, Updates).
delete_all(Q) -> kura_repo_worker:delete_all(?MODULE, Q).
insert_all(Schema, Entries) -> kura_repo_worker:insert_all(?MODULE, Schema, Entries).
preload(Schema, Records, Assocs) -> kura_repo_worker:preload(?MODULE, Schema, Records, Assocs).
transaction(Fun) -> kura_repo_worker:transaction(?MODULE, Fun).
multi(Multi) -> kura_repo_worker:multi(?MODULE, Multi).
query(SQL, Params) -> kura_repo_worker:query(?MODULE, SQL, Params).
```

The `kura_repo` behaviour only requires one callback — `config/0` — which tells Kura how to connect to PostgreSQL. Every other function is a convenience delegation to `kura_repo_worker`.

The setup command also creates `src/migrations/` for migration files.

## PostgreSQL with Docker Compose

Create `docker-compose.yml` in your project root:

```yaml
services:
  db:
    image: postgres:16
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: blog_dev
    ports:
      - "5432:5432"
    volumes:
      - pgdata:/var/lib/postgresql/data

volumes:
  pgdata:
```

Start it:

```shell
docker compose up -d
```

## Configuring the repo

Notice that `config/0` uses `application:get_env(blog, database, <<"blog_dev">>)` for the database name. This means you can override it per environment through `sys.config` without touching the module.

The `blog_dev` default in `config/0` works without any sys.config entry. If you ever need a separate database for production or CI, override it with an application environment variable:

```erlang
{blog, [
       {database, <<"blog_prod">>}
      ]}
```

## Starting the repo in the supervisor

The repo needs to be started when your application boots. Add it to your supervisor in `src/blog_sup.erl`:

```erlang
-module(blog_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    blog_repo:start(),
    kura_migrator:migrate(blog_repo),
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, []}}.
```

`blog_repo:start()` creates the pgo connection pool using the config from `config/0`. `kura_migrator:migrate/1` then runs any pending migrations — it tracks which versions have been applied in a `schema_migrations` table.

```admonish tip
Auto-migrating on startup is convenient during development. For production, run migrations as a separate step before deploying (e.g. a release command or CI job) so that failures don't prevent the application from starting.
```

## Adding the rebar3_kura compile hook

To get automatic migration generation (covered in the next chapter), add a provider hook to `rebar.config`:

```erlang
{provider_hooks, [
    {pre, [{compile, {kura, compile}}]}
]}.
```

This runs `rebar3 kura compile` before every `rebar3 compile`, scanning your schemas and generating migrations for any changes.

## Verifying the connection

Start the development server:

```shell
rebar3 nova serve
```

You should see the application start without errors. If the database is unreachable, you will see a connection error in the logs. Verify from the shell:

```erlang
1> blog_repo:query("SELECT 1 AS result", []).
{ok, [#{result => 1}]}
```

`query/2` returns `{ok, Rows}` where each row is a map with atom keys — the same format you will see from all Kura query functions.

---

Now let's define our first schemas and watch Kura generate migrations automatically in [Schemas and Migrations](schemas-migrations.md).
