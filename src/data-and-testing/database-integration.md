# Database Integration

Nova does not include a built-in ORM or database layer — by design, you choose whatever database and driver fits your project. In this chapter we will integrate PostgreSQL using [pgo](https://github.com/erleans/pgo) and structure our data access code.

## Why pgo?

pgo is a PostgreSQL client for Erlang with built-in connection pooling. You don't need a separate pool library — configure a pool in `sys.config`, call `pgo:query/2`, and pgo handles checkout, checkin, and reconnection for you.

## Adding the dependency

Add `pgo` to `rebar.config`:

```erlang
{deps, [
        nova,
        {flatlog, "0.1.2"},
        pgo
       ]}.
```

Also add it to your application dependencies in `src/my_first_nova.app.src`:

```erlang
{applications,
 [kernel,
  stdlib,
  nova,
  pgo
 ]},
```

## Database configuration

Configure the pgo pool in `dev_sys.config.src`:

```erlang
{pgo, [
    {pools, [
        {default, #{
            pool_size => 10,
            host => "localhost",
            port => 5432,
            database => "my_first_nova_dev",
            user => "postgres",
            password => "postgres"
        }}
    ]}
]}
```

The `default` pool is used automatically when you call `pgo:query/2` without specifying a pool name. That is it for setup — no gen_server, no supervision tree changes. pgo starts its own pool supervisor when the application boots.

```admonish info title="Database setup"
Create the database before continuing:

~~~sql
CREATE DATABASE my_first_nova_dev;
\c my_first_nova_dev

CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    inserted_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);
~~~
```

## Creating a repository module

Create `src/my_first_nova_user_repo.erl` to encapsulate data access:

```erlang
-module(my_first_nova_user_repo).
-export([
         all/0,
         get/1,
         create/2,
         update/3,
         delete/1
        ]).

all() ->
    case pgo:query("SELECT id, name, email FROM users ORDER BY id") of
        #{rows := Rows} ->
            {ok, [row_to_map(Row) || Row <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end.

get(Id) ->
    case pgo:query("SELECT id, name, email FROM users WHERE id = $1", [Id]) of
        #{rows := [Row]} ->
            {ok, row_to_map(Row)};
        #{rows := []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

create(Name, Email) ->
    case pgo:query("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
                   [Name, Email]) of
        #{rows := [Row]} ->
            {ok, row_to_map(Row)};
        {error, Reason} ->
            {error, Reason}
    end.

update(Id, Name, Email) ->
    case pgo:query("UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email",
                   [Name, Email, Id]) of
        #{rows := [Row]} ->
            {ok, row_to_map(Row)};
        #{rows := []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

delete(Id) ->
    case pgo:query("DELETE FROM users WHERE id = $1", [Id]) of
        #{command := delete, num_rows := 1} -> ok;
        #{num_rows := 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

row_to_map({Id, Name, Email}) ->
    #{id => Id, name => Name, email => Email}.
```

`pgo:query/1` and `pgo:query/2` handle connection pooling transparently. Results are maps with a `rows` key containing tuples.

## Using the repo in controllers

Update the API controller to use real data:

```erlang
-module(my_first_nova_api_controller).
-export([
         index/1,
         show/1,
         create/1,
         update/1,
         delete/1
        ]).

index(_Req) ->
    {ok, Users} = my_first_nova_user_repo:all(),
    {json, #{users => Users}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_user_repo:get(binary_to_integer(Id)) of
        {ok, User} ->
            {json, User};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"user not found">>}}
    end.

create(#{params := #{<<"name">> := Name, <<"email">> := Email}}) ->
    case my_first_nova_user_repo:create(Name, Email) of
        {ok, User} ->
            {json, 201, #{}, User};
        {error, Reason} ->
            {status, 422, #{}, #{error => list_to_binary(io_lib:format("~p", [Reason]))}}
    end;
create(_Req) ->
    {status, 422, #{}, #{error => <<"name and email required">>}}.

update(#{bindings := #{<<"id">> := Id},
         params := #{<<"name">> := Name, <<"email">> := Email}}) ->
    case my_first_nova_user_repo:update(binary_to_integer(Id), Name, Email) of
        {ok, User} ->
            {json, User};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"user not found">>}}
    end.

delete(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_user_repo:delete(binary_to_integer(Id)) of
        ok ->
            {status, 204};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"user not found">>}}
    end.
```

Add the new routes:

```erlang
#{prefix => "/api",
  security => false,
  routes => [
             {"/users", fun my_first_nova_api_controller:index/1, #{methods => [get]}},
             {"/users/:id", fun my_first_nova_api_controller:show/1, #{methods => [get]}},
             {"/users", fun my_first_nova_api_controller:create/1, #{methods => [post]}},
             {"/users/:id", fun my_first_nova_api_controller:update/1, #{methods => [put]}},
             {"/users/:id", fun my_first_nova_api_controller:delete/1, #{methods => [delete]}}
            ]
}
```

## Named pools

For multiple databases or separate workloads:

```erlang
{pgo, [
    {pools, [
        {default, #{pool_size => 10, host => "localhost", database => "my_first_nova_dev",
                     user => "postgres", password => "postgres"}},
        {readonly, #{pool_size => 5, host => "localhost", database => "my_first_nova_dev",
                      user => "readonly_user", password => "readonly_pass"}}
    ]}
]}
```

Query a specific pool:

```erlang
pgo:query(readonly, "SELECT count(*) FROM users", []).
```

## Transactions

pgo supports transactions — if any query fails, the whole thing rolls back:

```erlang
pgo:transaction(fun() ->
    pgo:query("INSERT INTO users (name, email) VALUES ($1, $2)", [Name, Email]),
    pgo:query("INSERT INTO audit_log (action, target) VALUES ($1, $2)", [<<"create_user">>, Email])
end).
```

## Other databases

The repository pattern works for any database. Popular Erlang database drivers:

- **PostgreSQL**: `pgo`, `epgsql`
- **MySQL**: `mysql-otp`
- **Redis**: `eredis`
- **Mnesia**: built into OTP, no external dependency
- **SQLite**: `esqlite`

---

Now that we have data persistence, let's learn how to [test](testing.md) our Nova application.
