# Data Safety

A blog loses data in boring ways: someone deletes the wrong post, two editors overwrite each other, an admin removes the last admin, a database dump leaks. This chapter adds the guard rails: optimistic locking, soft deletes, lifecycle hooks, an audit log and encryption at rest.

## The schema changes

Posts get two fields and users one. In `blog_post:fields/0`:

```erlang
#kura_field{name = lock_version, type = integer, nullable = false, default = 1},
#kura_field{name = deleted_at, type = utc_datetime},
```

In `blog_user:fields/0`:

```erlang
#kura_field{name = full_name, type = {encrypted, string}},
```

The compile hook generates `m20261103073431_update_schema.erl`:

```erlang
up() ->
    [
        {alter_table, ~"posts", [
            {add_column, #kura_column{name = deleted_at, type = utc_datetime}},
            {add_column, #kura_column{
                name = lock_version, type = integer, nullable = false, default = 1
            }}
        ]},
        {alter_table, ~"users", [
            {add_column, #kura_column{name = full_name, type = {encrypted, string}}}
        ]}
    ].
```

## Optimistic locking

Alice and Bob both open a draft. Alice saves, then Bob saves his older copy, and Alice's change is silently lost. **Optimistic locking** catches this: every update checks that `lock_version` still has the value the editor started from, and increments it.

`update_post/2` takes the version the client last saw from the params:

```erlang
update_post(Post, Params) ->
    Expected = expected_version(Post, Params),
    CS = blog_post:changeset(Post#{lock_version => Expected}, Params),
    blog_repo:update(kura_changeset:optimistic_lock(CS, lock_version)).

expected_version(#{lock_version := Current}, Params) ->
    case kura_types:cast(integer, maps:get(~"lock_version", Params, Current)) of
        {ok, Version} when is_integer(Version) -> Version;
        _ -> Current
    end.
```

`optimistic_lock(CS, lock_version)` adds `WHERE lock_version = Expected` to the update and sets the column to `Expected + 1`. If no row matches, someone else got there first and `update/1` returns `{error, stale}`. A client that sends no `lock_version` is checked against the current version, so it always wins; send it to opt in.

The controller maps `stale` to 409 Conflict:

```erlang
case blog_posts:update_post(Post, Params) of
    {ok, Updated} -> {json, blog_api_json:post(Updated)};
    {error, stale} -> {json, 409, #{}, #{error => ~"post was changed by someone else"}};
    {error, #kura_changeset{} = CS} -> unprocessable(CS)
end;
```

and `blog_api_json:post/1` renders `lock_version => maps:get(lock_version, Post)` so clients know what to send back.

```shell
curl -s -X PUT localhost:8080/api/posts/1 -H 'content-type: application/json' -d '{"title": "Hello again, Kura", "lock_version": 1}'
```

```json
{"id":1,"status":"draft","title":"Hello again, Kura","metadata":null,"body":"Kura is the data layer of the blog.","user_id":1,"inserted_at":"2026-11-03T07:44:22Z","published_at":null,"lock_version":2}
```

The same request again, from someone still holding version 1:

```shell
curl -s -i -X PUT localhost:8080/api/posts/1 -H 'content-type: application/json' -d '{"title": "Lost update", "lock_version": 1}'
```

```
HTTP/1.1 409 Conflict
...

{"error":"post was changed by someone else"}
```

## Soft deletes

A schema with a `deleted_at` field supports **soft deletes**: deleting sets the timestamp instead of removing the row, and normal queries stop seeing it. Add the two repo functions to `blog_repo.erl`:

```erlang
soft_delete(CS) -> kura_repo_worker:soft_delete(?MODULE, CS).
restore(CS) -> kura_repo_worker:restore(?MODULE, CS).
```

The posts context switches `delete_post/1` over and gains a restore:

```erlang
delete_post(Post) ->
    blog_repo:soft_delete(kura_changeset:cast(blog_post, Post, #{}, [])).

restore_post(Id) ->
    Query = kura_query:only_deleted(kura_query:where(kura_query:from(blog_post), {id, Id})),
    case blog_repo:one(Query) of
        {ok, Post} -> blog_repo:restore(kura_changeset:cast(blog_post, Post, #{}, []));
        {error, not_found} -> {error, not_found}
    end.
```

The controller does not change: `DELETE /api/posts/:id` still answers 204, and the post is gone from the API.

```shell
curl -s -i -X DELETE localhost:8080/api/posts/1
```

```
HTTP/1.1 204 No Content
...
```

```shell
curl -s -i localhost:8080/api/posts/1
```

```
HTTP/1.1 404 Not Found
...

{"error":"post not found"}
```

The row is still there:

```
blog_dev=# SELECT id, deleted_at IS NOT NULL AS deleted, lock_version FROM posts ORDER BY id;
 id | deleted | lock_version
----+---------+--------------
  1 | t       |            2
  2 | f       |            1
```

and can be restored:

```erlang
1> blog_posts:restore_post(1).
{ok,#{id => 1,status => draft,title => <<"Hello again, Kura">>,
      ...
      deleted_at => undefined}}
```

`all/1`, `one/1`, `get/2`, `count/1`, `aggregate/2`, `kura_paginator:paginate/3`, `kura_stream:stream/3,4`, `update_all/2` and `delete_all/1` add `deleted_at IS NULL` automatically. Wrap a query in `kura_query:with_deleted/1` to see every row, or `kura_query:only_deleted/1` for the deleted ones, as `restore_post/1` and `purge_deleted/1` from the [previous chapter](transactions-bulk.md#delete_all) do.

````admonish warning title="Raw SQL and nested queries see deleted rows"
The filter is added for the schema in the outer query's `from` only. A query inside a CTE or a union, and raw SQL through `query/2`, see every row. That is why the CTEs in `blog_stats` start from `live_posts()`, which adds `{deleted_at, is_nil}` itself, and why the raw SQL in `top_commented/1` says `WHERE p.deleted_at IS NULL`. `test/blog_data_safety_SUITE.erl` checks the automatic filter right after soft-deleting the only post:

```erlang
{ok, []} = blog_repo:all(kura_query:from(blog_post)),
{ok, 0} = blog_repo:count(kura_query:from(blog_post)),
{ok, 1} = blog_repo:count(kura_query:with_deleted(kura_query:from(blog_post))),
```
````

## Lifecycle hooks

A schema can export hook callbacks that run around every write:

| Callback | Gets | Returns |
|---|---|---|
| `before_insert/1`, `before_update/1` | the changeset | `{ok, CS}` or `{error, CS}` |
| `after_insert/1`, `after_update/1` | the stored row | `{ok, Row}` or `{error, Reason}` |
| `before_delete/1` | the record | `ok` or `{error, Reason}` |
| `after_delete/1` | the deleted record | `ok` or `{error, Reason}` |

A `before_*` hook that returns an error stops the write. When a schema has after hooks, Kura runs the write and the hook in one transaction, so an error from the hook rolls the write back.

The blog never wants to end up without an admin. `blog_user.erl`:

```erlang
-export([table/0, fields/0, indexes/0, associations/0, before_delete/1]).

before_delete(#{role := admin, id := Id}) ->
    Others = kura_query:where(
        kura_query:from(?MODULE), {'and', [{role, ~"admin"}, {id, '!=', Id}]}
    ),
    case blog_repo:exists(Others) of
        {ok, true} -> ok;
        {ok, false} -> {error, last_admin}
    end;
before_delete(_User) ->
    ok.
```

From the tests:

```erlang
{ok, Admin} = blog_repo:update(blog_user:role_changeset(Alice, #{role => ~"admin"})),
{error, last_admin} = blog_repo:delete(kura_changeset:cast(blog_user, Admin, #{}, [])),
```

Hooks run for `insert/1`, `update/1` and `delete/1`. They do not run for the bulk functions, and not for `insert/2` with options.

## An audit log

`kura_audit` records who changed what, as rows in an `audit_log` table. The table is the one migration in the blog that is not generated: `kura_audit` ships its operations, and a hand-written migration, `src/migrations/m20261103073500_create_audit_log.erl`, calls them:

```erlang
-module(m20261103073500_create_audit_log).
-moduledoc false.
-behaviour(kura_migration).
-export([up/0, down/0]).

-spec up() -> [kura_migration:operation()].
up() -> kura_audit:migration_up().

-spec down() -> [kura_migration:operation()].
down() -> kura_audit:migration_down().
```

Give it a timestamp later than your newest migration. Posts are audited from their hooks:

```erlang
-export([before_update/1, after_insert/1, after_update/1, after_delete/1]).

before_update(CS) ->
    kura_audit:stash(CS),
    {ok, CS}.

after_insert(Post) ->
    kura_audit:log(blog_repo, ?MODULE, insert, Post),
    {ok, Post}.

after_update(Post) ->
    kura_audit:log(blog_repo, ?MODULE, update, Post),
    {ok, Post}.

after_delete(Post) ->
    kura_audit:log(blog_repo, ?MODULE, delete, Post).
```

`stash/1` remembers the row as it was before the update, so `log/4` can store the old values next to the new ones. `kura_audit:set_actor(~"alice")` sets who is acting for the current process; once Part III has a logged-in user, the security functions set it per request ([Authentication](../auth-sessions/authentication.md#security-functions)). After the curl session above:

```
blog_dev=# SELECT table_name, record_id, action, changes FROM audit_log ORDER BY id;
 table_name | record_id | action |                                              changes
------------+-----------+--------+----------------------------------------------------------------------------------------------------
 posts      | 1         | insert |
 posts      | 2         | insert |
 posts      | 1         | update | {"title": {"new": "Hello again, Kura", "old": "Hello Kura"}, "lock_version": {"new": 2, "old": 1}}
 posts      | 1         | update | {"deleted_at": {"new": "2026-11-03T07:44:22Z", "old": null}}
```

A soft delete is an update of `deleted_at`, and is logged as one. The audit rows can be queried like any other schema: `kura_query:from(kura_audit_log)`.

## Encryption at rest

A user's full name is personal data the blog never searches on. Declared as `{encrypted, string}`, it is encrypted with AES-256-GCM before it leaves the node and decrypted when it is loaded. The column is `BYTEA`, and a stolen backup only contains ciphertext.

Kura reads the keys from its application environment. In `config/dev_sys.config.src`, next to `repos`:

```erlang
%% Development-only key. Generate your own: base64 of 32 random bytes.
{encryption, #{
    active => 1,
    keys => [{1, ~"ec/kfyy6LwSTNrK7oGj3PkEc8KhsmcIRcC43z0kl89Q="}]
}}
```

Each key is a base64-encoded 32-byte value with an id. `active` is the key used for new writes. Every ciphertext starts with a format version byte and the id of the key that encrypted it (the `\x0101` in the dump below), so you rotate by adding a key and moving `active` to it while old rows remain readable. Generate a key in the shell:

```erlang
1> base64:encode(crypto:strong_rand_bytes(32)).
```

The test config has its own key. Production must never use a key from a file in the repository: read it from the environment in `config/prod_sys.config.src`, or implement the `kura_keyring` behaviour and set `{kura, [{keyring, my_keyring}]}` to fetch keys from a secrets manager.

`full_name` joins the registration and profile changesets:

```erlang
CS = kura_changeset:cast(?MODULE, Data, Params, [
    email, username, full_name, password, password_confirmation
]),
```

```erlang
CS = kura_changeset:cast(?MODULE, Data, Params, [username, full_name]),
```

In Erlang the field is a normal binary:

```erlang
2> maps:with([id, username, role, full_name, hashed_password], Alice).
#{id => 1,role => author,username => <<"alice">>,
  hashed_password =>
      <<"$pbkdf2-sha256$600000$NyP/2YvxTsToa6elnvDwvg==$1BrPIaLtuZlwYwPpNT71X0XAU5NHXsdDOMcqXAEJNF4=">>,
  full_name => <<"Alice Andersson">>}
```

In the database it is not:

```
blog_dev=# SELECT id, username, full_name FROM users;
 id | username |                                          full_name
----+----------+----------------------------------------------------------------------------------------------
  1 | alice    | \x0101f7a0b0eece043b775c2d9792cc8dc29a91c6921e1b6ef2f295277746b172e1c1881add2500ce5a19ddbc22
  2 | bob      |
```

The same name encrypts to different bytes every time, so the database cannot compare encrypted values. Kura refuses to build a query that would try:

```erlang
3> kura_query:where(kura_query:from(blog_user), {full_name, ~"Alice Andersson"}).
** exception error: {kura,{encrypted_field_in_where,full_name}}
     in function  kura_query:refuse_field/3 (.../kura/src/kura_query.erl:750)
     ...
```

Encrypt fields you only display, never fields you look up by. `email` stays plain because login looks users up by it.

## What else Kura can do

Part II used most of Kura, but not all of it. Worth knowing about:

- **Row locks**: `kura_query:lock(Q, ~"FOR UPDATE")` inside a transaction, for read-modify-write sequences.
- **Multi-tenancy**: `kura_tenant` and `kura_query:prefix/2` run queries against a PostgreSQL schema per tenant.
- **Custom types**: `{custom, Module}` with the `kura_type` behaviour, for values Kura does not know.
- **Vectors**: `vector` fields and `kura_query:distance/3` for pgvector similarity search.
- **Introspection**: `rebar3 kura gen_schemas` writes schema modules from an existing database.
- **Telemetry**: Kura emits telemetry events for every query; `opentelemetry_kura` turns them into traces (see [Observability](../production/opentelemetry.md)).
- **Data migrations**: a migration can export `transactional() -> false` for operations PostgreSQL cannot run in a transaction, like `CREATE INDEX CONCURRENTLY`.

## Testing

`test/blog_data_safety_SUITE.erl` covers soft delete and restore, stale updates in the context and over HTTP, the audit trail, the last-admin hook, ciphertext at rest and the refused query on an encrypted field. With the other suites from this Part:

```shell
rebar3 ct --suite=test/blog_data_changeset_SUITE,test/blog_data_crud_SUITE,test/blog_data_assoc_SUITE,test/blog_data_advanced_data_SUITE,test/blog_data_query_SUITE,test/blog_data_tx_SUITE,test/blog_data_safety_SUITE
```

---

The blog has a data layer. In Part III, users get to log in: [Sessions](../auth-sessions/sessions.md).
