# Transactions and Bulk Operations

Some changes only make sense together. Publishing a post and tagging it should both happen or neither. Importing twenty posts from an old blog should not leave ten behind when the eleventh is invalid. This chapter covers transactions, `kura_multi` and the bulk functions that change many rows in one statement.

## Transactions

`blog_repo:transaction/1` runs a function inside a database transaction and returns whatever the function returns. If the function raises, the transaction is rolled back and the exception propagates.

The import from the previous chapters, for a whole list of posts:

```erlang
import_posts(Author, PostsParams) ->
    try blog_repo:transaction(fun() -> [import_or_throw(Author, P) || P <- PostsParams] end) of
        Imported -> {ok, Imported}
    catch
        throw:{import_failed, CS} -> {error, CS}
    end.

import_or_throw(Author, Params) ->
    case import_post(Author, Params) of
        {ok, Post} -> Post;
        {error, CS} -> throw({import_failed, CS})
    end.
```

The first invalid post throws, which rolls back every post inserted before it. The `try` turns the throw back into an error tuple for the caller.

```erlang
1> blog_repo:count(kura_query:from(blog_post)).
{ok,6}
2> {error, ImportCS} = blog_posts:import_posts(Al, [#{~"title" => ~"One", ~"body" => ~"1"}, #{~"title" => ~"Two"}]).
...
3> blog_api_json:errors(ImportCS).
#{body => [<<"can't be blank">>]}
4> blog_repo:count(kura_query:from(blog_post)).
{ok,6}
```

"One" was inserted and then rolled back.

````admonish warning title="Returning an error does not roll back"
Only an exception rolls back. A function that returns `{error, Reason}` commits whatever it did before returning. From `test/blog_data_tx_SUITE.erl`:

```erlang
{error, kept} = blog_repo:transaction(fun() ->
    {ok, _} = blog_posts:create_post(Alice, #{~"title" => ~"Committed", ~"body" => ~"x"}),
    {error, kept}
end),
{ok, 1} = blog_repo:count(kura_query:from(blog_post)).
```

Throw (as `import_posts/2` does) or use `kura_multi`, which handles this for you.
````

## Multi: named steps

`kura_multi` describes a transaction as data: a list of named steps, each of which may use the results of earlier steps. The repo runs them in order in one transaction and stops at the first failure. Publishing a post with its tags:

```erlang
publish(Post, TagNames) ->
    Multi = kura_multi:new(),
    Multi1 = kura_multi:update(Multi, post, blog_post:changeset(Post, #{status => published})),
    Multi2 = kura_multi:run(Multi1, tagged, fun(#{post := Published}) ->
        tag_post(Published, TagNames)
    end),
    case blog_repo:multi(Multi2) of
        {ok, #{tagged := Tagged}} -> {ok, Tagged};
        {error, Step, Reason, _Completed} -> {error, {Step, Reason}}
    end.
```

- `kura_multi:update(Multi, Name, Changeset)` adds an update step. `insert/3` and `delete/3` work the same, and each also accepts a function from the results so far to a changeset.
- `kura_multi:run(Multi, Name, Fun)` adds a step that runs any code. `Fun` gets a map of the results so far and returns `{ok, Value}` or `{error, Reason}`. Here it receives the published post and tags it, reusing `tag_post/2`. The function is a closure because it needs `TagNames`.
- On success you get `{ok, #{post => ..., tagged => ...}}`. On failure, `{error, FailedStep, Reason, Completed}`, and everything is rolled back, including the steps in `Completed`.

A bad tag name fails the `tagged` step, and the post stays a draft:

```erlang
5> {ok, Ready} = blog_posts:create_post(Al, #{~"title" => ~"Ready to ship", ~"body" => ~"Done."}).
...
6> blog_posts:publish(Ready, [~"Bad Tag"]).
{error,
    {tagged,
        #kura_changeset{
            valid = false,schema = blog_tag,data = #{},
            params = #{name => <<"Bad Tag">>},
            changes = #{name => <<"Bad Tag">>},
            errors = [{name,<<"has invalid format">>}],
            ...}}}
7> maps:get(status, element(2, blog_repo:get(blog_post, maps:get(id, Ready)))).
draft
8> {ok, Shipped} = blog_posts:publish(Ready, [~"release"]).
...
9> maps:with([status, published_at, tags], Shipped).
#{status => published,
  tags =>
      [#{id => 5,name => <<"release">>,
         inserted_at => {{2026,11,3},{7,49,14}}}],
  published_at => {{2026,11,3},{7,49,14}}}
```

`kura_multi:append/2` concatenates two multis, so you can build the steps in different functions and run them together.

## Bulk operations

Changesets are for rows that come from users. For maintenance work across many rows, Kura has functions that compile to a single statement. They skip changesets, validations and lifecycle hooks, so use them for data you control.

### insert_all

```erlang
{ok, 3} = blog_repo:insert_all(blog_tag, [#{name => N} || N <- [~"erlang", ~"elixir", ~"gleam"]]).
```

Rows are maps of field values. Kura fills in the timestamp fields and returns the number of rows inserted. `ensure_tags/1` in the [previous chapter](advanced-data.md#tagging-posts-with-put_assoc) validates each name with the tag changeset and uses `apply_changes/1` to get the rows.

### update_all

Archive drafts nobody has touched for a while:

```erlang
archive_stale_drafts(Before) ->
    Query = kura_query:where(kura_query:from(blog_post), {status, ~"draft"}),
    Stale = kura_query:where(Query, {updated_at, '<', Before}),
    blog_repo:update_all(Stale, #{status => archived, updated_at => calendar:universal_time()}).
```

```erlang
10> blog_posts:archive_stale_drafts(calendar:universal_time()).
{ok,1}
```

The update map's values go through the field types, so the atom `archived` is fine here, unlike in `where`. `updated_at` is set explicitly because there is no changeset to do it.

### delete_all

Purge posts that were soft-deleted before a date (soft deletes are in the [next chapter](data-safety.md)):

```erlang
purge_deleted(Before) ->
    Query = kura_query:only_deleted(kura_query:from(blog_post)),
    blog_repo:delete_all(kura_query:where(Query, {deleted_at, '<', Before})).
```

Both `update_all/2` and `delete_all/1` return `{ok, Count}`.

## Upserts

An upsert inserts a row or, if it collides with a unique constraint, does something else. `on_conflict` takes `{Target, Action}`:

| Target | Meaning |
|---|---|
| `Field` or `[Fields]` | the columns of a unique index |
| `{constraint, ~"name"}` | a named constraint |

| Action | On conflict |
|---|---|
| `nothing` | skip the row |
| `replace_all` | overwrite every column except the conflict target |
| `{replace, [Fields]}` | overwrite only these columns |

With `insert_all/3`, skipping existing tags:

```erlang
11> blog_repo:insert_all(blog_tag, [#{name => ~"kura"}, #{name => ~"gleam"}], #{on_conflict => {name, nothing}}).
{ok,1}
```

`kura` existed, so only `gleam` was inserted. `insert/2` takes the same options for a single changeset. With `returning => true` it returns the row as stored; on a conflict with `replace` that is the existing row, updated, so the id stays the same. From the tests:

```erlang
{ok, #{id := Id}} = blog_repo:insert(blog_tag:changeset(#{}, #{name => ~"erlang"})),
CS = blog_tag:changeset(#{}, #{name => ~"erlang"}),
{ok, #{id := Id}} = blog_repo:insert(CS, #{
    on_conflict => {name, {replace, [inserted_at]}}, returning => true
}),
```

```admonish note
`insert/2` with options takes a different code path from `insert/1`: it does not run lifecycle hooks and does not insert `cast_assoc`/`put_assoc` children. The blog only upserts tags, which have neither.
```

## Testing transactions

`test/blog_data_tx_SUITE.erl` pins every behaviour in this chapter: the transaction result, rollback on exception, commit on returned error, a successful and a failed multi, bulk inserts, both upsert forms, `update_all` and `delete_all`. Inside `kura_sandbox`, a transaction runs as a savepoint on the test's connection, so a rolled-back transaction behaves exactly as it would outside the sandbox, and everything is still rolled back when the test checks in.

---

Next: protecting data from mistakes and leaks in [Data Safety](data-safety.md).
