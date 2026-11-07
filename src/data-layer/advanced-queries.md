# Advanced Queries

The blog needs more than "list posts": counts for a dashboard, an activity feed, search, exports. All of this still goes through `kura_query` and stays parameterised. The queries in this chapter live in `src/blog_stats.erl`, with two helpers at the bottom of the module:

```erlang
published() ->
    kura_query:where(kura_query:from(blog_post), {status, ~"published"}).

live_posts() ->
    kura_query:where(kura_query:from(blog_post), {deleted_at, is_nil}).
```

`deleted_at` arrives with soft deletes in the [Data Safety](data-safety.md) chapter. Kura hides soft-deleted rows by itself, but only for the schema in the outer query's `from`. A query nested inside a CTE or a union is not checked, so those start from `live_posts()`, which filters explicitly.

The outputs were captured on a development database with five posts by Alice and Bob and a handful of comments.

## Aggregates

```erlang
published_count() ->
    blog_repo:count(published()).

latest_publication() ->
    blog_repo:aggregate(published(), {max, published_at}).
```

`count/1` and `aggregate/2` are two of the lines we add to `blog_repo` (see [Database Setup](setup.md#generating-the-repo-with-rebar3-kura-setup)):

```erlang
count(Q) -> kura_repo_worker:count(?MODULE, Q).
aggregate(Q, Agg) -> kura_repo_worker:aggregate(?MODULE, Q, Agg).
```

```erlang
1> blog_stats:published_count().
{ok,4}
2> blog_stats:latest_publication().
{ok,{{2026,11,3},{7,44,43}}}
```

`aggregate/2` takes `{count, Field}`, `{sum, Field}`, `{avg, Field}`, `{min, Field}` or `{max, Field}`.

## Group by and having

Posts per author, only for authors with at least `MinPosts` posts:

```erlang
posts_per_author(MinPosts) ->
    Q = kura_query:group_by(kura_query:from(blog_post), [user_id]),
    Q1 = kura_query:select(Q, [user_id, {count, '*'}]),
    Q2 = kura_query:having(Q1, {fragment, ~"count(*) >= ?", [MinPosts]}),
    blog_repo:all(kura_query:order_by(Q2, [{user_id, asc}])).
```

```erlang
3> blog_stats:posts_per_author(2).
{ok,[#{count => 2,user_id => 1},#{count => 2,user_id => 2}]}
```

`{count, '*'}` in a select becomes `count(*)` and shows up under the key `count`. The `having` condition is a **fragment**: raw SQL with `?` placeholders and a parameter list. Kura renumbers the placeholders into `$N` bind parameters, so `MinPosts` is never part of the SQL text.

## Joins

```erlang
post_titles_with_authors() ->
    Q = kura_query:join(kura_query:from(blog_post), inner, blog_user, {user_id, id}),
    Q1 = kura_query:select(Q, [title, username]),
    blog_repo:all(kura_query:order_by(Q1, [{title, asc}])).
```

`join(Query, Type, Schema, {LocalField, ForeignField})` joins on `posts.user_id = users.id`. The type is `inner`, `left`, `right` or `full`.

```erlang
4> blog_stats:post_titles_with_authors().
{ok,[#{title => <<"Changesets">>,username => <<"alice">>},
     #{title => <<"Pattern matching">>,username => <<"bob">>},
     #{title => <<"Postgres full text search">>,
       username => <<"bob">>},
     #{title => <<"Supervision trees">>,username => <<"alice">>}]}
```

After a join, Kura qualifies the columns of the `from` schema with its table, so names that both tables have (`id`, `inserted_at`) are not ambiguous. `select [id, title, username]` on the join above compiles to `SELECT "posts"."id", "posts"."title", "username" FROM "posts" INNER JOIN "users" ...`. A column that only the joined schema has, like `username`, stays bare. To select a joined column whose name the `from` schema also has, such as `users.inserted_at`, use a fragment with the qualified name.

When you only need the related rows, not one combined row, a preload is usually simpler than a join.

## Subqueries

`{Field, in, {subquery, Q}}` filters by the rows of another query. Posts written by admins:

```erlang
posts_by_admins() ->
    Admins = kura_query:select(kura_query:where(kura_query:from(blog_user), {role, ~"admin"}), [id]),
    blog_repo:all(kura_query:where(kura_query:from(blog_post), {user_id, in, {subquery, Admins}})).
```

`{exists, {subquery, Q}}` and `{not_exists, {subquery, Q}}` test for matching rows. To relate the subquery to the outer row, use a fragment:

```erlang
posts_without_comments() ->
    Comments = kura_query:where(
        kura_query:from(blog_comment),
        {fragment, ~"\"comments\".\"post_id\" = \"posts\".\"id\"", []}
    ),
    blog_repo:all(kura_query:where(kura_query:from(blog_post), {not_exists, {subquery, Comments}})).
```

```erlang
5> blog_stats:posts_without_comments().
{ok,[#{id => 4,status => published,title => <<"Pattern matching">>, ...},
     #{id => 5,status => published,
       title => <<"Postgres full text search">>, ...}]}
```

## Window functions and CTEs

"The latest post of each author" is a classic window function: number each author's posts newest first, keep number 1. `kura_query:over/2` builds the window expression, `select_expr/2` selects expressions under names of your choice, and `with_cte/3` wraps the ranked query in a common table expression so the outer query can filter on the rank:

```erlang
latest_post_per_author() ->
    Ranked = kura_query:select_expr(live_posts(), [
        {id, id},
        {user_id, user_id},
        {title, title},
        {position,
            kura_query:over(row_number, #{
                partition_by => [user_id], order_by => [{inserted_at, desc}]
            })}
    ]),
    Q = kura_query:with_cte(kura_query:from(~"ranked_posts"), ~"ranked_posts", Ranked),
    Q1 = kura_query:where(Q, {position, 1}),
    blog_repo:all(kura_query:order_by(Q1, [{user_id, asc}])).
```

```erlang
6> blog_stats:latest_post_per_author().
{ok,[#{id => 3,position => 1,title => <<"Supervision trees">>,
       user_id => 1},
     #{id => 4,position => 1,title => <<"Pattern matching">>,
       user_id => 2}]}
```

`from(~"ranked_posts")` selects from the CTE by name instead of from a schema. The rows come back as maps keyed by the names given in `select_expr/2`, with the values as PostgreSQL returned them: there is no schema to load them through. The outer query has no schema either, so Kura adds no soft-delete filter anywhere; that is why the CTE starts from `live_posts()`.

## Unions

An activity feed mixes posts and comments. Both sides of a union must select the same columns, so `select_expr/2` renames them, and a fragment adds a constant `kind` column:

```erlang
activity_feed(Limit) ->
    Posts = kura_query:select_expr(live_posts(), [
        {kind, {fragment, ~"'post'", []}}, {text, title}, {at, inserted_at}
    ]),
    Comments = kura_query:select_expr(kura_query:from(blog_comment), [
        {kind, {fragment, ~"'comment'", []}}, {text, body}, {at, inserted_at}
    ]),
    Feed = kura_query:with_cte(
        kura_query:from(~"feed"), ~"feed", kura_query:union_all(Posts, Comments)
    ),
    blog_repo:all(kura_query:limit(kura_query:order_by(Feed, [{at, desc}]), Limit)).
```

```erlang
7> blog_stats:activity_feed(4).
{ok,[#{at => {{2026,11,3},{7,44,43}},
       text => <<"Postgres full text search">>,kind => <<"post">>},
     #{at => {{2026,11,3},{7,44,43}},
       text => <<"Supervision trees">>,kind => <<"post">>},
     #{at => {{2026,11,3},{7,44,43}},
       text => <<"Pattern matching">>,kind => <<"post">>},
     #{at => {{2026,11,3},{7,44,43}},
       text => <<"Nice.">>,kind => <<"comment">>}]}
```

`union/2` removes duplicates, `union_all/2` keeps them; `intersect/2` and `except/2` work the same way.

## Distinct

Everyone who commented on a post, once each:

```erlang
commenters(#{id := PostId}) ->
    Q = kura_query:where(kura_query:from(blog_comment), {post_id, PostId}),
    blog_repo:all(kura_query:distinct(kura_query:select(Q, [user_id]))).
```

`distinct/2` takes a list of fields for PostgreSQL's `DISTINCT ON`.

## Fragments in where

Anything the condition syntax cannot express can be a fragment. Posts published in the last `Days` days, using PostgreSQL's date arithmetic:

```erlang
published_since(Days) ->
    blog_repo:all(
        kura_query:where(
            published(), {fragment, ~"published_at > now() - make_interval(days => ?)", [Days]}
        )
    ).
```

```admonish warning
The SQL in a fragment is sent as written. Put values in the parameter list, never into the SQL binary. A fragment built with string concatenation from user input is an SQL injection.
```

## Full-text search

`{Field, matches, Terms}` uses PostgreSQL full-text search: `to_tsvector("field") @@ plainto_tsquery($1)`. Words are stemmed and the terms are treated as plain words, not query syntax, so user input is safe to pass:

```erlang
search(Terms) ->
    Q = kura_query:where(published(), {'or', [{title, matches, Terms}, {body, matches, Terms}]}),
    blog_repo:all(kura_query:order_by(Q, [{published_at, desc}])).
```

```erlang
8> blog_stats:search(~"search").
{ok,[#{id => 5,status => published,
       title => <<"Postgres full text search">>,
       body => <<"Search posts by meaning, not substring.">>,
       ...}]}
```

On a large table, add a GIN index on `to_tsvector(...)` with an `execute` migration so the search does not scan every row.

## Cursor pagination

`kura_paginator:paginate/3` from the [CRUD](crud.md) chapter counts rows and uses `OFFSET`, which gets slow deep into a large table. Cursor (keyset) pagination remembers the last row instead:

```erlang
comments_page(#{id := PostId}, Opts) ->
    Q = kura_query:where(kura_query:from(blog_comment), {post_id, PostId}),
    kura_paginator:cursor_paginate(blog_repo, Q, Opts).
```

Options are `limit`, `cursor_field` (default `id`) and `'after'` or `before` with a cursor from a previous page. From `test/blog_data_query_SUITE.erl`, with five comments on a post:

```erlang
{ok, #{entries := [#{body := ~"1"}, #{body := ~"2"}], has_next := true, end_cursor := Cursor}} =
    blog_stats:comments_page(First, #{limit => 2}),
{ok, #{entries := [#{body := ~"3"}, #{body := ~"4"}], has_prev := true}} =
    blog_stats:comments_page(First, #{limit => 2, 'after' => Cursor}).
```

## Streaming

To export every published title without loading all posts into memory, `kura_stream` reads through a server-side cursor in batches:

```erlang
export_titles(IoDevice) ->
    kura_stream:stream(blog_repo, published(), fun(Batch) -> write_titles(IoDevice, Batch) end, #{
        batch_size => 100
    }).

write_titles(IoDevice, Posts) ->
    ok = file:write(IoDevice, [[maps:get(title, P), $\n] || P <- Posts]).
```

The callback is called once per batch and must return `ok`. It is a closure because it has to carry `IoDevice`.

```erlang
9> blog_stats:export_titles(standard_io).
Changesets
Supervision trees
Pattern matching
Postgres full text search
ok
```

## Raw SQL

When the builder gets in the way, `blog_repo:query/2` runs SQL with `$N` parameters. Multi-line SQL reads best in a triple-quoted sigil:

```erlang
top_commented(Limit) ->
    blog_repo:query(
        ~"""
        SELECT p.id, p.title, count(c.id) AS comments
        FROM posts p LEFT JOIN comments c ON c.post_id = p.id
        WHERE p.deleted_at IS NULL
        GROUP BY p.id
        ORDER BY comments DESC, p.id
        LIMIT $1
        """,
        [Limit]
    ).
```

```erlang
10> blog_stats:top_commented(2).
{ok,[#{id => 2,title => <<"Changesets">>,comments => 2},
     #{id => 3,title => <<"Supervision trees">>,comments => 1}]}
```

Raw queries return rows as maps with atom keys, but values are not loaded through the schema types: enums stay binaries and JSONB stays text.

---

Next: making multi-step changes safe with [Transactions and Bulk Operations](transactions-bulk.md).
