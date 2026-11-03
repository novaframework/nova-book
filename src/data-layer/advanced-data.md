# Tags, Many-to-Many and Embedded Schemas

Posts get tags, and each tag can be on many posts. That is a many-to-many relationship, stored in a join table. Posts also get a small bag of SEO metadata that has no business being its own table; it goes into a JSONB column as an embedded schema.

## The tag schema

`src/schemas/blog_tag.erl`:

```erlang
-module(blog_tag).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0, associations/0]).
-export([changeset/2]).

table() -> ~"tags".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = name, type = string, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime}
    ].

indexes() ->
    [{[name], #{unique => true}}].

associations() ->
    [
        #kura_assoc{
            name = posts,
            type = many_to_many,
            schema = blog_post,
            join_through = blog_post_tag,
            join_keys = {tag_id, post_id}
        }
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [name]),
    CS1 = kura_changeset:validate_required(CS, [name]),
    CS2 = kura_changeset:validate_length(CS1, name, [{max, 30}]),
    kura_changeset:validate_format(CS2, name, ~"^[a-z0-9-]+\\z").
```

Tag names are lowercase slugs like `erlang` or `open-source`, so they can go into URLs as they are.

## The join schema

The join table gets its own schema, `src/schemas/blog_post_tag.erl`:

```erlang
-module(blog_post_tag).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, key/0, fields/0, indexes/0, associations/0]).

table() -> ~"post_tags".

key() -> [post_id, tag_id].

fields() ->
    [
        #kura_field{name = post_id, type = bigint, nullable = false},
        #kura_field{name = tag_id, type = bigint, nullable = false}
    ].

indexes() ->
    [
        {[post_id, tag_id], #{unique => true}},
        {[tag_id], #{}}
    ].

associations() ->
    [
        #kura_assoc{
            name = post,
            type = belongs_to,
            schema = blog_post,
            foreign_key = post_id,
            on_delete = cascade
        },
        #kura_assoc{
            name = tag,
            type = belongs_to,
            schema = blog_tag,
            foreign_key = tag_id,
            on_delete = cascade
        }
    ].
```

There is no `id` column. `key/0` declares a composite primary key, and the unique index stops a tag from being attached twice. The two `belongs_to` associations give the generator its foreign keys; deleting a post or a tag removes the link rows.

## Tags on posts

Add the other side of the relationship to `blog_post:associations/0`:

```erlang
#kura_assoc{
    name = tags,
    type = many_to_many,
    schema = blog_tag,
    join_through = blog_post_tag,
    join_keys = {post_id, tag_id}
}
```

`join_keys` is `{KeyToThisSchema, KeyToTheOtherSchema}` in the join table, which is why it is flipped in `blog_tag`.

## Generating the migrations

```admonish warning title="Compile the tag schema before the join schema"
In one compile, the generator creates new tables in alphabetical order, not in dependency order. With `blog_tag` and `blog_post_tag` added together it emits `create_table post_tags` before `create_table tags`, and the migration fails because `tags` does not exist yet. Add `blog_tag` first and compile, then add `blog_post_tag` and compile again. Each step is its own migration.
```

The first compile, with `blog_tag` and the metadata column from the second half of this chapter, generates `m20261103073021_update_schema.erl`:

```erlang
up() ->
    [
        {create_table, ~"tags", [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{name = name, type = string, nullable = false},
            #kura_column{name = inserted_at, type = utc_datetime}
        ]},
        {alter_table, ~"posts", [
            {add_column, #kura_column{name = metadata, type = jsonb}}
        ]},
        {create_index, ~"tags", [name], #{unique => true}}
    ].
```

The second, after adding `blog_post_tag`, generates `m20261103073031_create_post_tags.erl`:

```erlang
up() ->
    [
        {create_table, ~"post_tags", [
            #kura_column{
                name = post_id,
                type = bigint,
                nullable = false,
                references = {~"posts", id},
                on_delete = cascade
            },
            #kura_column{
                name = tag_id,
                type = bigint,
                nullable = false,
                references = {~"tags", id},
                on_delete = cascade
            }
        ]},
        {create_index, ~"post_tags", [post_id, tag_id], #{unique => true}},
        {create_index, ~"post_tags", [tag_id], #{}}
    ].
```

## Tagging posts with put_assoc

Clients send tag names. The context makes sure the tags exist, then replaces the post's tags with `put_assoc/3`. In `blog_posts.erl`:

```erlang
tag_post(Post, Names) ->
    case ensure_tags(Names) of
        {ok, Tags} ->
            CS = kura_changeset:cast(blog_post, Post, #{}, []),
            blog_repo:update(kura_changeset:put_assoc(CS, tags, Tags));
        {error, _} = Error ->
            Error
    end.

ensure_tags([]) ->
    {ok, []};
ensure_tags(Names) ->
    Changesets = [blog_tag:changeset(#{}, #{name => Name}) || Name <- lists:usort(Names)],
    case [CS || #kura_changeset{valid = false} = CS <- Changesets] of
        [Invalid | _] ->
            {error, Invalid};
        [] ->
            Rows = [kura_changeset:apply_changes(CS) || CS <- Changesets],
            {ok, _} = blog_repo:insert_all(blog_tag, Rows, #{on_conflict => {name, nothing}}),
            blog_repo:all(kura_query:where(kura_query:from(blog_tag), {name, in, Names}))
    end.
```

- Every name is validated with the tag changeset first. Invalid names never reach the database.
- `insert_all/3` with `on_conflict => {name, nothing}` inserts the tags that are new and skips the ones that exist, in one statement. The [Transactions and Bulk Operations](transactions-bulk.md) chapter covers it in detail.
- `put_assoc(CS, tags, Tags)` says "these are the post's tags now". On update Kura deletes the post's old join rows and inserts the new ones.

```erlang
1> {ok, P2} = blog_repo:get(blog_post, 2).
...
2> {ok, Tagged} = blog_posts:tag_post(P2, [~"otp", ~"kura"]).
...
3> [maps:get(name, T) || T <- maps:get(tags, Tagged)].
[<<"kura">>,<<"otp">>]
4> blog_repo:query(~"SELECT post_id, tag_id FROM post_tags ORDER BY tag_id", []).
{ok,[#{post_id => 2,tag_id => 2},
     #{post_id => 2,tag_id => 4}]}
```

Post 2 was tagged `erlang` and `kura` before; `erlang` is gone, `otp` is new.

Preloading works in both directions:

```erlang
5> {ok, [KuraTag]} = blog_repo:all(kura_query:where(kura_query:from(blog_tag), {name, ~"kura"})).
{ok,[#{id => 2,name => <<"kura">>,
       inserted_at => {{2026,11,3},{7,44,22}}}]}
6> [maps:get(title, P) || P <- maps:get(posts, blog_repo:preload(blog_tag, KuraTag, [posts]))].
[<<"Changesets">>]
```

## Filtering by tag

"Posts tagged kura" is a subquery: find the tag's id, find the post ids linked to it, then the posts. `{Field, in, {subquery, Query}}` nests one query in another:

```erlang
tagged_post_ids(TagName) ->
    TagIds = kura_query:select(kura_query:where(kura_query:from(blog_tag), {name, TagName}), [id]),
    PostTags = kura_query:where(kura_query:from(blog_post_tag), {tag_id, in, {subquery, TagIds}}),
    kura_query:select(PostTags, [post_id]).
```

`list_posts/1` accepts a `tag` parameter (add `tag => string` to its types and to the cast list) and applies it next to the status filter:

```erlang
{ok, Filters} ->
    Query = filter_by_tag(filter_by_status(kura_query:from(blog_post), Filters), Filters),
    ...

filter_by_tag(Query, #{tag := Tag}) ->
    kura_query:where(Query, {id, in, {subquery, tagged_post_ids(Tag)}});
filter_by_tag(Query, #{}) ->
    Query.
```

## Embedded schemas

Each post can carry SEO metadata: a title and description for search engines, a cover image and a reading time. We always read and write it together with the post and never query it on its own, so it lives in the post row as JSONB, described by an **embedded schema**.

### The metadata schema

`src/schemas/blog_post_metadata.erl`:

```erlang
-module(blog_post_metadata).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0]).
-export([changeset/2]).

table() -> ~"_embedded".

fields() ->
    [
        #kura_field{name = seo_title, type = string},
        #kura_field{name = seo_description, type = string},
        #kura_field{name = cover_image_url, type = string},
        #kura_field{name = reading_minutes, type = integer}
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [
        seo_title, seo_description, cover_image_url, reading_minutes
    ]),
    CS1 = kura_changeset:validate_length(CS, seo_title, [{max, 70}]),
    CS2 = kura_changeset:validate_length(CS1, seo_description, [{max, 160}]),
    CS3 = kura_changeset:validate_format(CS2, cover_image_url, ~"^https://"),
    kura_changeset:validate_number(CS3, reading_minutes, [{greater_than, 0}]).
```

It looks like a schema, with fields and a changeset, but it has no table of its own.

```admonish warning title="No -behaviour(kura_schema) on embedded schemas"
Kura's guide describes an embedded schema as a regular `kura_schema` module with `table() -> ~"_embedded"`. The `rebar3_kura` compile hook, however, treats every module with `-behaviour(kura_schema)` as a table and would generate `create_table "_embedded"`. Leave the behaviour attribute out. Keep `table/0`: `kura_changeset:cast/4` calls it.
```

### Embedding it in posts

In `blog_post.erl`, declare the column as a `jsonb` field and the embed in a new `embeds/0` callback:

```erlang
-export([table/0, fields/0, indexes/0, associations/0, embeds/0]).

fields() ->
    [
        ...
        #kura_field{name = metadata, type = jsonb},
        ...
    ].

embeds() ->
    [#kura_embed{name = metadata, type = embeds_one, schema = blog_post_metadata}].
```

`embeds_one` holds one map, `embeds_many` a list. The `jsonb` field is what the migration generator sees; it reads only `fields/0`, so an embed declared only in `embeds/0` gets no column. At runtime the embed declaration wins and the field is cast and loaded through `blog_post_metadata`.

The post changeset casts the embed with `cast_embed/3`, again with an explicit changeset function:

```erlang
changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [title, body, status]),
    CS1 = kura_changeset:validate_required(CS, [title, body]),
    CS2 = kura_changeset:validate_length(CS1, title, [{min, 3}, {max, 200}]),
    CS3 = kura_changeset:foreign_key_constraint(CS2, user_id),
    CS4 = kura_changeset:cast_embed(CS3, metadata, #{with => fun blog_post_metadata:changeset/2}),
    stamp_published_at(CS4).
```

Errors from the embed land on the post changeset:

```erlang
7> blog_post:changeset(P2, #{~"metadata" => #{~"seo_title" => binary:copy(~"x", 80), ~"cover_image_url" => ~"http://cdn.example.com/a.png"}}).
#kura_changeset{
    valid = false,schema = blog_post,
    ...
    changes = #{},
    errors =
        [{seo_title,<<"should be at most 70 character(s)">>},
         {cover_image_url,<<"has invalid format">>}],
    ...}
```

A loaded post has the metadata as a map with atom keys, holding only the keys that were set. That is what `P2` looked like above:

```erlang
{ok,#{id => 2,status => published,title => <<"Changesets">>,
      metadata =>
          #{seo_title => <<"Kura changesets">>,reading_minutes => 4},
      ...}}
```

```admonish note
Raw SQL through `blog_repo:query/2` returns JSONB columns as JSON text, not decoded: `<<"{\"seo_title\": \"Kura changesets\", \"reading_minutes\": 4}">>`. Use PostgreSQL's JSON operators (`metadata->>'seo_title'`) or decode it yourself.
```

## Tags and metadata in the API

The JSON helper renders preloaded tags as a sorted list of names, and the metadata with every field present:

```erlang
with_loaded(Record, Json) ->
    Loaded = [
        {Key, loaded(Key, maps:get(Key, Record))}
     || Key <- [author, comments, tags], maps:is_key(Key, Record)
    ],
    maps:merge(Json, maps:from_list(Loaded)).

loaded(author, Author) -> user(Author);
loaded(comments, Comments) -> [comment(C) || C <- Comments];
loaded(tags, Tags) -> lists:sort([maps:get(name, T) || T <- Tags]).

metadata(undefined) ->
    null;
metadata(Metadata) ->
    #{
        Field => null_if_undefined(maps:get(Field, Metadata, undefined))
     || Field <- kura_schema:field_names(blog_post_metadata)
    }.

null_if_undefined(undefined) -> null;
null_if_undefined(Value) -> Value.
```

`post/1` gains `metadata => metadata(maps:get(metadata, Post))`. The controller preloads tags in `show/1` and gets an action to set them:

```erlang
show(#{bindings := #{~"id" := Id}}) ->
    case blog_posts:get_post(Id, [author, tags, {comments, [author]}]) of
        {ok, Post} -> {json, blog_api_json:post(Post)};
        {error, not_found} -> not_found()
    end.

update_tags(#{bindings := #{~"id" := Id}, json := #{~"tags" := Names}}) when is_list(Names) ->
    case blog_posts:get_post(Id) of
        {ok, Post} ->
            case blog_posts:tag_post(Post, Names) of
                {ok, Tagged} -> {json, blog_api_json:post(Tagged)};
                {error, #kura_changeset{} = CS} -> unprocessable(CS)
            end;
        {error, not_found} ->
            not_found()
    end;
update_tags(_Req) ->
    {json, 400, #{}, #{error => ~"expected a tags list"}}.
```

And the route, in the `/api` group:

```erlang
{"/posts/:id/tags", fun blog_posts_controller:update_tags/1, #{methods => [put]}}
```

These responses come from the finished blog, so they also show `lock_version` from the [Data Safety](data-safety.md) chapter.

```shell
curl -s -X POST localhost:8080/api/posts -H 'content-type: application/json' -d '{"user_id": 1, "title": "Changesets", "body": "Cast, validate, persist.", "status": "published", "metadata": {"seo_title": "Kura changesets", "reading_minutes": 4}}'
```

```json
{"id":2,"status":"published","title":"Changesets","metadata":{"seo_title":"Kura changesets","seo_description":null,"cover_image_url":null,"reading_minutes":4},"body":"Cast, validate, persist.","user_id":1,"inserted_at":"2026-11-03T07:44:22Z","published_at":"2026-11-03T07:44:22Z","lock_version":1}
```

```shell
curl -s -X PUT localhost:8080/api/posts/2/tags -H 'content-type: application/json' -d '{"tags": ["erlang", "kura"]}'
```

```json
{"id":2,"status":"published","title":"Changesets","metadata":{"seo_title":"Kura changesets","seo_description":null,"cover_image_url":null,"reading_minutes":4},"tags":["erlang","kura"],"body":"Cast, validate, persist.","user_id":1,"inserted_at":"2026-11-03T07:44:22Z","published_at":"2026-11-03T07:44:22Z","lock_version":1}
```

An invalid tag is rejected with the tag changeset's error:

```shell
curl -s -i -X PUT localhost:8080/api/posts/2/tags -H 'content-type: application/json' -d '{"tags": ["Not A Slug"]}'
```

```
HTTP/1.1 422 Unprocessable Entity
...

{"errors":{"name":["has invalid format"]}}
```

Filter the list by tag:

```shell
curl -s 'localhost:8080/api/posts?tag=kura'
```

```json
{"posts":[{"id":2,"status":"published","title":"Changesets","metadata":{"seo_title":"Kura changesets","seo_description":null,"cover_image_url":null,"reading_minutes":4},"body":"Cast, validate, persist.","user_id":1,"inserted_at":"2026-11-03T07:44:22Z","published_at":"2026-11-03T07:44:22Z","lock_version":1}],"page":1,"total_pages":1,"total_entries":1}
```

The full post, with author, tags and comments:

```shell
curl -s localhost:8080/api/posts/2
```

```json
{"id":2,"status":"published","title":"Changesets","metadata":{"seo_title":"Kura changesets","seo_description":null,"cover_image_url":null,"reading_minutes":4},"author":{"id":1,"role":"author","username":"alice"},"tags":["erlang","kura"],"body":"Cast, validate, persist.","user_id":1,"inserted_at":"2026-11-03T07:44:22Z","published_at":"2026-11-03T07:44:22Z","lock_version":1,"comments":[{"id":1,"author":{"id":2,"role":"author","username":"bob"},"body":"Great post!","user_id":2,"inserted_at":"2026-11-03T07:44:22Z"}]}
```

---

The data model is complete. Next we ask it harder questions in [Advanced Queries](advanced-queries.md).
