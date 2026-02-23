# Tags, Many-to-Many & Embedded Schemas

Our blog has users, posts, and comments. Now let's add tags (many-to-many through a join table) and post metadata (embedded schema stored as JSONB).

## Tag schema

Create `src/schemas/tag.erl`:

```erlang
-module(tag).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0, associations/0, changeset/2]).

table() -> <<"tags">>.

primary_key() -> id.

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = name, type = string, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime}
    ].

associations() ->
    [
        #kura_assoc{name = posts, type = many_to_many, schema = post,
                    join_through = <<"posts_tags">>, join_keys = {tag_id, post_id}}
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(tag, Data, Params, [name]),
    CS1 = kura_changeset:validate_required(CS, [name]),
    kura_changeset:unique_constraint(CS1, name).
```

## Join table schema

The many-to-many relationship needs a join table. Create `src/schemas/posts_tags.erl`:

```erlang
-module(posts_tags).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0]).

table() -> <<"posts_tags">>.

primary_key() -> id.

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = post_id, type = integer, nullable = false},
        #kura_field{name = tag_id, type = integer, nullable = false}
    ].
```

## Adding many-to-many to posts

Update the `associations/0` in `src/schemas/post.erl`:

```erlang
associations() ->
    [
        #kura_assoc{name = author, type = belongs_to, schema = user, foreign_key = user_id},
        #kura_assoc{name = comments, type = has_many, schema = comment, foreign_key = post_id},
        #kura_assoc{name = tags, type = many_to_many, schema = tag,
                    join_through = <<"posts_tags">>, join_keys = {post_id, tag_id}}
    ].
```

The `many_to_many` association specifies:
- `join_through` — the join table name
- `join_keys` — `{this_side_fk, other_side_fk}` on the join table

## Generate the migrations

Compile to generate the new tables:

```shell
rebar3 compile
```

```
===> [kura] Schema diff detected changes
===> [kura] Generated src/migrations/m20260223140000_create_tags.erl
===> [kura] Generated src/migrations/m20260223140100_create_posts_tags.erl
```

## Tagging posts with put_assoc

Use `put_assoc` to set tags on a post:

```erlang
%% Get existing tags (or create new ones first)
{ok, Erlang} = blog_repo:get_by(tag, [{name, <<"erlang">>}]),
{ok, Nova} = blog_repo:get_by(tag, [{name, <<"nova">>}]),

%% Assign tags to a post
{ok, Post} = blog_repo:get(post, 1),
CS = kura_changeset:cast(post, Post, #{}, []),
CS1 = kura_changeset:put_assoc(CS, tags, [Erlang, Nova]),
{ok, _} = blog_repo:update(CS1).
```

`put_assoc` replaces the entire association — under the hood it deletes existing join table rows and inserts new ones, all in a transaction.

## Preloading tags

```erlang
Q = kura_query:from(post),
Q1 = kura_query:preload(Q, [author, tags]),
{ok, Posts} = blog_repo:all(Q1).
```

Each post now has a `tags` key with a list of tag maps:

```erlang
#{id => 1, title => <<"My First Post">>,
  tags => [#{id => 1, name => <<"erlang">>}, #{id => 2, name => <<"nova">>}],
  ...}
```

## Embedded schemas

Sometimes you need structured data that doesn't deserve its own table. Kura's embedded schemas store nested structures as JSONB columns.

### Post metadata

Create `src/schemas/post_metadata.erl`:

```erlang
-module(post_metadata).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0, changeset/2]).

table() -> <<"embedded">>.

primary_key() -> undefined.

fields() ->
    [
        #kura_field{name = meta_title, type = string},
        #kura_field{name = meta_description, type = string},
        #kura_field{name = og_image, type = string}
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(post_metadata, Data, Params,
                             [meta_title, meta_description, og_image]),
    kura_changeset:validate_length(CS, meta_description, [{max, 160}]).
```

The embedded schema looks like a regular schema but with `table()` returning a placeholder (it's never queried directly) and `primary_key()` returning `undefined`.

### Adding the embed to posts

Update `src/schemas/post.erl` to add an `embeds/0` callback and a `metadata` JSONB field:

```erlang
-export([table/0, fields/0, primary_key/0, associations/0, embeds/0, changeset/2]).

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = title, type = string, nullable = false},
        #kura_field{name = body, type = text},
        #kura_field{name = status, type = {enum, [draft, published, archived]}, default = <<"draft">>},
        #kura_field{name = user_id, type = integer},
        #kura_field{name = metadata, type = {embed, embeds_one, post_metadata}},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

embeds() ->
    [
        #kura_embed{name = metadata, type = embeds_one, schema = post_metadata}
    ].
```

Compile to generate a migration that adds the `metadata` JSONB column:

```shell
rebar3 compile
```

### Using embedded schemas

Cast the embed in your changeset:

```erlang
changeset(Data, Params) ->
    CS = kura_changeset:cast(post, Data, Params, [title, body, status, user_id, metadata]),
    CS1 = kura_changeset:validate_required(CS, [title, body]),
    CS2 = kura_changeset:validate_length(CS1, title, [{min, 3}, {max, 200}]),
    CS3 = kura_changeset:validate_inclusion(CS2, status, [draft, published, archived]),
    CS4 = kura_changeset:foreign_key_constraint(CS3, user_id),
    kura_changeset:cast_embed(CS4, metadata).
```

`cast_embed` reads the `metadata` key from params and builds a nested changeset using `post_metadata:changeset/2`. Create a post with metadata:

```shell
curl -s -X POST localhost:8080/api/posts \
  -H "Content-Type: application/json" \
  -d '{
    "title": "SEO Optimized Post",
    "body": "Great content here",
    "user_id": 1,
    "metadata": {
      "meta_title": "Best Post Ever",
      "meta_description": "A post about great things",
      "og_image": "https://example.com/image.jpg"
    }
  }' | python3 -m json.tool
```

The metadata is stored as JSONB in PostgreSQL and loaded back as a nested map:

```erlang
#{id => 5,
  title => <<"SEO Optimized Post">>,
  metadata => #{meta_title => <<"Best Post Ever">>,
                meta_description => <<"A post about great things">>,
                og_image => <<"https://example.com/image.jpg">>},
  ...}
```

## Filtering by tag

To find posts with a specific tag, use a raw SQL fragment or build the query through the join table:

```erlang
%% Find all post IDs for a given tag
find_posts_by_tag(TagName) ->
    {ok, Tag} = blog_repo:get_by(tag, [{name, TagName}]),
    TagId = maps:get(id, Tag),
    Q = kura_query:from(posts_tags),
    Q1 = kura_query:where(Q, {tag_id, TagId}),
    {ok, JoinRows} = blog_repo:all(Q1),
    PostIds = [maps:get(post_id, R) || R <- JoinRows],
    Q2 = kura_query:from(post),
    Q3 = kura_query:where(Q2, {id, in, PostIds}),
    Q4 = kura_query:preload(Q3, [author, tags]),
    blog_repo:all(Q4).
```

## API endpoint for tags

Add a simple tags controller:

```erlang
-module(blog_tags_controller).
-export([index/1, create/1]).

index(_Req) ->
    Q = kura_query:from(tag),
    Q1 = kura_query:order_by(Q, [{name, asc}]),
    {ok, Tags} = blog_repo:all(Q1),
    {json, #{tags => Tags}}.

create(#{params := Params}) ->
    CS = tag:changeset(#{}, Params),
    case blog_repo:insert(CS) of
        {ok, Tag} ->
            {json, 201, #{}, Tag};
        {error, _CS} ->
            {status, 422, #{}, #{error => <<"invalid tag">>}}
    end.
```

---

We now have a rich data model with associations, many-to-many relationships, and embedded schemas. Next, let's write proper [tests](../testing-errors/testing.md) for our application.
