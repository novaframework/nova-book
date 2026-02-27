# CRUD with the Repository

We have schemas, migrations, and changesets. Now let's use the repository to create, read, update, and delete records — and wire it all up to a controller.

## Insert

Create a record by building a changeset and passing it to `blog_repo:insert/1`:

```erlang
Params = #{<<"title">> => <<"My First Post">>,
           <<"body">> => <<"Hello from Nova!">>,
           <<"status">> => <<"draft">>,
           <<"user_id">> => 1},
CS = post:changeset(#{}, Params),
{ok, Post} = blog_repo:insert(CS).
```

If the changeset is invalid, `insert` returns `{error, Changeset}` with the errors:

```erlang
CS = post:changeset(#{}, #{}),
{error, #kura_changeset{errors = [{title, <<"can't be blank">>}, ...]}} = blog_repo:insert(CS).
```

## Query all

Use the query builder to fetch records:

```erlang
Q = kura_query:from(post),
{ok, Posts} = blog_repo:all(Q).
```

`Posts` is a list of maps, each representing a row:

```erlang
[#{id => 1, title => <<"My First Post">>, body => <<"Hello from Nova!">>,
   status => draft, user_id => 1,
   inserted_at => {{2026,2,23},{12,0,0}}, updated_at => {{2026,2,23},{12,0,0}}}]
```

Notice `status` is the atom `draft`, not a binary — Kura handles the conversion.

## Get by ID

Fetch a single record by primary key:

```erlang
{ok, Post} = blog_repo:get(post, 1).
{error, not_found} = blog_repo:get(post, 999).
```

## Get by field

`get_by/2` fetches a single record matching the given fields:

```erlang
{ok, User} = blog_repo:get_by(user, [{email, <<"alice@example.com">>}]).
{error, not_found} = blog_repo:get_by(user, [{username, <<"nobody">>}]).
```

If more than one row matches, it returns `{error, multiple_results}`.

For more complex lookups, `one/1` returns a single result from a query:

```erlang
Q = kura_query:from(post),
Q1 = kura_query:where(Q, {status, published}),
Q2 = kura_query:order_by(Q1, [{inserted_at, desc}]),
{ok, Latest} = blog_repo:one(Q2).
```

Like `get_by`, it returns `{error, not_found}` when no rows match and `{error, multiple_results}` when more than one row matches.

## Update

To update a record, build a changeset from the existing data and new params:

```erlang
{ok, Post} = blog_repo:get(post, 1),
CS = post:changeset(Post, #{<<"title">> => <<"Updated Title">>}),
{ok, UpdatedPost} = blog_repo:update(CS).
```

Only the changed fields are included in the `UPDATE` statement.

## Delete

Delete takes a changeset built from the existing record:

```erlang
{ok, Post} = blog_repo:get(post, 1),
CS = kura_changeset:cast(post, Post, #{}, []),
{ok, _} = blog_repo:delete(CS).
```

## Query builder

The query builder composes — chain functions to build up complex queries:

```erlang
%% Filter by status
Q = kura_query:from(post),
Q1 = kura_query:where(Q, {status, published}),
{ok, Published} = blog_repo:all(Q1).

%% Order by insertion date, newest first
Q2 = kura_query:order_by(Q1, [{inserted_at, desc}]),

%% Limit and offset for pagination
Q3 = kura_query:limit(Q2, 10),
Q4 = kura_query:offset(Q3, 20),
{ok, Page3} = blog_repo:all(Q4).

%% Select specific fields only
Q5 = kura_query:select(Q, [id, title, status]),
{ok, Posts} = blog_repo:all(Q5).
```

### Where conditions

```erlang
%% Equality
kura_query:where(Q, {title, <<"Hello">>})

%% Comparison operators
kura_query:where(Q, {user_id, '>', 5})
kura_query:where(Q, {inserted_at, '>=', {{2026,1,1},{0,0,0}}})

%% IN clause
kura_query:where(Q, {status, in, [draft, published]})

%% LIKE / ILIKE
kura_query:where(Q, {title, ilike, <<"%nova%">>})

%% NULL checks
kura_query:where(Q, {body, is_nil})
kura_query:where(Q, {body, is_not_nil})

%% OR conditions
kura_query:where(Q, {'or', [{status, draft}, {status, archived}]})

%% NOT IN clause
kura_query:where(Q, {status, not_in, [archived, deleted]})

%% BETWEEN
kura_query:where(Q, {user_id, between, {1, 100}})

%% NOT wrapper
kura_query:where(Q, {'not', {status, draft}})

%% AND conditions (multiple where calls are AND by default)
Q1 = kura_query:where(Q, {status, published}),
Q2 = kura_query:where(Q1, {user_id, 1}).
```

## Wiring up to a controller

Let's build a posts API controller that uses the repo. Create `src/controllers/blog_posts_controller.erl`:

```erlang
-module(blog_posts_controller).
-include_lib("kura/include/kura.hrl").

-export([
         list/1,
         show/1,
         create/1,
         update/1,
         delete/1
        ]).

list(_Req) ->
    Q = kura_query:from(post),
    Q1 = kura_query:order_by(Q, [{inserted_at, desc}]),
    {ok, Posts} = blog_repo:all(Q1),
    {json, #{posts => [post_to_json(P) || P <- Posts]}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    case blog_repo:get(post, binary_to_integer(Id)) of
        {ok, Post} ->
            {json, post_to_json(Post)};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"post not found">>}}
    end.

create(#{json := Params}) ->
    CS = post:changeset(#{}, Params),
    case blog_repo:insert(CS) of
        {ok, Post} ->
            {json, 201, #{}, post_to_json(Post)};
        {error, #kura_changeset{} = CS1} ->
            {json, 422, #{}, #{errors => changeset_errors_to_json(CS1)}}
    end;
create(_Req) ->
    {status, 422, #{}, #{error => <<"request body required">>}}.

update(#{bindings := #{<<"id">> := Id}, json := Params}) ->
    case blog_repo:get(post, binary_to_integer(Id)) of
        {ok, Post} ->
            CS = post:changeset(Post, Params),
            case blog_repo:update(CS) of
                {ok, Updated} ->
                    {json, post_to_json(Updated)};
                {error, #kura_changeset{} = CS1} ->
                    {json, 422, #{}, #{errors => changeset_errors_to_json(CS1)}}
            end;
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"post not found">>}}
    end.

delete(#{bindings := #{<<"id">> := Id}}) ->
    case blog_repo:get(post, binary_to_integer(Id)) of
        {ok, Post} ->
            CS = kura_changeset:cast(post, Post, #{}, []),
            {ok, _} = blog_repo:delete(CS),
            {status, 204};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"post not found">>}}
    end.

%% Helpers

post_to_json(#{id := Id, title := Title, body := Body, status := Status,
               user_id := UserId, inserted_at := InsertedAt}) ->
    #{id => Id, title => Title, body => Body,
      status => atom_to_binary(Status), user_id => UserId,
      inserted_at => format_datetime(InsertedAt)}.

changeset_errors_to_json(#kura_changeset{errors = Errors}) ->
    lists:foldl(fun({Field, Msg}, Acc) ->
        Key = atom_to_binary(Field),
        Existing = maps:get(Key, Acc, []),
        Acc#{Key => Existing ++ [Msg]}
    end, #{}, Errors).

format_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
                                  [Y, Mo, D, H, Mi, S]));
format_datetime(_) ->
    null.
```

## Adding the routes

```erlang
#{prefix => "/api",
  security => false,
  routes => [
             {"/posts", fun blog_posts_controller:list/1, #{methods => [get]}},
             {"/posts/:id", fun blog_posts_controller:show/1, #{methods => [get]}},
             {"/posts", fun blog_posts_controller:create/1, #{methods => [post]}},
             {"/posts/:id", fun blog_posts_controller:update/1, #{methods => [put]}},
             {"/posts/:id", fun blog_posts_controller:delete/1, #{methods => [delete]}}
            ]
}
```

## Testing with curl

Start the node and test:

```shell
# Create a post
curl -s -X POST localhost:8080/api/posts \
  -H "Content-Type: application/json" \
  -d '{"title": "My First Post", "body": "Hello from Nova!", "status": "draft", "user_id": 1}' \
  | python3 -m json.tool

# List all posts
curl -s localhost:8080/api/posts | python3 -m json.tool

# Get a single post
curl -s localhost:8080/api/posts/1 | python3 -m json.tool

# Update a post
curl -s -X PUT localhost:8080/api/posts/1 \
  -H "Content-Type: application/json" \
  -d '{"title": "Updated Title", "status": "published"}' \
  | python3 -m json.tool

# Delete a post
curl -s -X DELETE localhost:8080/api/posts/1 -w "%{http_code}\n"

# Try creating with invalid data
curl -s -X POST localhost:8080/api/posts \
  -H "Content-Type: application/json" \
  -d '{"title": "Hi"}' \
  | python3 -m json.tool
```

The last command returns a 422 with validation errors.

No SQL strings anywhere. The query builder composes, the repo executes.

---

This gives us a working API for a single resource. Next, let's add [associations and preloading](associations.md) to connect posts to users and comments.
