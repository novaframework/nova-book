# JSON API with Generators

In the previous chapter we built a posts controller by hand. The `rebar3_nova` plugin includes generators that scaffold controllers, JSON schemas, and test suites so you can skip the boilerplate.

## Generate a resource

The `nova gen_resource` command creates a controller, a JSON schema, and prints route definitions. Like `gen_controller`, it also accepts `--actions` to limit which actions are scaffolded:

```shell
rebar3 nova gen_resource --name posts
===> Writing src/controllers/blog_posts_controller.erl
===> Writing priv/schemas/post.json

Add these routes to your router:

  {<<"/posts">>, {blog_posts_controller, list}, #{methods => [get]}}
  {<<"/posts/:id">>, {blog_posts_controller, show}, #{methods => [get]}}
  {<<"/posts">>, {blog_posts_controller, create}, #{methods => [post]}}
  {<<"/posts/:id">>, {blog_posts_controller, update}, #{methods => [put]}}
  {<<"/posts/:id">>, {blog_posts_controller, delete}, #{methods => [delete]}}
```

### The generated controller

```erlang
-module(blog_posts_controller).
-export([
         list/1,
         show/1,
         create/1,
         update/1,
         delete/1
        ]).

list(#{req := _Req} = _NovaReq) ->
    {json, #{<<"message">> => <<"TODO">>}}.

show(#{req := _Req} = _NovaReq) ->
    {json, #{<<"message">> => <<"TODO">>}}.

create(#{req := _Req} = _NovaReq) ->
    {status, 201, #{}, #{<<"message">> => <<"TODO">>}}.

update(#{req := _Req} = _NovaReq) ->
    {json, #{<<"message">> => <<"TODO">>}}.

delete(#{req := _Req} = _NovaReq) ->
    {status, 204}.
```

Every action returns a valid Nova response tuple so you can compile and run immediately.

### The generated JSON schema

`priv/schemas/post.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "id": { "type": "integer" },
    "name": { "type": "string" }
  },
  "required": ["id", "name"]
}
```

Edit this to match your actual data model:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "id": { "type": "integer", "description": "Unique identifier" },
    "title": { "type": "string", "description": "Post title" },
    "body": { "type": "string", "description": "Post body" },
    "status": { "type": "string", "enum": ["draft", "published", "archived"] },
    "user_id": { "type": "integer", "description": "Author ID" }
  },
  "required": ["title", "body"]
}
```

This schema is picked up by the [OpenAPI generator](../going-further/openapi-tools.md) to produce API documentation automatically.

## Filling in Kura calls

Replace the TODO stubs with actual Kura repo calls. Since we already wrote a full posts controller in the [CRUD chapter](../data-layer/crud.md), here is the pattern — generate, then fill in:

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
               user_id := UserId}) ->
    #{id => Id, title => Title, body => Body,
      status => atom_to_binary(Status), user_id => UserId}.

changeset_errors_to_json(#kura_changeset{errors = Errors}) ->
    lists:foldl(fun({Field, Msg}, Acc) ->
        Key = atom_to_binary(Field),
        Existing = maps:get(Key, Acc, []),
        Acc#{Key => Existing ++ [Msg]}
    end, #{}, Errors).
```

## Generate a test suite

The `nova gen_test` command scaffolds a Common Test suite:

```shell
rebar3 nova gen_test --name posts
===> Writing test/blog_posts_controller_SUITE.erl
```

The generated suite has test cases for each CRUD action that make HTTP requests against your running application:

```erlang
-module(blog_posts_controller_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_list/1, test_show/1, test_create/1, test_update/1, test_delete/1]).

all() ->
    [test_list, test_show, test_create, test_update, test_delete].

init_per_suite(Config) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(blog),
    Config.

end_per_suite(_Config) ->
    application:stop(blog),
    ok.

test_list(_Config) ->
    {ok, {{_, 200, _}, _, _}} =
        httpc:request("http://localhost:8080/posts").

test_show(_Config) ->
    {ok, {{_, 200, _}, _, _}} =
        httpc:request("http://localhost:8080/posts/1").

test_create(_Config) ->
    {ok, {{_, 201, _}, _, _}} =
        httpc:request(post, {"http://localhost:8080/posts", [],
                             "application/json", "{}"}, [], []).

test_update(_Config) ->
    {ok, {{_, 200, _}, _, _}} =
        httpc:request(put, {"http://localhost:8080/posts/1", [],
                            "application/json", "{}"}, [], []).

test_delete(_Config) ->
    {ok, {{_, 204, _}, _, _}} =
        httpc:request(delete, {"http://localhost:8080/posts/1", []}, [], []).
```

Update the request bodies and assertions to match your actual API. We will cover testing in detail in the [Testing](../testing-errors/testing.md) chapter.

## Other generators

Generate a controller with specific actions:

```shell
rebar3 nova gen_controller --name comments --actions list,create
===> Writing src/controllers/blog_comments_controller.erl
```

## Typical workflow

Adding a new resource to your API:

```shell
# 1. Define the Kura schema
vi src/schemas/comment.erl

# 2. Compile to generate the migration
rebar3 compile

# 3. Generate the resource (controller + schema + route hints)
rebar3 nova gen_resource --name comments

# 4. Copy the printed routes into your router

# 5. Fill in the Kura repo calls in the controller

# 6. Generate a test suite
rebar3 nova gen_test --name comments

# 7. Run the tests
rebar3 ct
```

Generate, fill in the Kura calls, test. Three steps to a working API.

---

Our posts API works with flat data. Next, let's add [associations and preloading](associations.md) to connect posts to users and comments.
