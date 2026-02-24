# Testing

Nova applications can be tested with Erlang's built-in frameworks: EUnit for unit tests and Common Test for integration tests. The [nova_test](https://github.com/novaframework/nova_test) library adds helpers: a request builder for unit testing controllers, an HTTP client for integration tests, and assertion macros.

## Adding nova_test

Add `nova_test` as a test dependency in `rebar.config`:

```erlang
{profiles, [
    {test, [
        {deps, [
            {nova_test, "0.1.0"}
        ]}
    ]}
]}.
```

## Database setup for tests

Tests need a running PostgreSQL. Use the same `docker-compose.yml` from the [Database Setup](../data-layer/setup.md) chapter:

```shell
docker compose up -d
```

Your test configuration should point at the test database. You can use the same development database for simplicity, or create a separate one for isolation.

## EUnit — Unit testing controllers

Nova controllers are regular Erlang functions that receive a request map and return a tuple. The `nova_test_req` module builds well-formed request maps so you don't have to construct them by hand.

Create `test/blog_posts_controller_tests.erl`:

```erlang
-module(blog_posts_controller_tests).
-include_lib("nova_test/include/nova_test.hrl").

show_existing_post_test() ->
    Req = nova_test_req:new(get, "/api/posts/1"),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"1">>}, Req),
    Result = blog_posts_controller:show(Req1),
    ?assertJsonResponse(#{id := 1, title := _}, Result).

show_missing_post_test() ->
    Req = nova_test_req:new(get, "/api/posts/999999"),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"999999">>}, Req),
    Result = blog_posts_controller:show(Req1),
    ?assertStatusResponse(404, Result).

create_post_test() ->
    Req = nova_test_req:new(post, "/api/posts"),
    Req1 = nova_test_req:with_json(#{<<"title">> => <<"Test Post">>,
                                     <<"body">> => <<"Test body">>,
                                     <<"user_id">> => 1}, Req),
    Result = blog_posts_controller:create(Req1),
    ?assertJsonResponse(201, #{id := _}, Result).

create_invalid_post_test() ->
    Req = nova_test_req:new(post, "/api/posts"),
    Req1 = nova_test_req:with_json(#{}, Req),
    Result = blog_posts_controller:create(Req1),
    ?assertStatusResponse(422, Result).
```

### Request builder functions

| Function | Purpose |
|---|---|
| `nova_test_req:new/2` | Create a request with method and path |
| `nova_test_req:with_bindings/2` | Set path bindings (e.g. `#{<<"id">> => <<"1">>}`) |
| `nova_test_req:with_json/2` | Set a JSON body (auto-encodes, sets content-type) |
| `nova_test_req:with_header/3` | Add a request header |
| `nova_test_req:with_query/2` | Set query string parameters |
| `nova_test_req:with_body/2` | Set a raw body |
| `nova_test_req:with_auth_data/2` | Set auth data (for testing authenticated controllers) |
| `nova_test_req:with_peer/2` | Set the client peer address |

Run EUnit tests:

```shell
rebar3 eunit
```

## Testing changesets

Changesets are pure functions — no database needed. Test them directly:

```erlang
-module(post_changeset_tests).
-include_lib("kura/include/kura.hrl").
-include_lib("eunit/include/eunit.hrl").

valid_changeset_test() ->
    CS = post:changeset(#{}, #{<<"title">> => <<"Good Title">>,
                               <<"body">> => <<"Some content">>}),
    ?assert(CS#kura_changeset.valid).

missing_title_test() ->
    CS = post:changeset(#{}, #{<<"body">> => <<"Some content">>}),
    ?assertNot(CS#kura_changeset.valid),
    ?assertMatch([{title, _} | _], CS#kura_changeset.errors).

title_too_short_test() ->
    CS = post:changeset(#{}, #{<<"title">> => <<"Hi">>,
                               <<"body">> => <<"Content">>}),
    ?assertNot(CS#kura_changeset.valid),
    ?assertMatch([{title, _}], CS#kura_changeset.errors).

invalid_status_test() ->
    CS = post:changeset(#{}, #{<<"title">> => <<"Good Title">>,
                               <<"body">> => <<"Content">>,
                               <<"status">> => <<"invalid">>}),
    ?assertNot(CS#kura_changeset.valid).

valid_email_format_test() ->
    CS = user:changeset(#{}, #{<<"username">> => <<"alice">>,
                               <<"email">> => <<"alice@example.com">>,
                               <<"password_hash">> => <<"hashed">>}),
    ?assert(CS#kura_changeset.valid).

invalid_email_format_test() ->
    CS = user:changeset(#{}, #{<<"username">> => <<"alice">>,
                               <<"email">> => <<"not-an-email">>,
                               <<"password_hash">> => <<"hashed">>}),
    ?assertNot(CS#kura_changeset.valid).
```

## Testing security modules

Test your security functions directly:

```erlang
-module(blog_auth_tests).
-include_lib("nova_test/include/nova_test.hrl").

valid_login_test() ->
    Req = nova_test_req:new(post, "/login"),
    Req1 = Req#{params => #{<<"username">> => <<"admin">>,
                             <<"password">> => <<"password">>}},
    ?assertMatch({true, #{authed := true, username := <<"admin">>}},
                 blog_auth:username_password(Req1)).

invalid_password_test() ->
    Req = nova_test_req:new(post, "/login"),
    Req1 = Req#{params => #{<<"username">> => <<"admin">>,
                             <<"password">> => <<"wrong">>}},
    ?assertEqual(false, blog_auth:username_password(Req1)).

missing_params_test() ->
    Req = nova_test_req:new(post, "/login"),
    ?assertEqual(false, blog_auth:username_password(Req)).
```

## Common Test — Integration testing

Common Test is better for full-stack tests where you need the application running. `nova_test` provides an HTTP client that handles startup and port discovery.

Create `test/blog_api_SUITE.erl`:

```erlang
-module(blog_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         test_list_posts/1,
         test_create_post/1,
         test_create_invalid_post/1,
         test_get_post/1,
         test_update_post/1,
         test_delete_post/1,
         test_get_post_not_found/1
        ]).

all() ->
    [test_list_posts,
     test_create_post,
     test_create_invalid_post,
     test_get_post,
     test_update_post,
     test_delete_post,
     test_get_post_not_found].

init_per_suite(Config) ->
    nova_test:start(blog, Config).

end_per_suite(Config) ->
    nova_test:stop(Config).

test_list_posts(Config) ->
    {ok, Resp} = nova_test:get("/api/posts", Config),
    ?assertStatus(200, Resp),
    ?assertJson(#{<<"posts">> := _}, Resp).

test_create_post(Config) ->
    {ok, Resp} = nova_test:post("/api/posts",
                                #{json => #{<<"title">> => <<"Test Post">>,
                                            <<"body">> => <<"Test body">>,
                                            <<"user_id">> => 1}},
                                Config),
    ?assertStatus(201, Resp),
    ?assertJson(#{<<"title">> := <<"Test Post">>}, Resp).

test_create_invalid_post(Config) ->
    {ok, Resp} = nova_test:post("/api/posts",
                                #{json => #{<<"title">> => <<"Hi">>}},
                                Config),
    ?assertStatus(422, Resp),
    ?assertJson(#{<<"errors">> := _}, Resp).

test_get_post(Config) ->
    %% Create a post first
    {ok, CreateResp} = nova_test:post("/api/posts",
                                      #{json => #{<<"title">> => <<"Get Test">>,
                                                  <<"body">> => <<"Body">>,
                                                  <<"user_id">> => 1}},
                                      Config),
    ?assertStatus(201, CreateResp),
    #{<<"id">> := Id} = nova_test:json(CreateResp),

    %% Fetch it
    {ok, Resp} = nova_test:get("/api/posts/" ++ integer_to_list(Id), Config),
    ?assertStatus(200, Resp),
    ?assertJson(#{<<"title">> := <<"Get Test">>}, Resp).

test_update_post(Config) ->
    %% Create a post first
    {ok, CreateResp} = nova_test:post("/api/posts",
                                      #{json => #{<<"title">> => <<"Before Update">>,
                                                  <<"body">> => <<"Body">>,
                                                  <<"user_id">> => 1}},
                                      Config),
    #{<<"id">> := Id} = nova_test:json(CreateResp),

    %% Update it
    {ok, Resp} = nova_test:put("/api/posts/" ++ integer_to_list(Id),
                                #{json => #{<<"title">> => <<"After Update">>}},
                                Config),
    ?assertStatus(200, Resp),
    ?assertJson(#{<<"title">> := <<"After Update">>}, Resp).

test_delete_post(Config) ->
    %% Create a post first
    {ok, CreateResp} = nova_test:post("/api/posts",
                                      #{json => #{<<"title">> => <<"To Delete">>,
                                                  <<"body">> => <<"Body">>,
                                                  <<"user_id">> => 1}},
                                      Config),
    #{<<"id">> := Id} = nova_test:json(CreateResp),

    %% Delete it
    {ok, Resp} = nova_test:delete("/api/posts/" ++ integer_to_list(Id), Config),
    ?assertStatus(204, Resp).

test_get_post_not_found(Config) ->
    {ok, Resp} = nova_test:get("/api/posts/999999", Config),
    ?assertStatus(404, Resp).
```

### Assertion macros

| Macro | Purpose |
|---|---|
| `?assertStatus(Code, Resp)` | Assert the HTTP status code |
| `?assertJson(Pattern, Resp)` | Pattern-match the decoded JSON body |
| `?assertBody(Expected, Resp)` | Assert the raw response body |
| `?assertHeader(Name, Expected, Resp)` | Assert a response header value |

Run Common Test suites:

```shell
rebar3 ct
```

## Test structure

```
test/
├── blog_posts_controller_tests.erl       %% EUnit — controller unit tests
├── post_changeset_tests.erl              %% EUnit — changeset validation
├── blog_auth_tests.erl                   %% EUnit — security functions
└── blog_api_SUITE.erl                    %% Common Test — integration tests
```

```admonish tip
- Use EUnit for fast unit tests of individual functions and changesets
- Use Common Test for integration tests that need the full application running
- Run both with `rebar3 do eunit, ct`
```

---

With testing in place, let's look at how to handle errors gracefully in [Error Handling](error-handling.md).
