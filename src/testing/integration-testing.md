# Integration Testing

Unit tests verify individual functions. Integration tests verify that the full application works end-to-end — HTTP requests go through routing, plugins, security, controllers, and the database.

## Setup

Integration tests use Common Test with `nova_test` helpers that manage application lifecycle and provide an HTTP client.

### Database

Tests need a running PostgreSQL. Use the same `docker-compose.yml` from the [Database Setup](../data-layer/setup.md) chapter:

```shell
docker compose up -d
```

Your test configuration should point at the test database:

```erlang
%% test sys.config
{blog, [
    {database, <<"blog_test">>}
]}
```

## Writing integration tests

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

## Running integration tests

```shell
rebar3 ct
```

## Running both

```shell
rebar3 do eunit, ct
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

Next: [Testing Real-Time](testing-realtime.md) — testing WebSocket handlers and live views.
