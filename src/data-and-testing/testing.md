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

## EUnit — Unit testing controllers

Nova controllers are regular Erlang functions that receive a request map and return a tuple. The `nova_test_req` module builds well-formed request maps so you don't have to construct them by hand.

Create `test/my_first_nova_api_controller_tests.erl`:

```erlang
-module(my_first_nova_api_controller_tests).
-include_lib("nova_test/include/nova_test.hrl").

show_existing_user_test() ->
    Req = nova_test_req:new(get, "/users/1"),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"1">>}, Req),
    Result = my_first_nova_api_controller:show(Req1),
    ?assertJsonResponse(#{id := 1, name := _, email := _}, Result).

show_missing_user_test() ->
    Req = nova_test_req:new(get, "/users/999999"),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"999999">>}, Req),
    Result = my_first_nova_api_controller:show(Req1),
    ?assertStatusResponse(404, Result).

create_user_test() ->
    Req = nova_test_req:new(post, "/users"),
    Req1 = nova_test_req:with_json(#{<<"name">> => <<"Alice">>,
                                     <<"email">> => <<"alice@example.com">>}, Req),
    Result = my_first_nova_api_controller:create(Req1),
    ?assertJsonResponse(201, #{id := _}, Result).

create_missing_params_test() ->
    Req = nova_test_req:new(post, "/users"),
    Req1 = nova_test_req:with_json(#{}, Req),
    Result = my_first_nova_api_controller:create(Req1),
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

Run EUnit tests with:

```shell
rebar3 eunit
```

## Testing without database dependency

For pure unit tests, separate logic from data access:

```erlang
-module(my_first_nova_request_tests).
-include_lib("nova_test/include/nova_test.hrl").

parse_json_body_test() ->
    Req = nova_test_req:new(post, "/users"),
    Req1 = nova_test_req:with_json(#{<<"name">> => <<"Alice">>,
                                     <<"email">> => <<"alice@example.com">>}, Req),
    #{json := #{<<"name">> := Name, <<"email">> := Email}} = Req1,
    ?assertEqual(<<"Alice">>, Name),
    ?assertEqual(<<"alice@example.com">>, Email).

parse_bindings_test() ->
    Req = nova_test_req:new(get, "/users/42"),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"42">>}, Req),
    #{bindings := #{<<"id">> := Id}} = Req1,
    ?assertEqual(42, binary_to_integer(Id)).
```

```admonish tip
Use `-ifdef(TEST)` to export helper functions only in test builds:

~~~erlang
-ifdef(TEST).
-export([row_to_map/1]).
-endif.
~~~
```

## Common Test — Integration testing

Common Test is better for full-stack tests where you need the application running. `nova_test` provides an HTTP client that handles startup and port discovery.

Create `test/my_first_nova_api_SUITE.erl`:

```erlang
-module(my_first_nova_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         test_get_users/1,
         test_create_user/1,
         test_get_user_not_found/1
        ]).

all() ->
    [test_get_users,
     test_create_user,
     test_get_user_not_found].

init_per_suite(Config) ->
    nova_test:start(my_first_nova, Config).

end_per_suite(Config) ->
    nova_test:stop(Config).

test_get_users(Config) ->
    {ok, Resp} = nova_test:get("/api/users", Config),
    ?assertStatus(200, Resp),
    ?assertJson(#{<<"users">> := [_ | _]}, Resp).

test_create_user(Config) ->
    {ok, Resp} = nova_test:post("/api/users",
                                #{json => #{<<"name">> => <<"Test User">>,
                                            <<"email">> => <<"test@example.com">>}},
                                Config),
    ?assertStatus(201, Resp),
    ?assertJson(#{<<"name">> := <<"Test User">>}, Resp).

test_get_user_not_found(Config) ->
    {ok, Resp} = nova_test:get("/api/users/999999", Config),
    ?assertStatus(404, Resp).
```

`nova_test:start/2` boots your application and discovers the port. All HTTP functions (`get`, `post`, `put`, `patch`, `delete`) accept a path, optional options, and the Config.

### Assertion macros

| Macro | Purpose |
|---|---|
| `?assertStatus(Code, Resp)` | Assert the HTTP status code |
| `?assertJson(Pattern, Resp)` | Pattern-match the decoded JSON body |
| `?assertBody(Expected, Resp)` | Assert the raw response body |
| `?assertHeader(Name, Expected, Resp)` | Assert a response header value |

Run Common Test suites with:

```shell
rebar3 ct
```

## Testing security modules

Test your security functions directly:

```erlang
-module(my_first_nova_auth_tests).
-include_lib("nova_test/include/nova_test.hrl").

valid_login_test() ->
    Req = nova_test_req:new(post, "/login"),
    Req1 = nova_test_req:with_json(#{<<"username">> => <<"admin">>,
                                     <<"password">> => <<"password">>}, Req),
    ?assertMatch({true, #{authed := true, username := <<"admin">>}},
                 my_first_nova_auth:username_password(Req1)).

invalid_password_test() ->
    Req = nova_test_req:new(post, "/login"),
    Req1 = nova_test_req:with_json(#{<<"username">> => <<"admin">>,
                                     <<"password">> => <<"wrong">>}, Req),
    ?assertEqual(false, my_first_nova_auth:username_password(Req1)).

missing_params_test() ->
    Req = nova_test_req:new(post, "/login"),
    ?assertEqual(false, my_first_nova_auth:username_password(Req)).
```

## Test structure

```
test/
├── my_first_nova_api_controller_tests.erl   %% EUnit
├── my_first_nova_auth_tests.erl             %% EUnit
├── my_first_nova_request_tests.erl          %% EUnit
└── my_first_nova_api_SUITE.erl              %% Common Test
```

```admonish tip
- Use EUnit for fast unit tests of individual functions
- Use Common Test for integration tests that need the full application running
- Run both with `rebar3 do eunit, ct`
```

---

With testing in place, it is time to put everything together and build a complete [CRUD application](../building-app/crud-app.md).
