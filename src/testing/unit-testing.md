# Unit Testing

Nova controllers are regular Erlang functions — they take a request map and return a tuple. Changesets are pure functions — data in, data out. This makes unit testing straightforward with EUnit.

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

## Testing changesets

Changesets are pure — no database, no side effects. Test them directly:

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

## Testing controllers

```admonish note
The controller tests below call `blog_repo` functions, which need a running database. They are closer to integration tests. For true unit tests, you could mock the repo — but in practice, testing against a real database (as shown in [Integration Testing](integration-testing.md)) catches more bugs. These examples show how to use `nova_test_req` to build request maps.
```

The `nova_test_req` module builds well-formed request maps so you don't have to construct them by hand:

```erlang
-module(blog_posts_controller_tests).
-include_lib("nova_test/include/nova_test.hrl").

show_existing_post_test() ->
    Req = nova_test_req:new(get, "/api/posts/1"),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"1">>}, Req),
    Result = blog_posts_controller:show(Req1),
    ?assertMatch({json, #{id := 1, title := _}}, Result).

show_missing_post_test() ->
    Req = nova_test_req:new(get, "/api/posts/999999"),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"999999">>}, Req),
    Result = blog_posts_controller:show(Req1),
    ?assertMatch({status, 404, _, _}, Result).

create_post_test() ->
    Req = nova_test_req:new(post, "/api/posts"),
    Req1 = nova_test_req:with_json(#{<<"title">> => <<"Test Post">>,
                                     <<"body">> => <<"Test body">>,
                                     <<"user_id">> => 1}, Req),
    Result = blog_posts_controller:create(Req1),
    ?assertMatch({json, 201, _, #{id := _}}, Result).

create_invalid_post_test() ->
    Req = nova_test_req:new(post, "/api/posts"),
    Req1 = nova_test_req:with_json(#{}, Req),
    Result = blog_posts_controller:create(Req1),
    ?assertMatch({json, 422, _, #{errors := _}}, Result).
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

## Testing security modules

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

## Running EUnit tests

```shell
rebar3 eunit
```

---

Next: [Integration Testing](integration-testing.md) — testing the full application with HTTP requests.
