# Sessions

HTTP is stateless. To remember a reader between requests (who is logged in, which flash message to show, which CSRF token belongs to which form) the server keeps a little data per visitor and gives the browser a key to it in a cookie. That is a session.

Nova has sessions built in. This chapter shows how they work in Nova 0.18, what the defaults get wrong, and how the blog hardens them before the next chapter builds login on top.

## How Nova sessions work

Three pieces cooperate:

- **`nova_stream_h`**, a Cowboy stream handler, runs first on every request. If the request has no `session_id` cookie it generates an ID, stores it in the request map under `nova_session_id`, and sets the cookie on the response.
- **`nova_session`** is the API controllers use. It finds the ID (the `nova_session_id` key first, then the cookie) and calls the configured backend.
- **`nova_session_ets`** is the default backend: one ETS table owned by a gen_server. Writes are broadcast with `nova_pubsub`, so every node in a cluster keeps a copy.

So by the time a controller runs, every visitor has a session ID, and data is only stored once something calls `nova_session:set/3`.

## The session API

| Function | Returns |
|----------|---------|
| `nova_session:get(Req, Key)` | `{ok, Value}` or `{error, not_found}` |
| `nova_session:set(Req, Key, Value)` | `ok`, or `{error, session_id_not_set}` when the request has no session ID |
| `nova_session:delete(Req, Key)` | `{ok, Req}`: removes one key |
| `nova_session:delete(Req)` | `{ok, Req1}`: removes the whole session and adds an expired `session_id` cookie to `Req1` |

Keys are binaries. The behaviour types values as binaries too; the ETS backend stores any term, but keep to binaries so a different backend can be swapped in. `delete/1` returns a new request because it changes the response cookie: hand `Req1` back to Nova (`{redirect, Path, Req1}` and the other `..., Req}` return forms) or the browser never hears about it.

The session suite exercises the API directly. Any map with a `nova_session_id` will do as a request:

```erlang
session_api_round_trip(_Config) ->
    Req = #{nova_session_id => blog_session:new_id()},
    {error, not_found} = nova_session:get(Req, ~"theme"),
    ok = nova_session:set(Req, ~"theme", ~"dark"),
    {ok, ~"dark"} = nova_session:get(Req, ~"theme"),
    {ok, Req} = nova_session:delete(Req, ~"theme"),
    {error, not_found} = nova_session:get(Req, ~"theme"),
    ok = nova_session:set(Req, ~"theme", ~"light"),
    {ok, Req1} = nova_session:delete(Req),
    {error, not_found} = nova_session:get(Req, ~"theme"),
    #{resp_cookies := #{~"session_id" := Expired}} = Req1,
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Expired), ~"Max-Age=0")),
    {error, session_id_not_set} = nova_session:set(#{headers => #{}}, ~"theme", ~"dark").
```

## Configuration

Two settings control sessions, and they live in different places:

| Setting | Where | Default | Meaning |
|---------|-------|---------|---------|
| `use_sessions` | your application's section (`blog`) | `true` | `false` turns sessions off: no cookie is set, and any `nova_session` call throws `{nova_session, unsupported_session_used}` |
| `session_manager` | the `nova` section | `nova_session_ets` | The backend module |

`use_sessions` is read with `nova:get_env/2`, which looks in the environment of the application that started Nova (the `bootstrap_application`), so turning sessions off looks like this:

```erlang
[
    {nova, [{bootstrap_application, blog}, ...]},
    {blog, [{use_sessions, false}]}
].
```

Put `use_sessions` in the `nova` section instead and it is silently ignored.

A backend implements the `nova_session` behaviour: `get_value/2`, `set_value/3`, `delete_value/1` (whole session), `delete_value/2` (one key) and an optional `start_link/0`. If the module exports `start_link/0`, Nova starts it under its own supervisor.

```admonish tip title="rebar3 nova doctor"
`rebar3 nova doctor -v` shows the session backend in use: `session_manager = nova_session_ets` for the blog. It also warns when the config sets `session_backend`, a key some older guides use that Nova never reads.
```

## What the defaults get wrong

Look at the cookie a fresh Nova app sets:

```erlang
nova_default_cookie_has_no_flags(_Config) ->
    Req = #{headers => #{}},
    {[], State} = nova_stream_h:init(1, Req, #{stream_handlers => []}),
    #{resp_cookies := #{~"session_id" := Cookie}} = element(3, State),
    [<<"session_id=", _/binary>>] = binary:split(iolist_to_binary(Cookie), ~";", [global]).
```

The cookie is `session_id=...` and nothing else. In Nova 0.18.1:

- **No `HttpOnly`**: any script on the page, including one injected through an XSS bug, can read the session ID.
- **No `Secure`**: the browser sends the cookie over plain HTTP too.
- **No `SameSite`**: the browser falls back to its own default.
- **No `Path`**: the browser scopes the cookie to the directory of the first page visited. A reader whose first request is `/posts/7` gets a cookie for `/posts` only, and a second session as soon as they open `/login`.
- **A weak ID**: `nova_session:generate_session_id/0` builds the ID from `rand:uniform/1`, which is not a cryptographic generator.
- **Any ID is accepted**: Nova does not check that a cookie's ID is one it issued, and nothing rotates the ID when a user logs in. Whoever can plant a cookie in a victim's browser (session fixation) shares the session the victim then logs into.
- **No expiry**: `nova_session_ets` never removes a session unless the app deletes it, and every session is lost when the node restarts.

There is no configuration option for any of this. The blog fixes the first five itself and lives with the last one.

```admonish warning title="Never use the default session cookie for logins"
Until you have done what the rest of this chapter does, do not store anything that identifies a logged-in user in a Nova session.
```

## Hardening the cookie

Cowboy lets an application choose its stream handlers through `cowboy_configuration`. The blog adds one right after `nova_stream_h`. When `nova_stream_h` has just minted an ID for a new visitor (the request map has `nova_session_id`), it replaces the ID with a strong one and sets the cookie again with proper attributes. Setting a cookie with the same name replaces the earlier one. Create `src/blog_session_h.erl`:

```erlang
-module(blog_session_h).
-behaviour(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

init(StreamID, #{nova_session_id := _} = Req, Opts) ->
    SessionId = blog_session:new_id(),
    Req1 = blog_session:put_cookie(SessionId, Req#{nova_session_id => SessionId}),
    cowboy_stream:init(StreamID, Req1, Opts);
init(StreamID, Req, Opts) ->
    cowboy_stream:init(StreamID, Req, Opts).

data(StreamID, IsFin, Data, Next) ->
    cowboy_stream:data(StreamID, IsFin, Data, Next).

info(StreamID, Info, Next) ->
    cowboy_stream:info(StreamID, Info, Next).

terminate(StreamID, Reason, Next) ->
    cowboy_stream:terminate(StreamID, Reason, Next).

early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).
```

A stream handler wraps the next one in the list: `cowboy_stream:init/3` starts the rest of the chain, and the other callbacks pass everything through. Register it in each `sys.config` (`config/dev_sys.config.src`, `config/test_sys.config`, `config/prod_sys.config.src`). Setting `stream_handlers` replaces Nova's list, so repeat Nova's three handlers around the new one:

```erlang
{cowboy_configuration, #{
    port => 8080,
    stream_handlers => [nova_stream_h, blog_session_h, cowboy_compress_h, cowboy_stream_h],
    ...
}},
```

The `...` is the `middleware_handlers` list that [Arizona](../arizona/fundamentals.md) adds; the two keys are independent.

The ID and cookie helpers live in `src/blog_session.erl`, together with the login and logout functions of the next chapter:

```erlang
new_id() ->
    base64:encode(crypto:strong_rand_bytes(32), #{mode => urlsafe, padding => false}).

put_cookie(SessionId, Req) ->
    cowboy_req:set_resp_cookie(~"session_id", SessionId, Req, cookie_options()).

cookie_options() ->
    #{
        path => ~"/",
        http_only => true,
        secure => application:get_env(blog, secure_cookies, true),
        same_site => lax
    }.
```

- 32 bytes from `crypto:strong_rand_bytes/1`, URL-safe base64 without padding: 43 characters that need no escaping in a cookie.
- `path => ~"/"`: one cookie for the whole site.
- `http_only`: invisible to JavaScript.
- `secure`: only sent over HTTPS. Browsers treat `http://localhost` as secure, so development keeps working. If you serve a development build on another hostname over plain HTTP, set `{blog, [{secure_cookies, false}]}` there, and never in production.
- `same_site => lax`: the browser leaves the cookie off cross-site `POST`s, a second line of defence behind the CSRF token. `strict` would also drop it when someone follows a link to the blog from another site, which logs them out for that first page.

Check it:

```shell
curl -si localhost:8080/login | grep -i set-cookie
```

```
set-cookie: session_id=5PI4iB3zXEeh28ZA1fVU2tkkMPziHn8wPfZJGQaSlEo; Path=/; Secure; HttpOnly; SameSite=Lax
```

A returning visitor sends the cookie, Nova finds it, and no new cookie is set.

## Renewing the session

The stream handler cannot stop session fixation: a cookie the attacker planted is a cookie Nova accepts. The fix is to throw the session away whenever the user's privileges change, at login and at logout:

```erlang
renew(Req) ->
    {ok, Req1} = nova_session:delete(Req),
    SessionId = new_id(),
    put_cookie(SessionId, Req1#{nova_session_id => SessionId}).
```

`nova_session:delete/1` drops the old data. The new ID goes into the request map, where `nova_session` looks first, so later `set/3` calls in the same request write to the new session, and the new cookie replaces the expired one `delete/1` queued. The [Authentication](authentication.md) chapter calls `renew/1` from `log_in/2` and `log_out/1`. The test plants a cookie, logs in through it, and checks the planted ID is worthless afterwards:

```erlang
login_replaces_attacker_chosen_session(_Config) ->
    Planted = #{"session_id" => "attacker-knows-this"},
    {200, _, Form, Planted} = blog_web:get(Planted, "/login"),
    {302, _, _, Jar} = blog_web:post_form(Planted, "/login", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "alice@example.com"},
        {"password", ?PASSWORD}
    ]),
    ?assertNotEqual("attacker-knows-this", maps:get("session_id", Jar)),
    {200, _, _, _} = blog_web:get(Jar, "/posts/new"),
    {302, _, _, _} = blog_web:get(Planted, "/posts/new").
```

`blog_web` is the suites' small HTTP client: it keeps a cookie jar as a map and pulls the CSRF token out of a form. It lives in `test/blog_web.erl`.

## Expiry and restarts

`nova_session_ets` keeps sessions until they are deleted or the node stops. The blog does not rely on the session for how long a login lasts: the session only holds a database token, and the token expires after 14 days (next chapter). A restart therefore logs everyone out, and an abandoned session is a few bytes of ETS. If that matters for your app, write a backend that stores sessions in PostgreSQL with an expiry column; the behaviour is small.

## Flash messages

A flash is a message for the next page: "Post created." after a redirect. It is stored in the session and deleted the first time it is read. `src/blog_flash.erl`:

```erlang
-module(blog_flash).

-export([put/3, take/1]).

put(Req, Kind, Message) when Kind =:= info; Kind =:= error ->
    nova_session:set(Req, key(Kind), Message).

take(Req) ->
    [Message || Kind <- [info, error], Message <- take(Req, Kind)].

take(Req, Kind) ->
    case nova_session:get(Req, key(Kind)) of
        {ok, Text} ->
            {ok, _} = nova_session:delete(Req, key(Kind)),
            [#{kind => atom_to_binary(Kind), text => Text}];
        {error, _} ->
            []
    end.

key(info) -> ~"flash_info";
key(error) -> ~"flash_error".
```

`take/1` returns a list of maps ready for a template; [Building Pages](../views-templates/building-pages.md) renders it in the layout. The flash is written with the request the controller returns, so after a `renew/1` use the renewed request:

```erlang
delete(Req) ->
    Req1 = blog_session:log_out(Req),
    ok = blog_flash:put(Req1, info, ~"You have been logged out."),
    {redirect, "/posts", Req1}.
```

The flash test follows a redirect to the login page and reloads it:

```erlang
flash_is_shown_once(_Config) ->
    {302, _, _, Jar} = blog_web:get(#{}, "/posts/new"),
    {200, _, Login, Jar} = blog_web:get(Jar, "/login"),
    ?assertNotEqual(nomatch, binary:match(Login, ~"Please log in first.")),
    {200, _, Again, Jar} = blog_web:get(Jar, "/login"),
    ?assertEqual(nomatch, binary:match(Again, ~"Please log in first.")).
```

## Distributed sessions

Because `nova_session_ets` broadcasts every write with `nova_pubsub`, a session written on one node is readable on the others once the message arrives, and a load balancer does not need sticky sessions. The broadcast is asynchronous: a request that writes on node A and an immediate request that reads on node B can race. Within one request on one node, a `set/3` followed by a `get/2` always sees the write.

Run the session tests with:

```shell
rebar3 ct --suite=test/blog_auth_session_SUITE
```

---

With sessions under control, we can [log people in](authentication.md).
