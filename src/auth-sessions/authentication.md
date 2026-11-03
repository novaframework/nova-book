# Authentication

The blog has users (from [Schemas & Migrations](../data-layer/schemas-migrations.md)) with hashed passwords, and a session we can trust (from [Sessions](sessions.md)). This chapter lets them register, log in and out in the browser, get bearer tokens for the JSON API, and stops anyone guessing passwords at speed. All of it is built on [nova_auth](https://hex.pm/packages/nova_auth) 0.2.

```admonish note title="Pages from Part IV"
The controllers here render HTML with `blog_view:render/3,4`, `blog_view:form/2` and `blog_view:forbidden/0`, from `.dtl` templates, and the routes include `blog_posts_html_controller`. Those come from [Building Pages](../views-templates/building-pages.md) in Part IV. Copy them from the companion app, or read that chapter first.
```

## What nova_auth gives you

`nova_auth` is a toolkit for password authentication on Kura, not a finished login system. The parts the blog uses:

| Module | Does |
|--------|------|
| `nova_auth` | The behaviour your config module implements |
| `nova_auth_password` | PBKDF2-SHA256 hashing and constant-time verification |
| `nova_auth_accounts` | `register/3`, `authenticate/3`, `change_password/4` |
| `nova_auth_session` | Random tokens stored hashed in a tokens table, with an expiry |
| `nova_auth_actor` | Stores the logged-in "actor" map in the Nova session |
| `nova_auth_policy` | Building blocks for authorization rules (next chapter) |
| `nova_auth_rate_limit` | A Nova plugin that rate-limits requests |
| `nova_auth_confirm`, `nova_auth_reset` | Email confirmation and password reset tokens |

You write the controllers, templates and security functions. `nova_auth` is already a dependency (Part II hashes passwords with `nova_auth_password`), so there is nothing to add to `rebar.config`.

## Why not the generators

Two generators claim to do this for you: `rebar3 nova gen_auth` (rebar3_nova 0.13.4) and `rebar3 kura gen_auth` (rebar3_kura 0.16.5). Both write the same scaffold: a `blog_accounts` module, session and registration controllers, a token schema, a migration and a test suite. Run against the blog, the output does not fit:

- It predates nova_auth. The accounts module hashes with `bcrypt`, which is not a dependency, and encodes JSON with `thoas`, which the blog does not use.
- Its migration creates the `users` table again (Part II already has one) and runs `CREATE EXTENSION citext` for a column type it never uses.
- The controllers speak JSON only, with no CSRF, no HTML forms and no session renewal.

Rewriting the output would take longer than writing the few modules below, so the blog does not use the generators. Their file layout is still a good map: the modules here have the same names.

## The token table

`nova_auth_session` keeps one row per login. It needs a schema with `user_id`, `token`, `context` and `inserted_at`. Create `src/schemas/blog_user_token.erl`:

```erlang
-module(blog_user_token).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0, associations/0]).

table() -> ~"user_tokens".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = user_id, type = bigint, nullable = false},
        #kura_field{name = token, type = string, nullable = false},
        #kura_field{name = context, type = string, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime}
    ].

indexes() ->
    [
        {[context, token], #{unique => true}},
        {[user_id], #{}}
    ].

associations() ->
    [
        #kura_assoc{
            name = user,
            type = belongs_to,
            schema = blog_user,
            foreign_key = user_id,
            on_delete = cascade
        }
    ].
```

The same table holds confirmation and reset tokens, told apart by `context` (`session`, `confirm`, `reset`). `on_delete = cascade` removes a user's tokens with the user. Generate the migration:

```shell
rebar3 kura compile
```

It creates `src/migrations/m<timestamp>_create_user_tokens.erl` with the foreign key and both indexes; the next start runs it.

## The config module

`nova_auth` functions take a module that says where things are. Create `src/blog_auth.erl`:

```erlang
-module(blog_auth).
-behaviour(nova_auth).

-export([config/0]).

config() ->
    #{
        repo => blog_repo,
        user_schema => blog_user,
        token_schema => blog_user_token,
        session_validity_days => 14
    }.
```

Everything else has a default, merged in and cached by `nova_auth:config/1`: the user is found by `email`, the hash lives in `hashed_password` (the blog's field names), confirmation tokens last 3 days, reset tokens 1 hour, and tokens are 32 random bytes.

```admonish warning title="hash_algorithm is ignored"
The config type lists `hash_algorithm => pbkdf2_sha256 | bcrypt | argon2`. In 0.2.0 only PBKDF2 is implemented: asking for `bcrypt` or `argon2` silently gives you PBKDF2-SHA256. Leave the key out. The work factor is the `pbkdf2_iterations` setting of the `nova_auth` application (default 600000); the test fixtures lower it to 1000 so suites run fast.
```

## Accounts

`src/blog_accounts.erl` is the blog's front door to `nova_auth`. Controllers call it, never `nova_auth_*` directly:

```erlang
-module(blog_accounts).

-export([
    register/1,
    authenticate/2,
    issue_token/1,
    user_for_token/1,
    revoke_token/1,
    actor/1
]).

register(Params) ->
    nova_auth_accounts:register(
        blog_auth, fun blog_user:registration_changeset/2, normalize_email(Params)
    ).

authenticate(Email, Password) when is_binary(Email), is_binary(Password) ->
    nova_auth_accounts:authenticate(blog_auth, string:casefold(string:trim(Email)), Password);
authenticate(_Email, _Password) ->
    {error, invalid_credentials}.

issue_token(User) ->
    nova_auth_session:generate_session_token(blog_auth, User).

user_for_token(Token) when is_binary(Token) ->
    nova_auth_session:get_user_by_session_token(blog_auth, Token);
user_for_token(_Token) ->
    {error, not_found}.

revoke_token(Token) ->
    nova_auth_session:delete_session_token(blog_auth, Token).

actor(#{id := Id, username := Username, role := Role}) ->
    #{id => Id, provider => password, username => Username, role => Role}.

normalize_email(#{~"email" := Email} = Params) when is_binary(Email) ->
    Params#{~"email" := string:casefold(string:trim(Email))};
normalize_email(Params) ->
    Params.
```

- `register/1` runs `blog_user:registration_changeset/2` from [Changesets](../data-layer/changesets.md), which validates and hashes the password, and inserts the user. Errors come back as a changeset, ready for the form.
- `authenticate/2` returns `{ok, User}` or `{error, invalid_credentials}`, the same error for a wrong password and an unknown email. For an unknown email `nova_auth` still runs a dummy hash, so the response time does not tell an attacker which emails exist. Emails are trimmed and lowercased on the way in and on the way to the database, so `Alice@Example.com` logs in as `alice@example.com`.
- `issue_token/1` creates a row in `user_tokens` and returns the raw token. Only its SHA-256 hash is stored, so a leaked database backup holds no usable tokens.
- `user_for_token/1` turns a token back into a user, or an error once the token is older than `session_validity_days` or has been deleted.
- `actor/1` is the small map the rest of the app (and Arizona) sees as "the current user".

## Logging in and out

Login puts a token in the session; every request looks the token up. `src/blog_session.erl` also holds the cookie helpers from the [Sessions](sessions.md#hardening-the-cookie) chapter:

```erlang
-module(blog_session).

-export([log_in/2, log_out/1, new_id/0, put_cookie/2]).

log_in(Req, User) ->
    {ok, Token} = blog_accounts:issue_token(User),
    Req1 = renew(Req),
    ok = nova_session:set(Req1, ~"user_token", Token),
    ok = nova_auth_actor:store(Req1, blog_accounts:actor(User)),
    Req1.

log_out(Req) ->
    case nova_session:get(Req, ~"user_token") of
        {ok, Token} -> ok = blog_accounts:revoke_token(Token);
        {error, _} -> ok
    end,
    renew(Req).
```

`log_in/2` renews the session first (no fixation), then stores two things in the new one:

- `user_token`, the database token. This is what decides whether the user is logged in: it expires after 14 days, and deleting it (logout, a password change, a reset) ends the login.
- The actor map, through `nova_auth_actor:store/2`. Code that cannot afford a database lookup per call reads this copy; the blog's Arizona live views do.

`log_out/1` deletes the token row, then renews the session so the old cookie is worthless.

The controller, `src/controllers/blog_session_controller.erl`:

```erlang
-module(blog_session_controller).

-export([new/1, create/1, delete/1]).

new(Req) ->
    blog_view:render(Req, blog_login, #{form => blog_view:form([email], #{})}).

create(Req) ->
    Params = maps:get(params, Req, #{}),
    Email = maps:get(~"email", Params, undefined),
    Password = maps:get(~"password", Params, undefined),
    case blog_accounts:authenticate(Email, Password) of
        {ok, User} ->
            Req1 = blog_session:log_in(Req, User),
            ok = blog_flash:put(Req1, info, ~"Welcome back!"),
            {redirect, "/posts", Req1};
        {error, invalid_credentials} ->
            Vars = #{
                form => blog_view:form([email], Params),
                error => ~"Invalid email or password."
            },
            blog_view:render(Req, blog_login, Vars, 422)
    end.

delete(Req) ->
    Req1 = blog_session:log_out(Req),
    ok = blog_flash:put(Req1, info, ~"You have been logged out."),
    {redirect, "/posts", Req1}.
```

- `params` is the form body, decoded by `nova_request_plugin` with `read_urlencoded_body`.
- `{redirect, "/posts", Req1}` returns the renewed request so its `set-cookie` reaches the browser. Returning plain `{redirect, "/posts"}` would log the user in to a session they never receive.
- A failed login renders the form again with 422 and the email filled in; the password is never echoed back.
- Logout is a `POST` with a CSRF token. A `GET /logout` would let any page log your readers out with an `<img>` tag.

`blog_view:render/3,4` and the templates are the subject of [Building Pages](../views-templates/building-pages.md).

## Registration

`src/controllers/blog_registration_controller.erl`:

```erlang
-module(blog_registration_controller).

-export([new/1, create/1]).

-define(FIELDS, [email, username, full_name]).

new(Req) ->
    blog_view:render(Req, blog_register, #{form => blog_view:form(?FIELDS, #{}), errors => #{}}).

create(Req) ->
    Params = maps:get(params, Req, #{}),
    case blog_accounts:register(Params) of
        {ok, User} ->
            Req1 = blog_session:log_in(Req, User),
            ok = blog_flash:put(Req1, info, ~"Welcome to the blog!"),
            {redirect, "/posts", Req1};
        {error, CS} ->
            Vars = #{form => blog_view:form(?FIELDS, Params), errors => blog_view:errors(CS)},
            blog_view:render(Req, blog_register, Vars, 422)
    end.
```

`?FIELDS` lists what goes back into the form after an error. The passwords are left out on purpose. `role` is not in the changeset's cast list, so a hand-crafted `role=admin` field is ignored and every new user is an `author`. The unique index on `email` turns a duplicate into a "has already been taken" error on the form.

## Security functions

A Nova route group's `security` function runs before the controller (see [Plugins](../foundations/plugins.md)). The blog has four, in `src/blog_security.erl`:

```erlang
-module(blog_security).

-export([current_user/1, require_user/1, require_admin/1, require_bearer/1, bearer_token/1]).
-export([session_actor/1]).

current_user(Req) ->
    case session_actor(Req) of
        {ok, Actor} -> {true, Actor};
        {error, _} -> true
    end.

require_user(Req) ->
    case session_actor(Req) of
        {ok, Actor} ->
            {true, Actor};
        {error, _} ->
            ok = blog_flash:put(Req, error, ~"Please log in first."),
            {redirect, "/login"}
    end.
```

[Routing](../foundations/routing.md#security) lists what a security function may return. These two use `{true, Actor}` to continue with the actor under `auth_data`, plain `true` to continue without one, and `{redirect, "/login"}` (a string: a binary path crashes the handler).

`current_user/1` never refuses: it is for pages anyone can see, where a logged-in reader gets more (the post page shows "Edit" to the author). `require_user/1` sends anonymous visitors to the login page with a flash. Both read the session through one function:

```erlang
session_actor(Req) ->
    case nova_session:get(Req, ~"user_token") of
        {ok, Token} ->
            case blog_accounts:user_for_token(Token) of
                {ok, User} ->
                    {true, Actor} = authenticated(User),
                    {ok, Actor};
                {error, _} = Error ->
                    {ok, _} = nova_auth_actor:delete(Req),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

authenticated(#{id := Id} = User) ->
    ok = kura_audit:set_actor(integer_to_binary(Id)),
    {true, blog_accounts:actor(User)}.
```

Every request checks the token in the database, so a revoked or expired token takes effect immediately. When the token is gone, the cached actor is cleared too. `authenticated/1` also tells [Kura's audit log](../data-layer/data-safety.md) who is acting; the [Authorization](authorization.md#the-audit-trail) chapter tests that.

`nova_auth_security:require_authenticated/1` exists as a ready-made security function, but it only checks for an actor and answers with a JSON 401: right for an API, wrong for HTML pages, and it would trust the cached actor without asking the database.

```admonish note title="CSRF runs before security"
Plugins run before the security function. On a route group with `nova_csrf_plugin`, an anonymous `POST` without a CSRF token gets a 403 from the plugin, not a redirect to `/login`. That is the right answer for a forged request; just do not be surprised by it in tests.
```

## The actor and Arizona

The actor stored at login contains at least `id` (Arizona relies on that) and `provider`:

```erlang
#{id => 2, provider => password, username => ~"alice", role => author}
```

The live views of Part VI do not trust that copy: their middleware calls `session_actor/1` above, so every live page mount and WebSocket connect checks the token in the database and redirects to `/login` when it is gone ([Live Views](../arizona/live-views.md)). That matters because a password change or reset only deletes tokens; the cached actor stays in the session until `session_actor/1` clears it after the next failed check. [Security](../production/security.md#live-views-verify-the-token) has the test.

## The routes

The router gets four new groups for the HTML side, before the last group (static files and the 404 handler):

```erlang
#{
    prefix => "",
    security => fun blog_security:current_user/1,
    plugins => html_plugins(),
    routes => [
        {"/posts", fun blog_posts_html_controller:index/1, #{methods => [get]}},
        {"/posts/:id", fun blog_posts_html_controller:show/1, #{methods => [get]}},
        {"/login", fun blog_session_controller:new/1, #{methods => [get]}},
        {"/register", fun blog_registration_controller:new/1, #{methods => [get]}}
    ]
},
#{
    prefix => "",
    security => false,
    plugins => [
        {pre_request, blog_rate_limit, login_rate_limit()} | html_plugins()
    ],
    routes => [
        {"/login", fun blog_session_controller:create/1, #{methods => [post]}},
        {"/register", fun blog_registration_controller:create/1, #{methods => [post]}}
    ]
},
#{
    prefix => "",
    security => fun blog_security:require_user/1,
    plugins => html_plugins(),
    routes => [
        {"/posts/new", fun blog_posts_html_controller:new/1, #{methods => [get]}},
        {"/posts", fun blog_posts_html_controller:create/1, #{methods => [post]}},
        {"/posts/:id/edit", fun blog_posts_html_controller:edit/1, #{methods => [get]}},
        {"/posts/:id", fun blog_posts_html_controller:update/1, #{methods => [post]}},
        {"/posts/:id/delete", fun blog_posts_html_controller:delete/1, #{
            methods => [post]
        }},
        {"/logout", fun blog_session_controller:delete/1, #{methods => [post]}}
    ]
},
#{
    prefix => "/admin",
    security => fun blog_security:require_admin/1,
    plugins => html_plugins(),
    routes => [
        {"/users", fun blog_admin_controller:users/1, #{methods => [get]}}
    ]
},
```

with two helpers at the bottom of `src/blog_router.erl`:

```erlang
html_plugins() ->
    [
        {pre_request, nova_request_plugin, #{read_urlencoded_body => true}},
        {pre_request, nova_csrf_plugin, #{}}
    ].

login_rate_limit() ->
    #{
        limiter => blog_login_limiter,
        limit => 5,
        window => 60000,
        algorithm => sliding_window,
        key_fun => fun blog_rate_limit:client_ip/1
    }.
```

A group's `plugins` replace the global plugin list rather than adding to it, which is why every HTML group repeats `html_plugins()`. The posts pages and the admin group belong to the next chapters; they are here so the router is shown once.

## API tokens

Browsers have cookies; API clients send a token in the `authorization` header. The blog uses the same `nova_auth_session` tokens for both: a token is a row in `user_tokens`, whether it sits in a session or in a script.

`src/controllers/blog_tokens_controller.erl` swaps credentials for a token and revokes it:

```erlang
-module(blog_tokens_controller).

-export([create/1, delete/1]).

create(#{json := #{~"email" := Email, ~"password" := Password}}) ->
    case blog_accounts:authenticate(Email, Password) of
        {ok, User} ->
            {ok, Token} = blog_accounts:issue_token(User),
            ValidFor = nova_auth:config(blog_auth, session_validity_days) * 86400,
            {json, 201, #{}, #{token => Token, token_type => ~"Bearer", expires_in => ValidFor}};
        {error, invalid_credentials} ->
            {json, 401, #{}, #{error => ~"invalid email or password"}}
    end;
create(_Req) ->
    {json, 400, #{}, #{error => ~"expected email and password"}}.

delete(Req) ->
    {ok, Token} = blog_security:bearer_token(Req),
    ok = blog_accounts:revoke_token(Token),
    {status, 204}.
```

The security function for the API checks the header:

```erlang
require_bearer(Req) ->
    case bearer_token(Req) of
        {ok, Token} ->
            case blog_accounts:user_for_token(Token) of
                {ok, User} -> authenticated(User);
                {error, _} -> unauthorized(~"invalid or expired token")
            end;
        error ->
            unauthorized(~"missing bearer token")
    end.

bearer_token(Req) ->
    case cowboy_req:header(~"authorization", Req) of
        <<Scheme:6/binary, " ", Token/binary>> ->
            case string:lowercase(Scheme) of
                ~"bearer" -> {ok, string:trim(Token)};
                _ -> error
            end;
        _ ->
            error
    end.

unauthorized(Message) ->
    Headers = #{~"content-type" => ~"application/json", ~"www-authenticate" => ~"Bearer"},
    {false, 401, Headers, iolist_to_binary(json:encode(#{error => Message}))}.
```

`cowboy_req:parse_header(~"authorization", Req)` would also parse a bearer token, but it raises on a malformed header, which a client can turn into a 500 at will. Matching the bytes is simpler and always answers 401.

The API from [CRUD & the Query Builder](../data-layer/crud.md) is split: reads stay public, writes move into a group behind `require_bearer/1`, and `POST /api/tokens` gets its own group with the JSON body plugin and the rate limit:

```erlang
#{
    prefix => "/api",
    security => false,
    routes => [
        {"/posts", fun blog_posts_controller:index/1, #{methods => [get]}},
        {"/posts/:id", fun blog_posts_controller:show/1, #{methods => [get]}}
    ]
},
#{
    prefix => "/api",
    security => fun blog_security:require_bearer/1,
    routes => [
        {"/posts", fun blog_posts_controller:create/1, #{methods => [post]}},
        {"/posts/:id", fun blog_posts_controller:update/1, #{methods => [put]}},
        {"/posts/:id", fun blog_posts_controller:delete/1, #{methods => [delete]}},
        {"/posts/:id/comments", fun blog_posts_controller:create_comment/1, #{
            methods => [post]
        }},
        {"/posts/:id/tags", fun blog_posts_controller:update_tags/1, #{methods => [put]}},
        {"/tokens", fun blog_tokens_controller:delete/1, #{methods => [delete]}}
    ]
},
#{
    prefix => "/api",
    security => false,
    plugins => [
        {pre_request, nova_request_plugin, #{decode_json_body => true}},
        {pre_request, blog_rate_limit, login_rate_limit()}
    ],
    routes => [
        {"/tokens", fun blog_tokens_controller:create/1, #{methods => [post]}}
    ]
},
```

The write group has no `plugins` key, so it uses the global plugins, which decode the JSON body. `current_author/1` is gone from `blog_posts_controller`: the author is the actor from the token, and a `user_id` in the body is ignored.

```erlang
create(#{json := Params, auth_data := Actor}) ->
    case blog_posts:create_post(Actor, Params) of
        {ok, Post} -> {json, 201, #{}, blog_api_json:post(Post)};
        {error, #kura_changeset{} = CS} -> unprocessable(CS)
    end;
```

`create_comment/1` does the same. From here on the curl examples of Part II need a token. Get one:

```shell
curl -s -X POST localhost:8080/api/tokens -H 'content-type: application/json' \
  -d '{"email": "alice@example.com", "password": "correct horse battery"}'
```

```
{"token":"42tUXOo6dXemi5rpRE/wJ8HA2jPHzQjzl7KuwFTi3nE=","expires_in":1209600,"token_type":"Bearer"}
```

Without it a write is refused:

```shell
curl -si -X POST localhost:8080/api/posts -H 'content-type: application/json' \
  -d '{"title": "Hello", "body": "From curl"}'
```

```
HTTP/1.1 401 Unauthorized
content-length: 32
content-type: application/json
www-authenticate: Bearer

{"error":"missing bearer token"}
```

With it, the post belongs to the token's user:

```shell
curl -s -X POST localhost:8080/api/posts -H "authorization: Bearer $TOKEN" \
  -H 'content-type: application/json' \
  -d '{"title": "Hello tokens", "body": "Posted with a bearer token.", "status": "published"}'
```

```
{"id":2,"status":"published","title":"Hello tokens","metadata":null,"body":"Posted with a bearer token.","user_id":2,"inserted_at":"2026-11-03T08:47:38Z","published_at":"2026-11-03T08:47:38Z","lock_version":1}
```

`DELETE /api/tokens` with the same header revokes it (`HTTP/1.1 204 No Content`), and the next write with that token gets `{"error":"invalid or expired token"}`.

```admonish warning title="Tokens are standard base64"
`nova_auth` tokens use standard base64, so they can contain `+`, `/` and `=`. That is fine in a header. Anywhere else (a query string, a link in an email) URL-encode them with `uri_string:compose_query/1`, and never put one in a path segment.
```

## Rate limiting login

Without a limit, anyone can try thousands of passwords a minute. `nova_auth_rate_limit` is a Nova plugin over the [seki](https://hex.pm/packages/seki) rate limiter. It cannot be used directly in Nova 0.18.1, for two reasons:

- It reads its options as its state, so every option must be given; there are no defaults, whatever its documentation says. A missing key crashes with `badmatch`.
- When it refuses a request it replies 429 and returns `{stop, Req}`. Nova's plugin handler accepts only `{stop, Req, State}` (and a 4-tuple), so the request process crashes after the reply has gone out, logging the whole request, cookies included.

`src/blog_rate_limit.erl` wraps it and fixes the return value:

```erlang
-module(blog_rate_limit).
-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0, client_ip/1]).

pre_request(Req, Env, Options, State) ->
    case nova_auth_rate_limit:pre_request(Req, Env, Options, State) of
        {stop, Req1} -> {stop, Req1, State};
        Allowed -> Allowed
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
        title => ~"blog_rate_limit",
        version => ~"1.0.0",
        url => ~"https://github.com/novaframework/nova-book",
        authors => [~"The Nova Book"],
        description => ~"nova_auth_rate_limit with a stop reply Nova accepts"
    }.

client_ip(Req) ->
    {IP, _Port} = cowboy_req:peer(Req),
    IP.
```

`login_rate_limit/0` in the router gives all the options: five attempts per client IP per sliding minute. The three routes that check a password (`POST /login`, `POST /register` and `POST /api/tokens`) share the limiter `blog_login_limiter`, so a script cannot get five tries on each. The sixth attempt within a minute:

```
401 401 401 401 429 429
HTTP/1.1 429 Too Many Requests
content-length: 29
content-type: application/json
retry-after: 12
x-ratelimit-remaining: 0

{"error":"too many requests"}
```

(The first request of that run fetched a token, so the fifth failed attempt is already the sixth request.) The 429 body is JSON even on the HTML routes; a person who has failed five times in a minute can live with that.

```admonish warning title="Behind a proxy"
`cowboy_req:peer/1` is the address of whatever connected to Cowboy. Behind a load balancer that is the load balancer, and every visitor shares one limit. Read the client address from `x-forwarded-for` in `client_ip/1` only if your proxy sets that header and strips any value the client sent.
```

## Confirmation and password reset

`nova_auth_confirm` and `nova_auth_reset` complete the set. Each issues a token for a user and later redeems it:

```erlang
{ok, Token} = nova_auth_confirm:generate_confirm_token(blog_auth, User),
{ok, Confirmed} = nova_auth_confirm:confirm_user(blog_auth, Token),

{ok, Reset} = nova_auth_reset:generate_reset_token(blog_auth, User),
{ok, Updated} = nova_auth_reset:reset_password(blog_auth, Reset, #{
    ~"password" => New, ~"password_confirmation" => New
}),
```

`confirm_user/2` sets `confirmed_at`; `reset_password/3` runs the schema's `password_changeset/2` and deletes all the user's tokens, logging them out everywhere. Both tokens work once. They are meant to be emailed as links. Part VIII's [Transactional Email](../email/transactional-email.md#confirming-addresses-and-resetting-passwords) sends them from a background job, adds the pages that redeem them, and makes `register/1` above send the confirmation. `blog_auth_accounts_SUITE` checks both.

```admonish info title="Single sign-on with nova_auth_oidc"
To log in with Google, GitHub or any OpenID Connect provider, add [nova_auth_oidc](https://hex.pm/packages/nova_auth_oidc) 0.2.1. Its controller handles the redirect and the callback and stores an actor in the session with the same `nova_auth_actor` the blog uses, so `id`, `provider` and the Arizona live views carry over. Two differences from the password flow. There is no database token, so security functions must accept the actor alone. And in 0.2.1 the callback does not renew the session, so a planted session ID survives the login: call the callback from your own controller, then move the actor into a renewed session as `blog_session:log_in/2` does. The project's [README](https://github.com/novaframework/nova_auth_oidc) has the setup.
```

## Testing

`test/blog_web.erl` is a small HTTP client for the suites: it keeps cookies in a map, extracts CSRF tokens from forms and logs in. A login test reads like the browser session it replaces:

```erlang
login_sets_session_and_redirects(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/login"),
    AnonymousId = maps:get("session_id", Jar),
    {302, Headers, _, Jar1} = blog_web:post_form(Jar, "/login", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "alice@example.com"},
        {"password", ?PASSWORD}
    ]),
    "/posts" = blog_web:header("location", Headers),
    ?assertNotEqual(AnonymousId, maps:get("session_id", Jar1)),
    {200, _, Page, _} = blog_web:get(Jar1, "/posts"),
    ?assertNotEqual(nomatch, binary:match(Page, ~"Welcome back!")),
    ?assertNotEqual(nomatch, binary:match(Page, ~"<span class=\"who\">alice</span>")),
    {200, _, _, _} = blog_web:get(Jar1, "/posts/new"),
    {302, _, _, _} = blog_web:get(Jar, "/posts/new"),
    Req = #{nova_session_id => list_to_binary(maps:get("session_id", Jar1))},
    {ok, #{id := _, provider := password, username := ~"alice", role := author}} =
        nova_auth_actor:fetch(Req).
```

The last two lines check that login caches the actor, with its `id`, in the session. The rate-limit tests call `blog_web:reset_rate_limit/0` in `init_per_testcase/2`, since every test request comes from 127.0.0.1. For API tests, `blog_web:api_token/2` gets a token through `POST /api/tokens`, as a client would, and `blog_data_fixtures:request_as/3,4` sends it (`blog_data_fixtures:token/1` issues one directly, for tests that are not about logging in):

```erlang
post_author_comes_from_token(Config) ->
    #{id := AliceId} = ?config(alice, Config),
    Token = blog_web:api_token("alice@example.com", "correct horse battery"),
    {201, #{~"user_id" := AliceId}} = blog_data_fixtures:request_as(Token, post, "/api/posts", #{
        title => ~"Mine", body => ~"Written by Alice", user_id => AliceId + 1000
    }).
```

Run the authentication suites:

```shell
rebar3 ct --suite=test/blog_auth_login_SUITE,test/blog_auth_api_SUITE,test/blog_auth_accounts_SUITE
```

---

Knowing who someone is, the next question is what they may do: [Authorization](authorization.md).
