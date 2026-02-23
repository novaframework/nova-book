# Views, Auth & Sessions

In this chapter we will build a login page with ErlyDTL templates, add authentication to protect routes, and wire up sessions so users stay logged in across requests.

## Views with ErlyDTL

Nova uses [ErlyDTL](https://github.com/erlydtl/erlydtl) for HTML templating — an Erlang implementation of [Django's template language](https://django.readthedocs.io/en/1.6.x/ref/templates/builtins.html). Templates live in `src/views/` and are compiled to Erlang modules at build time.

### Creating a login template

Create `src/views/login.dtl`:

```html
<html>
<body>
  <div>
    {% if error %}<p style="color:red">{{ error }}</p>{% endif %}
    <form action="/login" method="post">
      <label for="username">Username:</label>
      <input type="text" id="username" name="username"><br>
      <label for="password">Password:</label>
      <input type="password" id="password" name="password"><br>
      <input type="submit" value="Submit">
    </form>
  </div>
</body>
</html>
```

This form POSTs to `/login` with `username` and `password` fields. The URL-encoded body will be decoded by `nova_request_plugin` (which we configured in the [Plugins](plugins.md) chapter).

### Adding a controller function

Our generated controller is in `src/controllers/blog_main_controller.erl`:

```erlang
-module(blog_main_controller).
-export([
         index/1,
         login/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.

login(_Req) ->
    {ok, [], #{view => login}}.
```

The return tuple `{ok, [], #{view => login}}` tells Nova:
- `ok` — render a template
- `[]` — no template variables
- `#{view => login}` — use the `login` template (matches `login.dtl`)

### How template resolution works

When a controller returns `{ok, Variables}` (without a `view` option), Nova looks for a template named after the controller module. For `blog_main_controller:index/1`, it looks for `blog_main.dtl`.

When you specify `#{view => login}`, Nova uses `login.dtl` instead.

## Authentication

Now let's handle the login form submission with a security module.

### Security in route groups

Authentication in Nova is configured per route group using the `security` key. It points to a function that receives the request and returns either `{true, AuthData}` (allow) or `false` (deny).

### Creating a security module

Create `src/blog_auth.erl`:

```erlang
-module(blog_auth).
-export([
         username_password/1,
         session_auth/1
        ]).

%% Used for the login POST
username_password(#{params := Params}) ->
    case Params of
        #{<<"username">> := Username,
          <<"password">> := <<"password">>} ->
            {true, #{authed => true, username => Username}};
        _ ->
            false
    end.

%% Used for pages that need an active session
session_auth(Req) ->
    case nova_session:get(Req, <<"username">>) of
        {ok, Username} ->
            {true, #{authed => true, username => Username}};
        {error, _} ->
            false
    end.
```

`username_password/1` checks the decoded form parameters. If the password matches, it returns `{true, AuthData}` — the auth data map is attached to the request and accessible in your controller as `auth_data`.

`session_auth/1` checks for an existing session (we will set this up next).

```admonish warning
This is a hardcoded password for demonstration only. In a real application you would validate credentials against a database with properly hashed passwords.
```

### How security works

The security flow for each request is:

1. Nova matches the request to a route group
2. If `security` is `false`, skip to the controller
3. If `security` is a function, call it with the request map
4. If it returns `{true, AuthData}`, merge `auth_data => AuthData` into the request and continue to the controller
5. If it returns `false`, trigger the 401 error handler

You can have different security functions for different route groups — one for API token auth, another for session auth, and so on.

## Sessions

Nova has a built-in session system backed by ETS (Erlang Term Storage). Session IDs are stored in a `session_id` cookie.

### The session API

```erlang
nova_session:get(Req, <<"key">>)           -> {ok, Value} | {error, not_found}.
nova_session:set(Req, <<"key">>, Value)    -> ok.
nova_session:delete(Req)                   -> {ok, Req1}.
nova_session:delete(Req, <<"key">>)        -> {ok, Req1}.
nova_session:generate_session_id()         -> {ok, SessionId}.
```

The session manager is configured in `sys.config`:

```erlang
{nova, [
    {session_manager, nova_session_ets}
]}
```

`nova_session_ets` is the default. It stores session data in an ETS table and replicates changes across clustered nodes using `nova_pubsub`.

### Wiring up the login flow

Update the controller to create a session on successful login:

```erlang
-module(blog_main_controller).
-export([
         index/1,
         login/1,
         login_post/1,
         logout/1
        ]).

index(#{auth_data := #{authed := true, username := Username}}) ->
    {ok, [{message, <<"Hello ", Username/binary>>}]};
index(_Req) ->
    {redirect, "/login"}.

login(_Req) ->
    {ok, [], #{view => login}}.

login_post(#{auth_data := #{authed := true, username := Username}} = Req) ->
    {ok, SessionId} = nova_session:generate_session_id(),
    Req1 = cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req,
                                       #{path => <<"/">>, http_only => true}),
    nova_session_ets:set_value(SessionId, <<"username">>, Username),
    {redirect, "/"};
login_post(_Req) ->
    {ok, [{error, <<"Invalid username or password">>}], #{view => login}}.

logout(Req) ->
    {ok, _Req1} = nova_session:delete(Req),
    {redirect, "/login"}.
```

The login flow:
1. Generate a session ID
2. Set the `session_id` cookie on the response
3. Store the username in the session
4. Redirect to the home page

### Updating the routes

```erlang
routes(_Environment) ->
  [
    %% Public routes
    #{prefix => "",
      security => false,
      routes => [
                 {"/login", fun blog_main_controller:login/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
    },

    %% Login POST (uses username/password auth)
    #{prefix => "",
      security => fun blog_auth:username_password/1,
      routes => [
                 {"/login", fun blog_main_controller:login_post/1, #{methods => [post]}}
                ]
    },

    %% Protected pages (uses session auth)
    #{prefix => "",
      security => fun blog_auth:session_auth/1,
      routes => [
                 {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                 {"/logout", fun blog_main_controller:logout/1, #{methods => [get]}}
                ]
    }
  ].
```

Now the flow is:
1. User visits `/login` — sees the login form
2. Form POSTs to `/login` — `username_password/1` checks credentials
3. On success, a session is created and the user is redirected to `/`
4. On `/`, `session_auth/1` checks the session cookie
5. `/logout` deletes the session and redirects to `/login`

### Cookie options

When setting the session cookie, control its behaviour with options:

```erlang
cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req, #{
    path => <<"/">>,          %% Cookie is valid for all paths
    http_only => true,        %% Not accessible from JavaScript
    secure => true,           %% Only sent over HTTPS
    max_age => 86400          %% Expires after 24 hours (in seconds)
}).
```

```admonish warning
For production, always set `http_only` and `secure` to `true`.
```

### Custom session backends

If you want to store sessions in a database or Redis instead of ETS, implement the `nova_session` behaviour:

```erlang
-module(my_redis_session).
-behaviour(nova_session).

-export([start_link/0,
         get_value/2,
         set_value/3,
         delete_value/1,
         delete_value/2]).

start_link() ->
    ignore.

get_value(SessionId, Key) ->
    {ok, Value}.

set_value(SessionId, Key, Value) ->
    ok.

delete_value(SessionId) ->
    ok.

delete_value(SessionId, Key) ->
    ok.
```

Then configure it:

```erlang
{nova, [
    {session_manager, my_redis_session}
]}
```

---

We now have a complete authentication and session system. Next, let's set up a database layer with [Kura](../data-layer/setup.md).
