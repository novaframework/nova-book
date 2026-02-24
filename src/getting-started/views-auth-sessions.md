# Views, Auth & Sessions

In this chapter we will build a login page with ErlyDTL templates, add authentication to protect routes, and wire up sessions so users stay logged in across requests.

## Views with ErlyDTL

Nova uses [ErlyDTL](https://github.com/erlydtl/erlydtl) for HTML templating — an Erlang implementation of [Django's template language](https://django.readthedocs.io/en/1.6.x/ref/templates/builtins.html). Templates live in `src/views/` and are compiled to Erlang modules at build time.

### Template basics

ErlyDTL supports the same syntax as Django templates:

| Syntax | Purpose | Example |
|--------|---------|---------|
| `{{ var }}` | Output a variable | `{{ username }}` |
| `{% if cond %}...{% endif %}` | Conditional | `{% if error %}...{% endif %}` |
| `{% for x in list %}...{% endfor %}` | Loop | `{% for post in posts %}...{% endfor %}` |
| `{{ var\|filter }}` | Apply a filter | `{{ name\|upper }}` |
| `{{ var\|default:"n/a" }}` | Fallback value | `{{ bio\|default:"No bio" }}` |
| `{% extends "base.dtl" %}` | Inherit a layout | See below |
| `{% block name %}...{% endblock %}` | Override a block | See below |

See the [ErlyDTL documentation](https://github.com/erlydtl/erlydtl) for the full list of tags and filters.

### Creating a base layout

Most pages share the same outer HTML. Template inheritance lets you define a base layout once and override specific blocks in child templates.

Create `src/views/base.dtl`:

```html
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>{% block title %}Blog{% endblock %}</title>
</head>
<body>
  <nav>
    {% if username %}
      <span>{{ username }}</span> | <a href="/logout">Logout</a>
    {% else %}
      <a href="/login">Login</a>
    {% endif %}
  </nav>
  <main>
    {% block content %}{% endblock %}
  </main>
</body>
</html>
```

Child templates use `{% extends "base.dtl" %}` and fill in the blocks they need. Anything outside a `{% block %}` tag in the child is ignored.

### Creating a login template

Create `src/views/login.dtl`:

```html
{% extends "base.dtl" %}

{% block title %}Login{% endblock %}

{% block content %}
<div>
  {% if error %}<p style="color:red">{{ error }}</p>{% endif %}
  <form action="/login" method="post">
    <input type="hidden" name="_csrf_token" value="{{ csrf_token }}" />
    <label for="username">Username:</label>
    <input type="text" id="username" name="username"><br>
    <label for="password">Password:</label>
    <input type="password" id="password" name="password"><br>
    <input type="submit" value="Submit">
  </form>
</div>
{% endblock %}
```

This form POSTs to `/login` with `username` and `password` fields. The URL-encoded body will be decoded by `nova_request_plugin` (which we configured in the [Plugins](plugins.md) chapter).

The hidden `_csrf_token` field is required because we enabled `nova_csrf_plugin`. Nova automatically injects the `csrf_token` variable into every template — you just need to include it in the form. Without it, the POST request would be rejected with a 403 error.

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

### Template options

The full return tuple is `{ok, Variables, Options}` where `Options` is a map that supports three keys:

| Option | Default | Description |
|--------|---------|-------------|
| `view` | derived from module name | Which template to render |
| `headers` | `#{<<"content-type">> => <<"text/html">>}` | Response headers |
| `status_code` | `200` | HTTP status code |

Some examples:

```erlang
%% Render login.dtl with default 200 status
{ok, [], #{view => login}}.

%% Render with a 422 status (useful for form validation errors)
{ok, [{error, <<"Invalid input">>}], #{view => login, status_code => 422}}.

%% Return plain text instead of HTML
{ok, [{data, Body}], #{headers => #{<<"content-type">> => <<"text/plain">>}}}.
```

```admonish tip
`{view, Variables}` and `{view, Variables, Options}` are aliases for `{ok, ...}` — they behave identically.
```

## Authentication

Now let's protect routes so only logged-in users can access them.

### Security in route groups

Authentication in Nova is configured per route group using the `security` key. It points to a function that receives the request and returns either `{true, AuthData}` (allow) or a denial value (deny). See ["How security works"](#how-security-works) below for all return values.

### Creating a security module

Create `src/blog_auth.erl`:

```erlang
-module(blog_auth).
-export([session_auth/1]).

session_auth(Req) ->
    case nova_session:get(Req, <<"username">>) of
        {ok, Username} ->
            {true, #{username => Username}};
        {error, _} ->
            {redirect, "/login"}
    end.
```

`session_auth/1` checks whether the session contains a username. If so, it returns `{true, AuthData}` — the auth data map is merged into the request and accessible in your controller as `auth_data`. If the session is empty, it redirects to the login page.

```admonish tip
Returning `{redirect, "/login"}` instead of bare `false` gives users a friendly redirect to the login page. A bare `false` would trigger the generic 401 error handler, which is more appropriate for APIs. We covered the 401 handler in the [Error Handling](../testing-errors/error-handling.md) chapter.
```

### Processing the login form

Credential validation belongs in the controller, not the security function. The security function's job is to *gate access* — the login POST route is public by definition (unauthenticated users need to reach it), so it uses `security => false`.

The controller checks the submitted credentials and either creates a session or re-renders the form with an error:

```erlang
login_post(#{params := Params} = Req) ->
    case Params of
        #{<<"username">> := Username,
          <<"password">> := <<"password">>} ->
            nova_session:set(Req, <<"username">>, Username),
            {redirect, "/"};
        _ ->
            {ok, [{error, <<"Invalid username or password">>}], #{view => login}}
    end.
```

On success, we store the username in the session and redirect to the home page. On failure, we re-render the login template with an error message — the user sees the form again instead of a raw error page.

```admonish warning
This is a hardcoded password for demonstration only. In a real application you would validate credentials against a database with properly hashed passwords.
```

### How security works

The security flow for each request is:

1. Nova matches the request to a route group
2. If `security` is `false`, skip to the controller
3. If `security` is a function, call it with the request map
4. If it returns `{true, AuthData}`, merge `auth_data => AuthData` into the request and continue to the controller
5. If it returns `true`, continue to the controller (no auth data attached)
6. If it returns `false`, trigger the 401 error handler
7. If it returns `{redirect, Path}`, send a 302 redirect without calling the controller
8. If it returns `{false, StatusCode, Headers, Body}`, respond with a custom error

The structured `{false, StatusCode, Headers, Body}` form is useful for APIs where you want to return JSON error details instead of triggering the generic 401 handler.

You can have different security functions for different route groups — one for API token auth, another for session auth, and so on.

## Sessions

Nova has a built-in session system backed by ETS (Erlang Term Storage). Session IDs are stored in a `session_id` cookie.

### How sessions work

Nova automatically creates a session for every visitor. On each request, the `nova_stream_h` stream handler checks for a `session_id` cookie:

- **Cookie exists** — the request proceeds normally. The session ID is read from the cookie when you call the session API.
- **No cookie** — Nova generates a new session ID, sets the `session_id` cookie on the response, and stores the ID in the request map.

This means you never need to manually generate session IDs or set the session cookie. By the time your controller runs, every request already has a session — you just read from and write to it.

### The session API

```erlang
nova_session:get(Req, Key)          -> {ok, Value} | {error, not_found}.
nova_session:set(Req, Key, Value)   -> ok | {error, session_id_not_set}.
nova_session:delete(Req)            -> {ok, Req1}.
nova_session:delete(Req, Key)       -> {ok, Req1}.
```

| Function | Description |
|----------|-------------|
| `get/2` | Retrieve a value by key. Returns `{error, not_found}` if the key or session doesn't exist. |
| `set/3` | Store a value in the current session. |
| `delete/1` | Delete the entire session and expire the cookie (sets `max_age => 0`). Returns an updated request — use this `Req1` if you need the cookie change in the response. |
| `delete/2` | Delete a single key from the session. |

The session manager is configured in `sys.config`:

```erlang
{nova, [
    {use_sessions, true},            %% Enable sessions (default: true)
    {session_manager, nova_session_ets}  %% Backend module (default)
]}
```

`nova_session_ets` stores session data in an ETS table and replicates changes across clustered nodes using `nova_pubsub`. Set `use_sessions` to `false` if your application doesn't need sessions (e.g. a pure JSON API).

### Wiring up the login flow

Update the controller to handle login, logout, and the home page:

```erlang
-module(blog_main_controller).
-export([
         index/1,
         login/1,
         login_post/1,
         logout/1
        ]).

index(#{auth_data := #{username := Username}}) ->
    {ok, [{message, <<"Hello ", Username/binary>>}]}.

login(_Req) ->
    {ok, [], #{view => login}}.

login_post(#{params := Params} = Req) ->
    case Params of
        #{<<"username">> := Username,
          <<"password">> := <<"password">>} ->
            nova_session:set(Req, <<"username">>, Username),
            {redirect, "/"};
        _ ->
            {ok, [{error, <<"Invalid username or password">>}], #{view => login}}
    end.

logout(Req) ->
    {ok, Req1} = nova_session:delete(Req),
    {redirect, "/login", Req1}.
```

The login flow:
1. User visits `/login` — sees the login form
2. Form POSTs to `/login` — `login_post/1` checks credentials
3. On success, store the username in the session and redirect to `/`
4. On failure, re-render the form with an error message
5. On `/`, `session_auth/1` verifies the session and populates `auth_data`
6. `/logout` deletes the session, expires the cookie, and redirects to `/login`

Notice that `index/1` only has one clause — it pattern-matches on `auth_data` directly. Since the route group uses `session_auth/1`, unauthenticated users are redirected before the controller runs.

The `logout/1` function passes `Req1` (from `nova_session:delete/1`) as the third element of the redirect tuple. This ensures the expired cookie is included in the response.

```admonish tip
Nova auto-creates the session cookie, so `login_post/1` just calls `nova_session:set/3` — no manual session ID generation or cookie setting needed.
```

### Updating the routes

```erlang
routes(_Environment) ->
  [
    %% Public routes (no auth required)
    #{prefix => "",
      security => false,
      routes => [
                 {"/login", fun blog_main_controller:login/1, #{methods => [get]}},
                 {"/login", fun blog_main_controller:login_post/1, #{methods => [post]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
    },

    %% Protected routes (session auth required)
    #{prefix => "",
      security => fun blog_auth:session_auth/1,
      routes => [
                 {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                 {"/logout", fun blog_main_controller:logout/1, #{methods => [get]}}
                ]
    }
  ].
```

Two route groups instead of three:
1. **Public** — login (GET and POST) and heartbeat. `security => false` means no auth check. Credential validation happens inside `login_post/1`.
2. **Protected** — home page and logout. `session_auth/1` redirects unauthenticated users to `/login`.

Now the flow is:
1. User visits `/login` — sees the login form
2. Form POSTs to `/login` — controller checks credentials
3. On success, a session value is set and the user is redirected to `/`
4. On `/`, `session_auth/1` checks the session
5. `/logout` deletes the session and redirects to `/login`

### Cookie options

Nova sets the `session_id` cookie automatically with default options. For production, you may want to customise the cookie by setting it yourself in a [plugin](plugins.md) or by configuring Cowboy's cookie defaults:

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
