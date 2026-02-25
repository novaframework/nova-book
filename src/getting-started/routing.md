# Routing

In the previous chapter we created a Nova application and saw it running. Now let's understand how requests are matched to controller functions.

## The router module

When Nova generated our project, it created `blog_router.erl`:

```erlang
-module(blog_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

routes(_Environment) ->
    [#{prefix => "",
      security => false,
      routes => [
                 {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
      }].
```

The `routes/1` function returns a list of **route groups**. Each group is a map with these keys:

| Key | Description |
|---|---|
| `prefix` | Path prefix prepended to all routes in this group |
| `security` | `false` or a fun reference to a security handler |
| `routes` | List of route tuples |
| `plugins` | *(optional)* Plugin list — overrides global plugins for this group |

Each route tuple has the form `{Path, Handler, Options}`:
- **Path** — the URL pattern (e.g. `"/users/:id"`)
- **Handler** — a fun reference like `fun Module:Function/1`
- **Options** — a map, typically `#{methods => [get, post, ...]}`

## Adding a route

Let's add a login page route:

```erlang
routes(_Environment) ->
  [#{prefix => "",
      security => false,
      routes => [
                 {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
                 {"/login", fun blog_main_controller:login/1, #{methods => [get]}}
                ]
      }].
```

We will implement the `login/1` function in the [Views, Auth & Sessions](views-auth-sessions.md) chapter.

## Route parameters

Path segments starting with `:` are captured as bindings:

```erlang
{"/users/:id", fun my_controller:show/1, #{methods => [get]}}
```

In the controller, access bindings from the request map:

```erlang
show(#{bindings := #{<<"id">> := Id}}) ->
    {json, #{id => binary_to_integer(Id)}}.
```

Bindings are always binary strings — convert them as needed.

## HTTP methods

The `methods` option takes a list of atoms: `get`, `post`, `put`, `delete`, `patch`, `options`, `head`, `connect`, `trace`.

The default is `['_']`, which matches **all** HTTP methods. Use this for routes where you handle the method inside the controller:

```erlang
{"/login", fun blog_main_controller:login/1, #{methods => ['_']}}
```

A route can handle multiple specific methods:

```erlang
{"/login", fun blog_main_controller:login/1, #{methods => [get, post]}}
```

```erlang
login(#{method := <<"GET">>}) ->
    {ok, [{message, <<"Please log in">>}]};
login(#{method := <<"POST">>}) ->
    %% process login form
    {redirect, "/"}.
```

Note that the `method` field in the request map is an uppercase binary (`<<"GET">>`, `<<"POST">>`, etc.) even though you define routes with lowercase atoms.

## Controller return values

Every controller function receives a request map and returns a tuple. The first element of the tuple tells Nova which handler to use. Here are the return types you'll use most often:

| Return | Description |
|---|---|
| `{json, Data}` | Encode `Data` as JSON. Status is 201 for POST, 200 otherwise. |
| `{ok, Variables}` | Render the default template with `Variables` (list or map). |
| `{view, Variables}` | Same as `{ok, Variables}` — an alias. |
| `{status, Code}` | Return an HTTP status code with no body. |
| `{redirect, Path}` | Send a 302 redirect to `Path`. |

Quick examples:

```erlang
%% Return JSON
index(_Req) ->
    {json, #{message => <<"hello">>}}.

%% Render a template
index(_Req) ->
    {ok, [{title, <<"My Blog">>}]}.

%% Return 204 No Content
delete(_Req) ->
    {status, 204}.

%% Redirect to another page
logout(_Req) ->
    {redirect, "/login"}.
```

Each of these has extended forms for setting custom status codes and headers (e.g. `{json, StatusCode, Headers, Data}`). We'll use those in the [JSON API](../building-api/json-api.md) and [Views, Auth & Sessions](views-auth-sessions.md) chapters.

## Prefixes for grouping

The `prefix` key groups related routes under a common path. For example, to build an API:

```erlang
#{prefix => "/api/v1",
  security => false,
  routes => [
             {"/users", fun blog_api_controller:list_users/1, #{methods => [get]}},
             {"/users/:id", fun blog_api_controller:get_user/1, #{methods => [get]}}
            ]
}
```

These routes become `/api/v1/users` and `/api/v1/users/:id`.

## Security

So far every route group has `security => false`, meaning no authentication check. When `security` is set to a fun reference, Nova calls that function **before** the controller for every route in the group.

The security function receives the request map and must return one of:

| Return | Effect |
|---|---|
| `true` | Allow — request proceeds to the controller. |
| `{true, AuthData}` | Allow — `AuthData` is added to the request map as `auth_data`. |
| `{redirect, Path}` | Deny — redirect the user (e.g. to a login page). |
| `{false, Headers}` | Deny — return 401 with the given headers. |

A basic example:

```erlang
#{prefix => "/admin",
  security => fun blog_auth:check/1,
  routes => [
             {"/dashboard", fun blog_admin_controller:index/1, #{methods => [get]}}
            ]
}
```

```erlang
-module(blog_auth).
-export([check/1]).

check(#{auth_data := _User}) ->
    true;
check(_Req) ->
    {redirect, "/login"}.
```

When `{true, AuthData}` is returned, the controller can access it:

```erlang
index(#{auth_data := User}) ->
    {ok, [{username, maps:get(name, User)}]}.
```

We'll build a full authentication flow in [Views, Auth & Sessions](views-auth-sessions.md).

## Error routes

Nova provides default pages for error status codes (404, 500, etc.). You can override them by adding **error routes** — tuples where the path is an integer status code:

```erlang
routes(_Environment) ->
    [#{prefix => "",
       security => false,
       routes => [
                  {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                  {404, fun blog_error_controller:not_found/1, #{}},
                  {500, fun blog_error_controller:server_error/1, #{}}
                 ]
      }].
```

The error controller works like any other controller:

```erlang
not_found(_Req) ->
    {status, 404, #{}, <<"Page not found">>}.
```

See the [Error Handling](../testing-errors/error-handling.md) chapter for rendering custom error templates.

## Static file serving

Nova can serve static files directly from the router. Use a two-element string tuple `{RemotePath, LocalPath}` (no handler function):

**Serve a directory** — the path must end with `/[...]` to match all files underneath:

```erlang
{"/assets/[...]", "priv/static", #{}}
```

This maps `/assets/css/style.css` to `priv/static/css/style.css`.

**Serve a single file:**

```erlang
{"/favicon.ico", "priv/static/favicon.ico", #{}}
```

Nova resolves `LocalPath` relative to your application's `priv` directory. The third element is an options map (typically empty).

## Inline handlers

For simple responses you can use an anonymous function directly in the route:

```erlang
{"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
```

This is useful for health checks and other trivial endpoints.

## Environment-based routing

The `routes/1` function receives the environment atom configured in `sys.config` (`dev` or `prod`). You can use pattern matching to add development-only routes:

```erlang
routes(prod) ->
  prod_routes();
routes(dev) ->
  prod_routes() ++ dev_routes().

prod_routes() ->
  [#{prefix => "",
      security => false,
      routes => [
                 {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
      }].

dev_routes() ->
  [#{prefix => "",
     security => false,
     routes => [
                {"/dev-tools", fun blog_dev_controller:index/1, #{methods => [get]}}
               ]
  }].
```

```admonish note
`rebar3 nova routes` shows production routes only. Development-only routes won't appear in the output.
```

---

Next, let's look at [plugins](plugins.md) — the middleware layer that processes requests before and after your controllers.
