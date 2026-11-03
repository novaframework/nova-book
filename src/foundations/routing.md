# Routing

In the previous chapter we created a Nova application and saw it running. Now let's see how a request finds its way to a controller function, and give the blog its first real pages.

## The router module

The generated `blog_router.erl` looks like this:

```erlang
-module(blog_router).
-behaviour(nova_router).

-export([
    routes/1
]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [
        #{
            prefix => "",
            security => false,
            routes => [
                {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
            ]
        }
    ].
```

Nova finds the router by naming convention: the router of application `blog` is `blog_router` (set `{router_module, Mod}` in the application's environment to override it). At startup Nova calls `routes/1` once with the configured environment and compiles the result into a routing trie.

`routes/1` returns a list of **route groups**. Each group is a map:

| Key | Default | Description |
|---|---|---|
| `routes` | `[]` | List of route entries (below) |
| `prefix` | `""` | Path prepended to every route in the group |
| `security` | `false` | `false`, or a `fun Mod:Fun/1` called before every controller in the group |
| `plugins` | global plugins | Plugin list for this group, replacing the global one (see [Plugins](plugins.md#per-route-plugins)) |
| `host` | `'_'` | Only match requests for this host |

A route entry is one of:

| Entry | Meaning |
|---|---|
| `{Path, fun Mod:Fun/1, Options}` | HTTP route handled by a controller function |
| `{Path, fun Mod:Fun/1}` | Same, with default options |
| `{Path, Module, #{protocol => ws}}` | WebSocket route (see [WebSockets](../real-time/websockets.md)) |
| `{StatusCode, fun Mod:Fun/1, Options}` | Handler for an error status such as 404 |
| `{Path, LocalPath}`, `{Path, LocalPath, Options}` | Static file or directory |

Options for HTTP routes are `methods` (default `['_']`, any method) and `extra_state`, a term handed to the controller in the request map. The old `{Path, {Module, Function}, Options}` form still works but logs a deprecation warning.

## Adding pages

Let's give the blog an about page. Add a new group at the end of the list, after the scaffold's group:

```erlang
#{
    prefix => "",
    security => false,
    routes => [
        {"/about", fun blog_pages_controller:about/1, #{methods => [get]}},
        {"/about-us", fun blog_pages_controller:about_moved/1, #{methods => [get]}},
        {"/about.md", fun blog_pages_controller:about_markdown/1, #{methods => [get]}},
        {"/version", fun blog_pages_controller:version/1, #{methods => [get]}}
    ]
},
```

The controller functions are the subject of the [next chapter](controllers.md). For now it is enough to know that `about/1` renders a template and `about_moved/1` redirects old links:

```shell
curl -i localhost:8080/about-us
```

```
HTTP/1.1 302 Found
content-length: 0
date: Tue, 03 Nov 2026 07:25:21 GMT
location: /about
server: Cowboy/Nova
vary: accept-encoding
set-cookie: session_id=AkwGvd7r7RRapL9jzjI9h5o4Elo35MzRG78Ie5hrYcg=
```

Empty path segments are ignored when matching, so `/about/` and `/about` are the same route.

## Route parameters

A path segment starting with `:` is a **binding**. The blog greets visitors by name:

```erlang
#{
    prefix => "/hello",
    security => false,
    plugins => [...],
    routes => [
        {"/", fun blog_hello_controller:create/1, #{methods => [post]}},
        {"/:name", fun blog_hello_controller:greet/1, #{methods => [get, options]}}
    ]
},
```

The controller reads the binding from the `bindings` map of the request. Keys are **binaries**, not atoms:

```erlang
greet(#{bindings := #{~"name" := Name}, parsed_qs := Query}) ->
    maybe
        ok ?= validate_name(Name),
        ?LOG_INFO(#{msg => ~"greeting", name => Name}),
        {json, #{greeting => greeting(Name, Query)}}
    end.
```

```shell
curl localhost:8080/hello/nova
```

```
{"greeting":"Hello, nova!"}
```

Binding values are always binaries, and they are the **raw path segment**: `/hello/J%C3%B6rgen` binds `~"J%C3%B6rgen"`, not `~"Jörgen"`. Decode with `uri_string:percent_decode/1` if you need the characters, and always validate before use. Here `validate_name/1` only accepts short ASCII names, so that request ends in a 422 (we will see how in the [next chapter](controllers.md#fallback-controllers)).

A `[...]` segment at the end of a path is a catch-all that matches zero or more remaining segments. We will use it for static files below.

### Which route wins

When several routes could match, Nova decides segment by segment: a literal segment beats a binding, and a binding beats `[...]`. Matching backtracks, so you can declare both `/posts/new` and `/posts/:id`: `/posts/new` is served by the first and everything else by the second. Two routes with the same path *and* method are a conflict; the first one wins and the second is logged and ignored. Set `{use_strict_routing, true}` in the `nova` config to refuse to start on a conflict instead.

## HTTP methods

`methods` takes a list of lowercase atoms: `get`, `post`, `put`, `patch`, `delete`, `head`, `options`, `connect`, `trace`, or `'_'` for any method (the default). The same path can be split across several entries by method, as the contact form in the [Plugins](plugins.md#csrf-protection) chapter does:

```erlang
{"/", fun blog_contact_controller:show/1, #{methods => [get]}},
{"/", fun blog_contact_controller:submit/1, #{methods => [post]}}
```

If the path exists but the method does not, Nova answers `405 Method Not Allowed` with an `allow` header, without calling any plugin or controller:

```shell
curl -i -X DELETE localhost:8080/hello/nova
```

```
HTTP/1.1 405 Method Not Allowed
allow: GET, OPTIONS
content-length: 0
date: Tue, 03 Nov 2026 07:25:21 GMT
server: Cowboy/Nova
vary: accept-encoding
set-cookie: session_id=VIYrCuaBJBY8Mf0KitMBLvkynff/TPLRmfZjU/34bRU=
```

Inside the controller the method is an uppercase binary (`~"GET"`, `~"POST"`) under the `method` key, even though routes use lowercase atoms.

## Prefixes

`prefix` mounts every route of a group under a path. In the `/hello` group above, `"/"` becomes `/hello` and `"/:name"` becomes `/hello/:name`. Prefixes are how you group an API (`"/api/v1"`), an admin area or anything else that shares plugins or security.

## Security

`security` is the group's gatekeeper. When it is a `fun Mod:Fun/1`, Nova calls it with the request map after the pre-request plugins and before the controller, for every route in the group.

The blog has a small operations endpoint that reports the node's status. It must not be public, so its group has a security function:

```erlang
#{
    prefix => "/ops",
    security => fun blog_ops_security:require_token/1,
    routes => [
        {"/status", fun blog_ops_controller:status/1, #{methods => [get]}}
    ]
},
```

```erlang
-module(blog_ops_security).

-export([require_token/1]).

require_token(Req) ->
    Expected = application:get_env(blog, ops_token, undefined),
    case matches(cowboy_req:header(~"x-ops-token", Req), Expected) of
        true ->
            {true, #{role => ops}};
        false ->
            {false, 401, #{~"www-authenticate" => ~"X-Ops-Token"}, ~"unauthorized\n"}
    end.

matches(Given, Expected) when is_binary(Given), is_binary(Expected), Expected =/= ~"" ->
    crypto:hash_equals(crypto:hash(sha256, Given), crypto:hash(sha256, Expected));
matches(_Given, _Expected) ->
    false.
```

The token comes from configuration, never from code (`{blog, [{ops_token, ~"dev-ops-token"}]}` in `dev_sys.config.src`). If it is not configured, or configured empty (an unset environment variable in production, see [Configuration](../production/configuration.md)), every request is refused, and the comparison uses `crypto:hash_equals/2` so its timing does not leak how much of a guess was right.

A security function can return:

| Return | Effect |
|---|---|
| `true` | Continue to the controller |
| `{true, AuthData}` | Continue; the controller finds `AuthData` under `auth_data` in the request |
| `false` | Stop with `401` |
| `{false, Headers}` | Stop with `401` and extra headers |
| `{false, Status, Headers}` | Stop with `Status` and extra headers |
| `{false, Status, Headers, Body}` | Stop with `Status`, headers and a body |
| `{redirect, Path}` | Stop with a `302` to `Path`; `Path` must be a string (`"/login"`), not a binary |

Anything else is treated as `false`. The controller pattern matches on what the security function returned:

```erlang
status(#{auth_data := #{role := Role}}) ->
    {json, #{status => ~"ok", role => Role, node => node()}}.
```

```shell
curl -i localhost:8080/ops/status
```

```
HTTP/1.1 401 Unauthorized
content-length: 13
date: Tue, 03 Nov 2026 07:25:21 GMT
server: Cowboy/Nova
vary: accept-encoding
www-authenticate: X-Ops-Token
set-cookie: session_id=jNCn7SsHTqnePMaquneQGItc7E9LC+qCHjCHcAtYDqY=

unauthorized
```

```shell
curl -H 'x-ops-token: dev-ops-token' localhost:8080/ops/status
```

```
{"node":"nonode@nohost","status":"ok","role":"ops"}
```

A header token is fine for an internal endpoint. For people logging in through a browser we build sessions and real authentication in [Part III](../auth-sessions/sessions.md).

## Static files

A route whose second element is a string is served from disk. The path is relative to the application's `priv` directory (or the current directory, if it exists there). The blog's last group serves its static assets:

```erlang
#{
    prefix => "",
    security => false,
    routes => [
        {"/robots.txt", "assets/robots.txt", #{mimetype => ~"text/plain"}},
        {"/assets/[...]", "assets"},
        {404, fun blog_error_controller:not_found/1, #{}}
    ]
}
```

- `{"/robots.txt", "assets/robots.txt", Options}` serves one file.
- `{"/assets/[...]", "assets"}` serves a directory: `/assets/favicon.ico` is `priv/assets/favicon.ico`.

The options map accepts `mimetype` (force a content type), `index_files` (default `["index.html"]`, served when a directory is requested) and `list_dir` (`true` to list directory contents; off by default, leave it that way). Byte-range requests are supported.

```admonish note
Without `mimetype`, Nova guesses the type from the extension using Cowlib's short list of web types (HTML, CSS, JS, images, fonts, audio, video). Anything else, `.txt` and `.json` included, is served as `application/octet-stream`, which is why `robots.txt` sets it explicitly.
```

```shell
curl -i localhost:8080/robots.txt
```

```
HTTP/1.1 200 OK
content-length: 30
content-type: text/plain
date: Tue, 03 Nov 2026 07:25:21 GMT
server: Cowboy/Nova
vary: accept-encoding
set-cookie: session_id=kE9d9qY7oT6qKGB4ca+6cIy6czbo95ASwZEElrhv56s=

User-agent: *
Disallow: /ops/
```

Static routes accept any HTTP method and use the group's `security`, so keep them in a group with `security => false` and do not put private files under a served directory.

## Error routes

An integer in the path position registers a handler for that status code. Nova uses it whenever it produces that status itself: a path that matches no route, a controller that crashes (`500`) and so on. A controller that returns `{status, Code}` does not reach it; that reply goes out as written, with an empty body (see [Error Handling](../building-api/error-handling.md#which-errors-reach-a-status-handler)).

Nova ships its own 404 handler, but it is worth replacing. In 0.18.1 its JSON variant encodes its message as a list of character codes (`{"message":[82,101,115,...]}`). The blog's handler answers in HTML or JSON depending on the `accept` header:

```erlang
-module(blog_error_controller).

-export([not_found/1]).

not_found(Req) ->
    case wants_html(cowboy_req:header(~"accept", Req, ~"")) of
        true ->
            {ok, [{path, cowboy_req:path(Req)}], #{view => blog_not_found, status_code => 404}};
        false ->
            {status, 404, #{}, #{error => ~"not_found"}}
    end.

wants_html(Accept) ->
    binary:match(Accept, ~"text/html") =/= nomatch.
```

An error handler must set the status itself (`status_code => 404`, `{status, 404, ...}`); returning a plain `{json, Map}` from it would answer `200`.

```shell
curl -i localhost:8080/no/such/page
```

```
HTTP/1.1 404 Not Found
content-length: 21
content-type: application/json
date: Tue, 03 Nov 2026 07:25:21 GMT
server: Cowboy/Nova
vary: accept-encoding
set-cookie: session_id=RTI/3dQmODEmm7NBmoT8k2wmbLtE3HaZhc7UeChn2Xc=

{"error":"not_found"}
```

A browser sends `accept: text/html` and gets the `blog_not_found.dtl` page instead. The [Error Handling](../building-api/error-handling.md) chapter goes further.

## Inline handlers

The scaffold's heartbeat shows that a handler can be an anonymous function written in the router:

```erlang
{"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
```

That is handy for a trivial endpoint like this one. Anything with logic belongs in a controller module, where it can be tested and where `rebar3 nova routes` can show a readable name.

## Environment-based routing

`routes/1` receives the `environment` value from the `nova` config (`dev` in `dev_sys.config.src`, `test` in the companion app's test config, `prod` in releases). Pattern matching on it gives you development-only routes:

```erlang
routes(dev) ->
    routes(prod) ++ dev_routes();
routes(_Environment) ->
    [
        #{
            prefix => "",
            security => false,
            routes => [
                {"/", fun blog_main_controller:index/1, #{methods => [get]}}
            ]
        }
    ].

dev_routes() ->
    [
        #{
            prefix => "/dev",
            security => false,
            routes => [
                {"/routes", fun blog_dev_controller:routes/1, #{methods => [get]}}
            ]
        }
    ].
```

The blog does not need any, so its router ignores the argument.

## Routes defined by controllers

A router can also export `controllers/1` and let each controller declare its own routes through the `nova_controller` behaviour. An entry may carry options, which lets one module be mounted twice:

```erlang
controllers(_Environment) ->
    [blog_pages_controller, {blog_feed_controller, #{prefix => "/feeds"}}].
```

```erlang
-module(blog_feed_controller).
-behaviour(nova_controller).

-export([routes/2, index/1]).

routes(_Environment, #{prefix := Prefix}) ->
    [
        #{
            prefix => Prefix,
            security => false,
            routes => [{"/", fun ?MODULE:index/1, #{methods => [get]}}]
        }
    ].

index(_Req) ->
    {json, #{items => []}}.
```

Routes from `routes/1` and from `controllers/1` are combined. This book keeps everything in `blog_router.erl` so the whole table is in one place.

Routes can also be added while the node runs with `nova_router:add_routes(blog, RouteGroup)`. The companion app's test suite uses that to mount a few test-only controllers.

## Listing the routes

With all the groups from Part I in place:

```shell
rebar3 nova routes
```

```
Host: '_'
     ├─  _ /404 (blog, blog_error_controller:not_found/1)
     ├─  /assets
         └─  _ /[...] (blog, nova_file_controller:get_dir/1)
     ├─  _ /robots.txt (blog, nova_file_controller:get_file/1)
     ├─  /ops
         └─  GET /status (blog, blog_ops_controller:status/1)
     ├─  POST /contact (blog, blog_contact_controller:submit/1)
     ├─  GET /contact (blog, blog_contact_controller:show/1)
     ├─  POST /hello (blog, blog_hello_controller:create/1)
         └─  OPTIONS /name (blog, blog_hello_controller:greet/1)
         └─  GET /name (blog, blog_hello_controller:greet/1)
     ├─  GET /version (blog, blog_pages_controller:version/1)
     ├─  GET /about.md (blog, blog_pages_controller:about_markdown/1)
     ├─  GET /about-us (blog, blog_pages_controller:about_moved/1)
     ├─  GET /about (blog, blog_pages_controller:about/1)
     ├─  GET /heartbeat (blog, blog_router:-routes/1-fun-0-/1)
     └─  GET / (blog, blog_main_controller:index/1)
```

The listing prints bindings without their colon (`/name` is `/hello/:name`). `rebar3 nova audit` reviews the same table from a security angle and flags write methods without a security function:

```shell
rebar3 nova audit
```

```
=== Security Audit ===

  WARNINGS:
    Wildcard method on /robots.txt (nova_file_controller) - all HTTP methods accepted
    PATCH /robots.txt (nova_file_controller) has no security
    ...
    POST /hello (blog_hello_controller) has no security
    POST /contact (blog_contact_controller) has no security

  INFO:
    GET / (blog_main_controller) has no security
    ...
    GET /contact (blog_contact_controller) has no security

  Summary: 11 warning(s), 9 info(s)
```

Both `POST`s are public on purpose, and the contact form is protected by CSRF tokens instead (see [Plugins](plugins.md#csrf-protection)). Treat the audit as a checklist, not a verdict.

---

Next, let's look at [controllers](controllers.md), the functions that handle requests and decide what to send back.
