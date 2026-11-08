# Error Handling

Part I introduced the pieces one at a time: [status-code routes](../foundations/routing.md#error-routes) and the blog's custom 404, [fallback controllers](../foundations/controllers.md#fallback-controllers), [throwing a status](../foundations/controllers.md#throwing-a-status) and [what happens when a controller crashes](../foundations/controllers.md#when-a-controller-crashes). This chapter puts them together for the API. We list every place an error can come from, see which of them we can shape, add a 500 handler, and test each path.

## Where errors come from

Here is every error a client of the blog's API can get, and what produces it. Each row is covered by `blog_api_tags_SUITE`, `blog_api_errors_SUITE`, `blog_auth_api_SUITE`, `blog_auth_policy_SUITE` or `blog_data_safety_SUITE`.

| Situation | Status | Produced by | Body |
|---|---|---|---|
| No route matches the path | 404 | 404 status handler (`blog_error_controller:not_found/1`) | `{"error":"not_found"}` |
| Path exists, method does not | 405 | Nova's router, `allow` header set | empty |
| Body is empty or not valid JSON | 400 | `nova_request_plugin` | empty |
| Write request without a JSON body (tags) | 415 | `blog_require_json_plugin` | `{"error":"expected a JSON body"}` |
| Body does not match the JSON schema (tags) | 400 | `nova_json_schemas` | empty |
| Record does not exist | 404 | the controller or `blog_api_fallback_controller` | `{"error":"not_found"}` |
| Changeset is invalid | 422 | the controller or `blog_api_fallback_controller` | `{"errors":{...}}` |
| Missing, invalid or expired bearer token | 401 | `blog_security:require_bearer/1` | `{"error":"missing bearer token"}` or `{"error":"invalid or expired token"}` |
| Security function returns plain `false` | 401 | Nova's security handler | empty |
| Wrong email or password for a token | 401 | `blog_tokens_controller:create/1` | `{"error":"invalid email or password"}` |
| Too many token requests from one address | 429 | `blog_rate_limit` (`nova_auth_rate_limit`) | `{"error":"too many requests"}` |
| Changing someone else's post | 403 | `blog_posts_controller` (`authorize/3`) | `{"error":"you can only change your own posts"}` |
| Updating a post that changed since it was read | 409 | `blog_posts_controller:update/1` | `{"error":"post was changed by someone else"}` |
| Controller or plugin crashes | 500 | 500 status handler (`blog_error_controller:server_error/1`) | `{"error":"internal_error"}` |
| Security function crashes | 500 | Nova's security handler | empty |
| Controller returns something unexpected | 500 | `blog_api_fallback_controller` | `{"error":"internal_error"}` |

The empty bodies are produced inside Nova or its plugins and cannot be changed from the application. For an API that is acceptable: the status code says what happened. Everything the blog's own code decides gets a JSON body with either an `error` code or per-field `errors`.

## Which errors reach a status handler

A status-code route (`{404, fun blog_error_controller:not_found/1, #{}}`) is used when **Nova itself** decides on a status:

- no route matches (404) or the method is wrong (405),
- a controller or plugin crashes (500),
- a controller throws `{Status, Reason}`,
- a controller returns something no handler understands and there is no fallback (500).

It is **not** used when a controller returns a status:

```erlang
bare_status(_Req) ->
    {status, 404}.
```

This answers 404 with an empty body; the blog's JSON 404 handler never runs (test `bare_status_return_skips_status_handler`). The same goes for the security layer: a security function that returns `false` gives a bare 401, and one that crashes a bare 500 (test `security_crash_skips_500_handler`), with or without a status route for that code. To send a body from a security function, return `{false, Status, Headers, Body}` (see [Routing](../foundations/routing.md#security)). So in a controller, return the full response: `{json, 404, #{}, #{error => ~"not_found"}}`, or `{error, not_found}` and let the fallback controller build it. If a helper deep in the call stack needs to give up, `throw({404, Reason})` does go through the status handler.

## A 500 handler

Nova registers its own handlers for 404 and 500. Part I replaced the 404 one because of its JSON bug. The 500 one is no better. With the dev config (`use_stacktrace` on), a crashing controller gives a JSON client this:

```shell
curl -i localhost:8080/api/crash -H 'accept: application/json'
```

```
HTTP/1.1 500 Internal Server Error
content-length: 104
content-type: application/json
...

{"message":"undefined","status":"undefined","title":"undefined","stacktrace":[],"extra_msg":"undefined"}
```

(`/api/crash` was a throwaway route to `blog_crash_demo_controller`, whose `list(#{req := _Req})` matches a key the request map does not have, so every call crashes with `function_clause`.) A browser gets Nova's error page with an empty heading, and any other `accept` value, curl's default `*/*` included, an empty body.

```admonish bug
In Nova 0.18.1, `nova_error_controller:server_error/1` formats a stacktrace that `nova_handler` has already formatted, fails on every frame (logging "Could not format stacktrace line" each time) and ends up with an empty list. The crash payload also has no title or message, so every field renders as `"undefined"`. It only answers JSON when `accept` is exactly `application/json`.
```

The blog registers its own. In the last route group of `src/blog_router.erl`, next to the 404 handler:

```erlang
{404, fun blog_error_controller:not_found/1, #{}},
{500, fun blog_error_controller:server_error/1, #{}}
```

`src/controllers/blog_error_controller.erl` now has both handlers, sharing the `accept` check:

```erlang
-module(blog_error_controller).

-export([not_found/1, server_error/1]).

not_found(Req) ->
    case wants_html(cowboy_req:header(~"accept", Req, ~"")) of
        true ->
            {ok, [{path, cowboy_req:path(Req)}], #{view => blog_not_found, status_code => 404}};
        false ->
            {status, 404, #{}, #{error => ~"not_found"}}
    end.

server_error(Req) ->
    case wants_html(cowboy_req:header(~"accept", Req, ~"")) of
        true -> {ok, [], #{view => blog_server_error, status_code => 500}};
        false -> {status, 500, #{}, #{error => ~"internal_error"}}
    end.

wants_html(Accept) ->
    binary:match(Accept, ~"text/html") =/= nomatch.
```

Browsers send `text/html` in `accept` and get a page; API clients, curl and anything else get JSON. `src/views/blog_server_error.dtl` is a plain page:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Something went wrong</title>
</head>
<body>
  <h1>Something went wrong</h1>
  <p>The error has been logged. Please try again in a moment.</p>
  <p><a href="/">Back to the front page</a></p>
</body>
</html>
```

The same crash now gives:

```
HTTP/1.1 500 Internal Server Error
content-length: 26
content-type: application/json
...

{"error":"internal_error"}
```

The response says nothing about the cause, on purpose. The details belong in the log, which Nova has already written by the time the handler runs. If a handler needs them, the request map it receives has a `crash_info` key with `class`, `reason` and, when `use_stacktrace` is on, `stacktrace`.

## Crashes and `use_stacktrace`

When a controller crashes, Nova logs a "Controller crashed" error with the class and reason. With `{use_stacktrace, true}` in the `nova` config (the blog's dev config) the log entry also has the stacktrace:

```
[error] ... class=error stacktrace="[#{arity => [#{pid => <0.1272.0>, ... path => <<\"/api/crash\">>, ... headers => #{<<\"accept\">> => <<\"application/json\">>, ...}, ...}], function => list,line => 5,module => blog_crash_demo_controller,...}, ...]" msg="Controller crashed" reason=function_clause  (nova_handler:execute/2)
```

```admonish warning
For a `function_clause` the first stack frame holds the arguments of the call that did not match, and for a controller that is the whole request map: headers, cookies and an `authorization` header if there is one. That is useful while developing and a liability in production logs. The blog's `prod_sys.config.src` sets `{use_stacktrace, false}`; keep it that way.
```

The client never sees the stacktrace from the blog's handler, whatever the setting.

## One error format for the API

The tags controller only describes the happy path and returns `{error, Reason}` for everything else. `blog_api_fallback_controller` (from [the JSON API chapter](json-api.md#the-controller)) turns those into responses:

```erlang
resolve(_Req, {error, not_found}) ->
    {json, 404, #{}, #{error => ~"not_found"}};
resolve(_Req, {error, #kura_changeset{} = CS}) ->
    {json, 422, #{}, #{errors => blog_api_json:errors(CS)}};
resolve(_Req, Unexpected) ->
    ?LOG_ERROR(#{msg => ~"unexpected controller return", return => Unexpected}),
    {json, 500, #{}, #{error => ~"internal_error"}}.
```

That gives the API two body shapes, and clients can rely on them:

- `{"error": Code}` for errors about the request as a whole (`not_found`, `internal_error`, `expected a JSON body`),
- `{"errors": {Field: [Message, ...]}}` for validation errors, built by `blog_api_json:errors/1` from [Changesets](../data-layer/changesets.md#rendering-errors-in-json-responses).

The posts controller from Part II produces the same shapes with explicit `case` expressions; both styles work. The catch-all clause matters: without it, an unexpected `{error, timeout}` from deep in the code would crash the fallback and end up as Nova's generic 500. Remember that only tuples reach a fallback ([Controllers](../foundations/controllers.md#fallback-controllers)).

## Errors from plugins

Plugins run before the controller, so their errors bypass the fallback controller:

- **`nova_request_plugin`** answers an empty or unparseable JSON body with a bare `400` and logs a warning. It replies directly, so no status handler runs.
- **`nova_json_schemas`** answers a schema violation with `400`, empty with `render_errors => false` (see [Rendering schema errors](json-api.md#rendering-schema-errors) for why the blog does not render them).
- **`blog_require_json_plugin`** answers `415` with a JSON body. It replies by returning `{stop, {reply, Status, Headers, Body}, Req, State}`, which is the way for a plugin to send an error response.

```admonish warning
`nova_plugin`'s documentation lists `{error, Reason}` as a valid plugin return that renders a 500 page. Nova 0.18.1 does not handle it: the request process crashes and the client gets an empty 500 that no status handler sees. Return `{stop, Reply, Req, State}` instead. A plugin that *raises* an exception is handled properly and rendered by the 500 handler. The test `unguarded_schema_route_crashes_without_json` shows the first case with `nova_json_schemas`.
```

## Turning error pages off

Setting `render_error_pages` to `false` in the `nova` config makes Nova skip status handlers for crashes and fallback errors and send the bare status:

```erlang
{nova, [
    {render_error_pages, false},
    ...
]}
```

The blog keeps the default (`true`), because its handlers are the ones that produce the JSON bodies. The test `render_error_pages_false_gives_bare_status` sets it at runtime to check the behaviour.

## Testing error paths

Error paths deserve tests as much as the happy path, and most need nothing special: `blog_api_tags_SUITE` sends bad requests and checks the status and body, as in [the previous chapter](json-api.md#generating-a-test-suite).

For behaviour no real route shows, such as a crash, `blog_api_errors_SUITE` mounts test-only routes, a crashing plugin (`test/blog_api_errors_test_plugin.erl`) and a crashing security function when the suite starts, with `nova_router:add_routes/2`:

```erlang
init_per_suite(Config) ->
    blog_data_fixtures:start(),
    ok = nova_router:add_routes(blog, #{
        prefix => "/errors",
        security => false,
        plugins => [
            {pre_request, nova_request_plugin, #{decode_json_body => true}},
            {pre_request, nova_json_schemas, #{render_errors => false}}
        ],
        routes => [
            {"/crash/:id", fun blog_api_errors_test_controller:crash/1, #{methods => [get]}},
            {"/bare", fun blog_api_errors_test_controller:bare_status/1, #{methods => [get]}},
            {"/unexpected", fun blog_api_errors_test_controller:unexpected/1, #{methods => [get]}},
            {"/echo", fun blog_api_errors_test_controller:echo/1, #{
                methods => [post], extra_state => #{json_schema => "schemas/tag.json"}
            }}
        ]
    }),
    ok = nova_router:add_routes(blog, [
        #{
            prefix => "/errors/plugin",
            security => false,
            plugins => [{pre_request, blog_api_errors_test_plugin, #{}}],
            routes => [
                {"/", fun blog_api_errors_test_controller:bare_status/1, #{methods => [get]}}
            ]
        },
        #{
            prefix => "/errors/security",
            security => fun blog_api_errors_test_controller:crashing_security/1,
            routes => [
                {"/", fun blog_api_errors_test_controller:bare_status/1, #{methods => [get]}}
            ]
        }
    ]),
    Config.
```

The controller lives in `test/`, so it never ships:

```erlang
-module(blog_api_errors_test_controller).
-fallback_controller(blog_api_fallback_controller).

-export([crash/1, bare_status/1, unexpected/1, echo/1]).

crash(#{bindings := #{~"id" := Id}}) ->
    {json, #{id => binary_to_integer(Id)}}.

bare_status(_Req) ->
    {status, 404}.

unexpected(_Req) ->
    {error, teapot}.

echo(#{json := Json}) ->
    {json, Json}.
```

`GET /errors/crash/abc` makes `binary_to_integer/1` raise `badarg`, and the tests check both faces of the 500 handler:

```erlang
crash_is_json_500(_Config) ->
    {500, Headers, Body} = get("/errors/crash/abc", "application/json"),
    "application/json" = proplists:get_value("content-type", Headers),
    #{~"error" := ~"internal_error"} = json:decode(Body).

crash_is_html_500_for_browsers(_Config) ->
    {500, Headers, Body} = get("/errors/crash/abc", "text/html"),
    "text/html" ++ _ = proplists:get_value("content-type", Headers),
    {_, _} = binary:match(Body, ~"Something went wrong").
```

```shell
rebar3 ct --suite=test/blog_api_errors_SUITE
```

```
%%% blog_api_errors_SUITE: .........
All 9 tests passed.
```

Crashing tests log "Controller crashed" errors. That is expected; the test config only prints warnings and above, so you will see them in the CT logs.

---

With error handling in place, our application is more robust. Next, let's explore real-time web interfaces with [Arizona Fundamentals](../arizona/fundamentals.md).
