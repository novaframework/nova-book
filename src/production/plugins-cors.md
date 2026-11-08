# Custom Plugins & CORS

[Plugins & Middleware](../foundations/plugins.md) in Part I used the plugins Nova ships. This chapter writes two of our own for production: a request logger that gives every request one structured log line, and a CORS plugin with an origin allowlist for the public JSON API. On the way it pins down the parts of the `nova_plugin` contract that behave differently from what the docs suggest.

## The `nova_plugin` behaviour in Nova 0.18.1

A plugin module implements `plugin_info/0` and any of four optional callbacks:

| Callback | When |
|---|---|
| `pre_request(Req, Env, Options, State)` | Before security and the controller |
| `post_request(Req, Env, Options, State)` | After the controller's reply |
| `init()` | Once, when Nova's plugin manager starts and registers the plugin; returns the initial `State` |
| `stop()` | When the plugin manager terminates |

`Options` is the map from the plugin's entry in the config or route group. `Env` is Cowboy's middleware env; after `nova_router` it holds `app`, `callback` (the controller fun) and the group's settings.

### Return values

What `nova_plugin_handler` does with each return, checked against its source and the blog's tests:

| Return | Effect |
|---|---|
| `{ok, Req, State}` | Continue with the next plugin |
| `{ok, {reply, Status, Headers, Body}, Req, State}` | Set status, headers and body on `Req`, then continue |
| `{stop, Req, State}` | Stop here; the plugin must already have sent a reply |
| `{stop, {reply, Status, Headers, Body}, Req, State}` | Stop, and Nova sends that reply |
| `{break, Req, State}` and `{break, Reply, Req, State}` | The request crashes: the handler returns a value Cowboy's middleware chain does not accept, and the client gets a 500 |
| `{stop, Req}`, `{error, Reason}` | Not accepted: a `case_clause` crash in the request process and an empty 500 |
| an exception | Logged as "Plugin crashed"; the 500 status page is rendered |

`{break, ...}` is in the behaviour's type spec, so it looks usable; the test plugin in `blog_prod_plugins_SUITE` returns it and gets a 500 every time. Use `stop` with a reply instead. `{reply, Body}` and `{reply, Status, Body}` are shorthands for the four-element reply, and `Headers` is a map.

### Where plugins run

- A route group without a `plugins` key uses the global list from `{nova, [{plugins, [...]}]}`. A group **with** `plugins` uses only its own list. Nothing merges the two.
- Plugins run before security, so a pre-request plugin also runs for requests the security function then refuses.
- When security refuses a request, Nova replies and stops: **post-request plugins do not run**. The same happens after a pre-request plugin stops.
- Post-request plugins run after `nova_handler` has sent the controller's reply. They can read `resp_status_code` and `auth_data` from the request map, but they cannot change the response any more.

### Plugin state

Nova keeps one `State` per plugin module, not per request. Each callback's returned state is written back through a `gen_server:call` to `nova_plugin_manager`, so every plugin call on every request goes through that one process, and concurrent requests overwrite each other's state. Keep per-request data in `Req` (as the request logger below does) and treat `State` as read-mostly.

Two smaller mismatches: the behaviour declares `stop/1`, but the manager calls `stop/0` and only if that is exported; and `init/0` only runs for plugin modules that the router has seen in the global list or a route group.

## A request logger

Each request should leave one line in the log with what was asked, how it went and how long it took, and nothing a log reader should not see. `blog_request_logger` stores a start time in the request map before the controller and logs a report after it:

```erlang
-module(blog_request_logger).
-behaviour(nova_plugin).

-include_lib("kernel/include/logger.hrl").

-export([pre_request/4, post_request/4, plugin_info/0]).

pre_request(Req, _Env, _Options, State) ->
    {ok, Req#{request_started_at => erlang:monotonic_time()}, State}.

post_request(#{request_started_at := StartedAt} = Req, _Env, _Options, State) ->
    Elapsed = erlang:monotonic_time() - StartedAt,
    Report = #{
        msg => ~"request",
        method => cowboy_req:method(Req),
        path => cowboy_req:path(Req),
        status => maps:get(resp_status_code, Req, 200),
        duration_us => erlang:convert_time_unit(Elapsed, native, microsecond)
    },
    ?LOG_INFO(with_user(Report, Req)),
    {ok, Req, State};
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
        title => ~"blog_request_logger",
        version => ~"1.0.0",
        url => ~"https://github.com/novaframework/nova-book",
        authors => [~"The Nova Book"],
        description => ~"Logs method, path, status, duration and user id of each request",
        options => []
    }.

with_user(Report, #{auth_data := #{id := UserId}}) ->
    Report#{user_id => UserId};
with_user(Report, _Req) ->
    Report.
```

What it leaves out is deliberate. The path is logged without the query string, because query strings carry tokens and search terms (the confirmation links in Part VIII are an example). Headers, cookies and bodies are never logged. The user is identified by id, which `auth_data` holds once a security function accepted the request.

The plugin goes into the global list, once as `pre_request` and once as `post_request`, around `nova_request_plugin` and after `otel_nova_plugin` from the [previous chapter](opentelemetry.md#the-handler-lists), so its lines carry the trace id:

```erlang
{plugins, [
    {pre_request, otel_nova_plugin, #{}},
    {pre_request, blog_request_logger, #{}},
    {pre_request, nova_request_plugin, #{decode_json_body => true,
                                         read_urlencoded_body => true}},
    {post_request, blog_request_logger, #{}}
]}
```

and, because the global list does not apply to groups with their own plugins, into the public `/api` group explicitly (below). With `nova_jsonlogger` and the OpenTelemetry context from the [previous chapter](opentelemetry.md), a line looks like this:

```json
{"line":20,"pid":"<0.1524.0>","status":200,"time":1794090061721075,"file":".../src/plugins/blog_request_logger.erl","path":"/api/posts","gl":"<0.1373.0>","msg":"request","level":"info","mfa":"blog_request_logger:post_request/4","method":"GET","span_id":"ea0de880798a649c","trace_id":"ecfce5117418849fe60bccea453d8d24","otel_span_id":"ea0de880798a649c","otel_trace_flags":"01","otel_trace_id":"ecfce5117418849fe60bccea453d8d24","duration_us":2864}
```

### Testing a log line

`blog_prod_plugins_SUITE` installs a logger handler that forwards request reports to the test process. A logger handler is a module with `log/2`, so the suite exports one:

```erlang
init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    #{level := Level} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, info),
    ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{pid => self()}}),
    [{primary_level, Level} | Config].

log(#{msg := {report, #{msg := ~"request"} = Report}}, #{config := #{pid := Pid}}) ->
    Pid ! {request_log, Report};
log(_Event, _Config) ->
    ok.
```

(The test config sets the primary level to `warning`, so the suite raises it for each test and restores it afterwards.) The main test sends a bearer token, a cookie and a secret-looking query parameter and checks that none of them reach the report:

```erlang
request_logger_writes_one_report(_Config) ->
    #{id := UserId} = User = blog_data_fixtures:user(~"alice"),
    Token = blog_data_fixtures:token(User),
    Headers = [
        {"authorization", "Bearer " ++ binary_to_list(Token)},
        {"cookie", "session_id=not-for-logs"}
    ],
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        get, {blog_data_fixtures:url("/api/posts?api_key=hunter2"), Headers}, [], []
    ),
    Report = receive_report(),
    #{
        method := ~"GET",
        path := ~"/api/posts",
        status := 200,
        user_id := UserId,
        duration_us := Duration
    } = Report,
    ?assert(is_integer(Duration)),
    Logged = iolist_to_binary(io_lib:format("~p", [Report])),
    [nomatch = binary:match(Logged, Secret) || Secret <- [Token, ~"not-for-logs", ~"hunter2"]].
```

Two more tests pin the rules from above: `/about` (no group plugins) is logged while `/api/tags` (its own plugin list) is not, and a 401 from `/ops/status` is not logged at all, because security refused it before the post-request phase. If you need refused requests in the log, log them where they are refused: in the security function, or with the OpenTelemetry span, which the stream handler ends for every request.

## Rate limiting

Rate limiting is a plugin too. Part III already put one in front of the three routes that check a password: [Rate limiting login](../auth-sessions/authentication.md#rate-limiting-login) wraps `nova_auth_rate_limit` (built on the seki limiter) in `blog_rate_limit`, because the original returns `{stop, Req}`, one of the returns Nova does not accept. Use the same wrapper with another limiter name and limits for anything else you want to throttle, and mind the proxy warning there: behind a load balancer every client has the balancer's address unless you read a trusted forwarding header.

## CORS

A browser only lets a script on `https://app.example` read a response from the blog if the response says so with CORS headers. For anything beyond a plain GET with simple headers (our API clients send `Authorization`), the browser first asks with a **preflight**: an `OPTIONS` request with `Origin`, `Access-Control-Request-Method` and `Access-Control-Request-Headers`.

### What `nova_cors_plugin` does

Part I put Nova's own plugin on `/hello` (see [nova_cors_plugin](../foundations/plugins.md#nova_cors_plugin)). Its behaviour in 0.18.1:

```shell
curl -si -X OPTIONS -H 'origin: https://evil.example' localhost:8080/hello/ada
```

```
HTTP/1.1 200 OK
Access-Control-Allow-Headers: *
Access-Control-Allow-Methods: *
Access-Control-Allow-Origin: *
content-length: 0
...
vary: accept-encoding
...
```

- `allow_origins` is a single value copied into `Access-Control-Allow-Origin` for every request, whatever the `Origin` was. There is no list, so you cannot allow two front ends.
- `Access-Control-Allow-Headers: *` and `Access-Control-Allow-Methods: *` are fixed. By the Fetch standard the `*` does not cover `Authorization`, so a browser will not send a bearer token on the strength of that preflight.
- It sends no `Vary: Origin`, so a cache may serve one origin's answer to another.
- The header names are written in mixed case, unlike everything else Cowboy sends. HTTP does not care, but it shows that the headers bypass `cowboy_req:set_resp_header/3`.

For a public, read-only, credential-free endpoint like the greeting that is acceptable. For the blog's API it is not.

### `blog_cors_plugin`: an allowlist

```erlang
-module(blog_cors_plugin).
-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0]).

pre_request(Req, _Env, Options, State) ->
    Origin = cowboy_req:header(~"origin", Req),
    Allowed = lists:member(Origin, maps:get(allow_origins, Options)),
    Req1 = cowboy_req:set_resp_header(~"vary", ~"origin", Req),
    case {cowboy_req:method(Req), Allowed} of
        {~"OPTIONS", true} ->
            {stop, {reply, 204, preflight_headers(Origin, Options), ~""}, Req1, State};
        {~"OPTIONS", false} ->
            {stop, {reply, 403, #{}, ~""}, Req1, State};
        {_Method, true} ->
            {ok, cowboy_req:set_resp_header(~"access-control-allow-origin", Origin, Req1), State};
        {_Method, false} ->
            {ok, Req1, State}
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

preflight_headers(Origin, Options) ->
    #{
        ~"access-control-allow-origin" => Origin,
        ~"access-control-allow-methods" => maps:get(allow_methods, Options),
        ~"access-control-allow-headers" => maps:get(allow_headers, Options),
        ~"access-control-max-age" => integer_to_binary(maps:get(max_age, Options))
    }.
```

(`plugin_info/0` is left out here; it lists the four options.) The plugin:

- echoes the request's `Origin` only if it is on the list, and sends no CORS header otherwise, so the browser blocks the read;
- answers an allowed preflight itself with 204 and names the method and header the API actually accepts, and refuses any other preflight with 403;
- always sends `Vary: origin`;
- never sends `Access-Control-Allow-Credentials`. The API authenticates with a bearer token that the front end adds explicitly, not with cookies, so no cross-origin request ever carries the blog's session.

It runs first on the public `/api` group (after `otel_nova_plugin`, which the router's `traced/1` puts in front of every group's list, see [Observability](opentelemetry.md#route-groups-with-their-own-plugins)), whose routes accept `options` next to `get` (the router only lets a request reach the plugins if the route accepts its method):

```erlang
#{
    prefix => "/api",
    security => fun blog_security:optional_bearer/1,
    plugins => [
        {pre_request, blog_cors_plugin, api_cors()},
        {pre_request, blog_request_logger, #{}},
        {post_request, blog_request_logger, #{}}
    ],
    routes => [
        {"/posts", fun blog_posts_controller:index/1, #{methods => [get, options]}},
        {"/posts/:id", fun blog_posts_controller:show/1, #{methods => [get, options]}}
    ]
},
```

```erlang
api_cors() ->
    #{
        allow_origins => application:get_env(blog, cors_origins, []),
        allow_methods => ~"GET",
        allow_headers => ~"authorization",
        max_age => 600
    }.
```

The allowlist is the `cors_origins` key in each config: `http://localhost:5173` in dev (a typical front-end dev server), `https://app.example` in test, and `[]` in production until you have a front end on another origin. The router reads it once, when the routes are compiled at boot.

With `https://app.example` in the list, against a release:

```shell
curl -si -X OPTIONS -H 'origin: https://app.example' \
     -H 'access-control-request-method: GET' \
     -H 'access-control-request-headers: authorization' localhost:8080/api/posts
```

```
HTTP/1.1 204 No Content
access-control-allow-headers: authorization
access-control-allow-methods: GET
access-control-allow-origin: https://app.example
access-control-max-age: 600
date: Tue, 03 Nov 2026 09:51:46 GMT
server: Cowboy/Nova
vary: origin, accept-encoding
...
```

The same preflight from `https://evil.example`:

```
HTTP/1.1 403 Forbidden
content-length: 0
...
vary: origin, accept-encoding
...
```

and a plain GET from the allowed origin:

```shell
curl -si -H 'origin: https://app.example' localhost:8080/api/posts
```

```
HTTP/1.1 200 OK
access-control-allow-origin: https://app.example
content-length: 55
content-type: application/json
...
vary: origin, accept-encoding
...

{"page":1,"total_entries":0,"total_pages":0,"posts":[]}
```

`cowboy_compress_h` adds `accept-encoding` to the same `vary` header, which is why the tests match the value's start rather than all of it:

```erlang
cors_preflight_from_an_allowed_origin(_Config) ->
    {204, Headers} = options("/api/posts", ?ORIGIN),
    ?ORIGIN = proplists:get_value("access-control-allow-origin", Headers),
    "GET" = proplists:get_value("access-control-allow-methods", Headers),
    "authorization" = proplists:get_value("access-control-allow-headers", Headers),
    "600" = proplists:get_value("access-control-max-age", Headers),
    ?assertMatch({match, _}, re:run(proplists:get_value("vary", Headers), ~"^origin")).

cors_preflight_from_a_foreign_origin(_Config) ->
    {403, Headers} = options("/api/posts", "https://evil.example"),
    undefined = proplists:get_value("access-control-allow-origin", Headers).
```

```admonish warning title="CORS is not access control"
CORS only tells browsers which scripts may *read* a response. curl, a server or a malicious extension ignores it, and a cross-origin form POST is sent whether or not CORS allows it. Authentication, authorization and CSRF protection still do the real work; see [Security](security.md).
```

The suite also pins `nova_cors_plugin`'s headers with a raw TCP request (httpc lowercases header names, which would hide the casing) and the `{break, ...}` crash, using a test-only plugin on a route added with `nova_router:add_routes/2`:

```shell
rebar3 ct --suite=test/blog_prod_plugins_SUITE
```

```
All 8 tests passed.
```

---

Next: [Security](security.md), which puts the whole stack under an attacker's eye.
