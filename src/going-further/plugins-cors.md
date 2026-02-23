# Custom Plugins and CORS

In the [Plugins](../getting-started/plugins.md) chapter we saw how Nova's built-in plugins work. Now let's build custom plugins and set up CORS for our blog API.

## The nova_plugin behaviour

Every callback in `nova_plugin` is optional — implement only what you need. A plugin registered as `pre_request` must export `pre_request/4`; one registered as `post_request` must export `post_request/4`.

### Request callbacks

```erlang
-callback pre_request(Req, Env, Options, State) ->
    {ok, Req, State} |      %% Continue to the next plugin
    {break, Req, State} |   %% Skip remaining plugins, go to controller
    {stop, Req, State} |    %% Stop entirely, plugin handles the response
    {error, Reason}.        %% Trigger a 500 error

-callback post_request(Req, Env, Options, State) ->
    {ok, Req, State} |
    {break, Req, State} |
    {stop, Req, State} |
    {error, Reason}.

-callback plugin_info() ->
    #{title := binary(), version := binary(), url := binary(),
      authors := [binary()], description := binary(),
      options => [{atom(), binary()}]}.
```

### Lifecycle callbacks: init/0 and stop/1

Two optional callbacks manage **global, long-lived state** that persists across requests:

```erlang
-callback init() -> State :: any().
-callback stop(State :: any()) -> ok.
```

`init/0` is called once when the plugin is loaded. The state it returns is passed as the `State` argument to every `pre_request/4` and `post_request/4` call. `stop/1` is called when the application shuts down and receives the current state for cleanup.

This is useful when a plugin needs a long-lived resource — an ETS table, a connection pool reference, or a background process:

```erlang
-module(blog_stats_plugin).
-behaviour(nova_plugin).

-export([init/0,
         stop/1,
         pre_request/4,
         post_request/4,
         plugin_info/0]).

init() ->
    Tab = ets:new(request_stats, [public, set]),
    ets:insert(Tab, {total_requests, 0}),
    #{table => Tab}.

stop(#{table := Tab}) ->
    ets:delete(Tab),
    ok.

pre_request(Req, _Env, _Options, #{table := Tab} = State) ->
    ets:update_counter(Tab, total_requests, 1),
    {ok, Req, State}.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{title => <<"blog_stats_plugin">>,
      version => <<"1.0.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Blog">>],
      description => <<"Tracks total request count in ETS">>}.
```

Without `init/0`, the plugin state starts as `undefined`. Without `stop/1`, no cleanup runs on shutdown.

## Example: Request logger

A plugin that logs every request with method, path, and response time.

Create `src/plugins/blog_logger_plugin.erl`:

```erlang
-module(blog_logger_plugin).
-behaviour(nova_plugin).

-include_lib("kernel/include/logger.hrl").

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

pre_request(Req, _Env, _Options, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    {ok, Req#{start_time => StartTime}, State}.

post_request(Req, _Env, _Options, State) ->
    StartTime = maps:get(start_time, Req, 0),
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    ?LOG_INFO("~s ~s completed in ~pms", [Method, Path, Duration]),
    {ok, Req, State}.

plugin_info() ->
    {<<"blog_logger_plugin">>,
     <<"1.0.0">>,
     <<"Blog">>,
     <<"Logs request method, path and duration">>,
     []}.
```

Register it as both pre-request and post-request in `sys.config`:

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{decode_json_body => true,
                                          read_urlencoded_body => true}},
    {pre_request, blog_logger_plugin, #{}},
    {post_request, blog_logger_plugin, #{}}
]}
```

Output:

```
[info] GET /api/posts completed in 3ms
[info] POST /api/posts completed in 12ms
```

## Example: Rate limiter

A plugin that limits requests per IP address using ETS:

```erlang
-module(blog_rate_limit_plugin).
-behaviour(nova_plugin).

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

pre_request(Req, _Env, Options, State) ->
    MaxRequests = maps:get(max_requests, Options, 100),
    WindowMs = maps:get(window_ms, Options, 60000),
    {IP, _Port} = cowboy_req:peer(Req),
    Key = {rate_limit, IP},
    Now = erlang:monotonic_time(millisecond),
    case ets:lookup(blog_rate_limits, Key) of
        [{Key, Count, WindowStart}] when Now - WindowStart < WindowMs ->
            if Count >= MaxRequests ->
                    Reply = cowboy_req:reply(429,
                        #{<<"content-type">> => <<"application/json">>},
                        <<"{\"error\":\"too many requests\"}">>,
                        Req),
                    {stop, Reply, State};
               true ->
                    ets:update_element(blog_rate_limits, Key, {2, Count + 1}),
                    {ok, Req, State}
            end;
        _ ->
            ets:insert(blog_rate_limits, {Key, 1, Now}),
            {ok, Req, State}
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    {<<"blog_rate_limit_plugin">>,
     <<"1.0.0">>,
     <<"Blog">>,
     <<"Simple IP-based rate limiting">>,
     [max_requests, window_ms]}.
```

Create the ETS table on application start in `src/blog_app.erl`:

```erlang
start(_StartType, _StartArgs) ->
    ets:new(blog_rate_limits, [named_table, public, set]),
    blog_sup:start_link().
```

When the limit is exceeded, the plugin returns `{stop, Reply, State}` — a 429 response is sent and the controller is never called.

## CORS

If your API is consumed by a frontend on a different domain, the browser blocks requests unless your server sends the right CORS (Cross-Origin Resource Sharing) headers. Nova includes a CORS plugin.

### Using nova_cors_plugin

Add it to your plugin configuration:

```erlang
{plugins, [
    {pre_request, nova_cors_plugin, #{allow_origins => <<"*">>}},
    {pre_request, nova_request_plugin, #{decode_json_body => true}}
]}
```

```admonish warning
Using `<<"*">>` allows requests from any origin. For production, restrict this to your frontend's domain:

~~~erlang
{pre_request, nova_cors_plugin, #{allow_origins => <<"https://myblog.com">>}}
~~~
```

The plugin adds CORS headers to every response and handles preflight `OPTIONS` requests automatically.

### Per-route CORS

Apply CORS only to API routes:

```erlang
routes(_Environment) ->
  [
    %% API routes with CORS
    #{prefix => "/api",
      plugins => [
          {pre_request, nova_cors_plugin, #{allow_origins => <<"https://myblog.com">>}},
          {pre_request, nova_request_plugin, #{decode_json_body => true}}
      ],
      routes => [
                 {"/posts", fun blog_posts_controller:index/1, #{methods => [get]}},
                 {"/posts", fun blog_posts_controller:create/1, #{methods => [post]}},
                 {"/posts/:id", fun blog_posts_controller:show/1, #{methods => [get]}},
                 {"/posts/:id", fun blog_posts_controller:update/1, #{methods => [put]}},
                 {"/posts/:id", fun blog_posts_controller:delete/1, #{methods => [delete]}}
                ]
    },

    %% HTML routes without CORS
    #{prefix => "",
      plugins => [
          {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
      ],
      routes => [
                 {"/login", fun blog_main_controller:login/1, #{methods => [get, post]}}
                ]
    }
  ].
```

When `plugins` is set on a route group, it overrides the global plugin configuration for those routes.

### Custom CORS plugin

The built-in plugin hardcodes `Allow-Headers` and `Allow-Methods` to `*`. For more control:

```erlang
-module(blog_cors_plugin).
-behaviour(nova_plugin).

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

pre_request(Req, _Env, Options, State) ->
    Origins = maps:get(allow_origins, Options, <<"*">>),
    Methods = maps:get(allow_methods, Options, <<"GET, POST, PUT, DELETE, OPTIONS">>),
    Headers = maps:get(allow_headers, Options, <<"Content-Type, Authorization">>),
    MaxAge = maps:get(max_age, Options, <<"86400">>),

    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origins, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Methods, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, Headers, Req2),
    Req4 = cowboy_req:set_resp_header(<<"access-control-max-age">>, MaxAge, Req3),

    Req5 = case maps:get(allow_credentials, Options, false) of
               true ->
                   cowboy_req:set_resp_header(
                       <<"access-control-allow-credentials">>, <<"true">>, Req4);
               false ->
                   Req4
           end,

    case cowboy_req:method(Req5) of
        <<"OPTIONS">> ->
            Reply = cowboy_req:reply(204, Req5),
            {stop, Reply, State};
        _ ->
            {ok, Req5, State}
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    {<<"blog_cors_plugin">>,
     <<"1.0.0">>,
     <<"Blog">>,
     <<"Configurable CORS plugin">>,
     [allow_origins, allow_methods, allow_headers, max_age, allow_credentials]}.
```

Configure with all options:

```erlang
{pre_request, blog_cors_plugin, #{
    allow_origins => <<"https://myblog.com">>,
    allow_methods => <<"GET, POST, PUT, DELETE">>,
    allow_headers => <<"Content-Type, Authorization, X-Request-ID">>,
    max_age => <<"3600">>,
    allow_credentials => true
}}
```

### Testing CORS

Verify headers with curl:

```shell
# Check preflight response
curl -v -X OPTIONS localhost:8080/api/posts \
  -H "Origin: https://myblog.com" \
  -H "Access-Control-Request-Method: POST"

# Check actual response headers
curl -v localhost:8080/api/posts \
  -H "Origin: https://myblog.com"
```

You should see the `Access-Control-Allow-Origin` header in the response.

## Plugin return values

| Return | Effect |
|---|---|
| `{ok, Req, State}` | Continue to the next plugin or controller |
| `{break, Req, State}` | Skip remaining plugins in this phase, go to controller |
| `{stop, Req, State}` | Stop everything — plugin must have already sent a response |
| `{error, Reason}` | Trigger a 500 error page |

---

For the final chapter, let's add observability with [OpenTelemetry](opentelemetry.md).
