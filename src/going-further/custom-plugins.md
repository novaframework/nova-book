# Custom Plugins

In the [Plugins](../getting-started/plugins.md) chapter we saw how Nova's built-in plugins work. Now let's build our own from scratch.

## The nova_plugin behaviour

A plugin module implements these callbacks:

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
    {Title, Version, Author, Description, Options}.
```

## Example 1: Request logger

A plugin that logs every request with method, path, and response time.

Create `src/plugins/my_first_nova_logger_plugin.erl`:

```erlang
-module(my_first_nova_logger_plugin).
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
    {<<"my_first_nova_logger_plugin">>,
     <<"1.0.0">>,
     <<"My First Nova">>,
     <<"Logs request method, path and duration">>,
     []}.
```

Register it as both pre-request and post-request in `sys.config`:

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{decode_json_body => true,
                                          read_urlencoded_body => true}},
    {pre_request, my_first_nova_logger_plugin, #{}},
    {post_request, my_first_nova_logger_plugin, #{}}
]}
```

Output:

```
[info] GET /api/users completed in 3ms
[info] POST /api/users completed in 12ms
```

## Example 2: Rate limiter

A plugin that limits requests per IP address using ETS:

```erlang
-module(my_first_nova_rate_limit_plugin).
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
    case ets:lookup(nova_rate_limits, Key) of
        [{Key, Count, WindowStart}] when Now - WindowStart < WindowMs ->
            if Count >= MaxRequests ->
                    Reply = cowboy_req:reply(429,
                        #{<<"content-type">> => <<"application/json">>},
                        <<"{\"error\":\"too many requests\"}">>,
                        Req),
                    {stop, Reply, State};
               true ->
                    ets:update_element(nova_rate_limits, Key, {2, Count + 1}),
                    {ok, Req, State}
            end;
        _ ->
            ets:insert(nova_rate_limits, {Key, 1, Now}),
            {ok, Req, State}
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    {<<"my_first_nova_rate_limit_plugin">>,
     <<"1.0.0">>,
     <<"My First Nova">>,
     <<"Simple IP-based rate limiting">>,
     [max_requests, window_ms]}.
```

Create the ETS table on application start in `src/my_first_nova_app.erl`:

```erlang
start(_StartType, _StartArgs) ->
    ets:new(nova_rate_limits, [named_table, public, set]),
    my_first_nova_sup:start_link().
```

Configure:

```erlang
{pre_request, my_first_nova_rate_limit_plugin, #{
    max_requests => 60,
    window_ms => 60000
}}
```

When the limit is exceeded, the plugin returns `{stop, Reply, State}` — a 429 response is sent and the controller is never called.

## Example 3: Request ID plugin

Add a unique request ID header to every response:

```erlang
-module(my_first_nova_request_id_plugin).
-behaviour(nova_plugin).

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

pre_request(Req, _Env, _Options, State) ->
    RequestId = generate_id(),
    Req1 = cowboy_req:set_resp_header(<<"x-request-id">>, RequestId, Req),
    {ok, Req1#{request_id => RequestId}, State}.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    {<<"my_first_nova_request_id_plugin">>,
     <<"1.0.0">>,
     <<"My First Nova">>,
     <<"Adds X-Request-ID header to responses">>,
     []}.

generate_id() ->
    Bytes = crypto:strong_rand_bytes(16),
    list_to_binary(
        lists:flatten(
            [io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bytes])).
```

## Per-route plugins

Set plugins on specific route groups instead of globally:

```erlang
routes(_Environment) ->
  [
    #{prefix => "/api",
      plugins => [
          {pre_request, nova_cors_plugin, #{allow_origins => <<"*">>}},
          {pre_request, nova_request_plugin, #{decode_json_body => true}},
          {pre_request, my_first_nova_rate_limit_plugin, #{max_requests => 100}}
      ],
      routes => [
                 {"/users", fun my_first_nova_api_controller:index/1, #{methods => [get]}}
                ]
    },

    #{prefix => "",
      plugins => [
          {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
      ],
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get, post]}}
                ]
    }
  ].
```

When `plugins` is set on a route group, it overrides the global plugin configuration for those routes.

## Plugin return values

| Return | Effect |
|---|---|
| `{ok, Req, State}` | Continue to the next plugin or controller |
| `{break, Req, State}` | Skip remaining plugins in this phase, go to controller |
| `{stop, Req, State}` | Stop everything — plugin must have already sent a response |
| `{error, Reason}` | Trigger a 500 error page |

---

Next, let's look at [CORS](cors.md) — a common need when your API serves a separate frontend.
