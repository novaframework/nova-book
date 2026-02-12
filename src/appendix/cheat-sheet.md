# Cheat Sheet

Quick reference for Nova's APIs, return values, and configuration.

## Controller return tuples

| Return | Description |
|---|---|
| `{ok, Variables}` | Render the default template with variables |
| `{ok, Variables, #{view => Name}}` | Render a specific template |
| `{ok, Variables, #{view => Name, status_code => Code}}` | Render template with custom status |
| `{json, Data}` | JSON response (status 200) |
| `{json, StatusCode, Headers, Body}` | JSON response with custom status and headers |
| `{status, StatusCode}` | Bare status code response |
| `{status, StatusCode, Headers, Body}` | Status with headers and body |
| `{redirect, Path}` | HTTP redirect |
| `{sendfile, StatusCode, Headers, FilePath, Offset, Length}` | Send a file |

## Route configuration

```erlang
#{
    prefix => "/api",                                %% Path prefix (string)
    security => false | fun Module:Function/1,       %% Security function
    plugins => [{Phase, Module, Options}],           %% Per-route plugins (optional)
    routes => [
        {Path, fun Module:Function/1, #{methods => [get, post, put, delete]}},
        {Path, WebSocketModule, #{protocol => ws}},  %% WebSocket route
        {StatusCode, fun Module:Function/1, #{}}     %% Error handler
    ]
}
```

### Path parameters

```erlang
{"/users/:id", fun my_controller:show/1, #{methods => [get]}}
%% Access in controller:
show(#{bindings := #{<<"id">> := Id}}) -> ...
```

## Security functions

```erlang
%% Return {true, AuthData} to allow, false to deny
my_security(#{params := Params}) ->
    case check_credentials(Params) of
        ok -> {true, #{user => <<"alice">>}};
        _  -> false
    end.

%% AuthData is available in the controller as auth_data
index(#{auth_data := #{user := User}}) -> ...
```

## Plugin callbacks

```erlang
-behaviour(nova_plugin).

pre_request(Req, Env, Options, State) ->
    {ok, Req, State} |       %% Continue
    {break, Req, State} |    %% Skip remaining plugins
    {stop, Req, State} |     %% Stop — plugin sent response
    {error, Reason}.         %% 500 error

post_request(Req, Env, Options, State) ->
    %% Same return values as pre_request

plugin_info() ->
    {Title, Version, Author, Description, OptionKeys}.
```

### Plugin configuration

```erlang
%% Global (sys.config)
{plugins, [
    {pre_request, Module, Options},
    {post_request, Module, Options}
]}

%% Per-route (in router)
#{plugins => [{pre_request, Module, Options}],
  routes => [...]}
```

## Session API

```erlang
nova_session:get(Req, <<"key">>)            -> {ok, Value} | {error, not_found}
nova_session:set(Req, <<"key">>, Value)     -> ok
nova_session:delete(Req)                    -> {ok, Req1}
nova_session:delete(Req, <<"key">>)         -> {ok, Req1}
nova_session:generate_session_id()          -> {ok, SessionId}
```

### Cookie setup

```erlang
Req1 = cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req, #{
    path => <<"/">>,
    http_only => true,
    secure => true,
    max_age => 86400
}).
```

## WebSocket callbacks

```erlang
-behaviour(nova_websocket).

init(State) ->
    {ok, State}.                          %% Accept connection

websocket_handle({text, Msg}, State) ->
    {ok, State} |                         %% Do nothing
    {reply, {text, Response}, State} |    %% Send message
    {stop, State}.                        %% Close connection

websocket_info(ErlangMsg, State) ->
    %% Same return values as websocket_handle
```

### WebSocket route

```erlang
{"/ws", my_ws_handler, #{protocol => ws}}
```

## Pub/Sub API

```erlang
nova_pubsub:join(Channel)
nova_pubsub:leave(Channel)
nova_pubsub:broadcast(Channel, Topic, Payload)
nova_pubsub:local_broadcast(Channel, Topic, Payload)
nova_pubsub:get_members(Channel)
nova_pubsub:get_local_members(Channel)

%% Message format received by processes:
{nova_pubsub, Channel, SenderPid, Topic, Payload}
```

## Nova request plugin options

```erlang
{pre_request, nova_request_plugin, #{
    decode_json_body => true,         %% Decode JSON request bodies
    read_urlencoded_body => true,     %% Decode URL-encoded form data
    read_body => true                 %% Read raw body
}}
```

## Nova configuration (sys.config)

```erlang
{nova, [
    {environment, dev | prod},
    {bootstrap_application, my_app},
    {dev_mode, true | false},
    {use_stacktrace, true | false},
    {session_manager, nova_session_ets},
    {render_error_pages, true | false},
    {cowboy_configuration, #{
        port => 8080,
        use_ssl => false,
        ssl_port => 8443,
        ssl_options => #{certfile => "...", keyfile => "..."},
        stream_handlers => [cowboy_stream_h]
    }},
    {plugins, [...]}
]}
```

## Sub-applications

```erlang
{my_app, [
    {nova_apps, [
        {nova_admin, #{prefix => "/admin"}},
        {other_app, #{prefix => "/other"}}
    ]}
]}
```

## Common rebar3 commands

| Command | Description |
|---|---|
| `rebar3 compile` | Compile the project |
| `rebar3 shell` | Start interactive shell |
| `rebar3 nova serve` | Dev server with hot-reload |
| `rebar3 nova routes` | List registered routes |
| `rebar3 eunit` | Run EUnit tests |
| `rebar3 ct` | Run Common Test suites |
| `rebar3 do eunit, ct` | Run both |
| `rebar3 as prod release` | Build production release |
| `rebar3 as prod tar` | Build release tarball |
| `rebar3 dialyzer` | Run type checker |

## Nova developer tool commands

| Command | Description |
|---|---|
| `rebar3 nova gen_controller --name NAME` | Generate a controller with stub actions |
| `rebar3 nova gen_resource --name NAME` | Generate controller + JSON schema + route hints |
| `rebar3 nova gen_test --name NAME` | Generate a Common Test suite |
| `rebar3 nova openapi` | Generate OpenAPI 3.0.3 spec + Swagger UI |
| `rebar3 nova config` | Show Nova configuration with defaults |
| `rebar3 nova middleware` | Show global and per-group plugin chains |
| `rebar3 nova audit` | Find routes missing security callbacks |
| `rebar3 nova release` | Build release with auto-generated OpenAPI |

### Generator options

```shell
# Controller with specific actions
rebar3 nova gen_controller --name products --actions list,show,create

# OpenAPI with custom output
rebar3 nova openapi --output priv/assets/openapi.json --title "My API" --api-version 1.0.0

# Release with specific profile
rebar3 nova release --profile staging
```
