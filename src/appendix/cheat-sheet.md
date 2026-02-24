# Cheat Sheet

Quick reference for Nova's APIs, return values, configuration, and Kura's database layer.

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
| `{sendfile, StatusCode, Headers, {Offset, Length, Path}, MimeType}` | Send a file |

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
    #{title := binary(), version := binary(), url := binary(),
      authors := [binary()], description := binary(),
      options => [{atom(), binary()}]}.
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
    decode_json_body => true,         %% Decode JSON body into `json` key
    read_urlencoded_body => true,     %% Decode URL-encoded form data into `params` key
    parse_qs => true                  %% Parse query string into `parsed_qs` key
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

---

## Kura — Schema definition

```erlang
-module(my_schema).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").
-export([table/0, fields/0, primary_key/0, associations/0, embeds/0]).

table() -> <<"my_table">>.
primary_key() -> id.

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = name, type = string, nullable = false},
        #kura_field{name = status, type = {enum, [active, inactive]}},
        #kura_field{name = metadata, type = {embed, embeds_one, metadata_schema}},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

associations() ->
    [
        #kura_assoc{name = author, type = belongs_to, schema = user, foreign_key = author_id},
        #kura_assoc{name = comments, type = has_many, schema = comment, foreign_key = post_id},
        #kura_assoc{name = tags, type = many_to_many, schema = tag,
                    join_through = <<"posts_tags">>, join_keys = {post_id, tag_id}}
    ].

embeds() ->
    [#kura_embed{name = metadata, type = embeds_one, schema = metadata_schema}].
```

### Kura field types

| Type | PostgreSQL | Erlang |
|---|---|---|
| `id` | `BIGSERIAL` | integer |
| `integer` | `INTEGER` | integer |
| `float` | `DOUBLE PRECISION` | float |
| `string` | `VARCHAR(255)` | binary |
| `text` | `TEXT` | binary |
| `boolean` | `BOOLEAN` | boolean |
| `date` | `DATE` | `{Y, M, D}` |
| `utc_datetime` | `TIMESTAMPTZ` | `{{Y,M,D},{H,Mi,S}}` |
| `uuid` | `UUID` | binary |
| `jsonb` | `JSONB` | map/list |
| `{enum, [atoms]}` | `VARCHAR(255)` | atom |
| `{array, Type}` | `Type[]` | list |
| `{embed, embeds_one, Mod}` | `JSONB` | map |
| `{embed, embeds_many, Mod}` | `JSONB` | list of maps |

## Kura — Changeset API

```erlang
%% Create a changeset
CS = kura_changeset:cast(SchemaModule, ExistingData, Params, AllowedFields).

%% Validations
kura_changeset:validate_required(CS, [field1, field2])
kura_changeset:validate_format(CS, field, <<"regex">>)
kura_changeset:validate_length(CS, field, [{min, 3}, {max, 200}])
kura_changeset:validate_number(CS, field, [{greater_than, 0}])
kura_changeset:validate_inclusion(CS, field, [val1, val2, val3])
kura_changeset:validate_change(CS, field, fun(Val) -> ok | {error, Msg} end)

%% Constraint declarations
kura_changeset:unique_constraint(CS, field)
kura_changeset:foreign_key_constraint(CS, field)
kura_changeset:check_constraint(CS, ConstraintName, field, #{message => Msg})

%% Association/embed casting
kura_changeset:cast_assoc(CS, assoc_name)
kura_changeset:cast_assoc(CS, assoc_name, #{with => Fun})
kura_changeset:put_assoc(CS, assoc_name, Value)
kura_changeset:cast_embed(CS, embed_name)

%% Changeset helpers
kura_changeset:get_change(CS, field)       -> Value | undefined
kura_changeset:get_field(CS, field)        -> Value | undefined
kura_changeset:put_change(CS, field, Val)  -> CS1
kura_changeset:add_error(CS, field, Msg)   -> CS1
kura_changeset:apply_changes(CS)           -> DataMap
kura_changeset:apply_action(CS, Action)    -> {ok, Data} | {error, CS}
```

### Schemaless changesets

```erlang
Types = #{email => string, age => integer},
CS = kura_changeset:cast(Types, #{}, Params, [email, age]).
```

## Kura — Query builder

```erlang
Q = kura_query:from(schema_module),

%% Where conditions
Q1 = kura_query:where(Q, {field, value}),                  %% =
Q1 = kura_query:where(Q, {field, '>', value}),             %% comparison
Q1 = kura_query:where(Q, {field, in, [val1, val2]}),       %% IN
Q1 = kura_query:where(Q, {field, ilike, <<"%term%">>}),    %% ILIKE
Q1 = kura_query:where(Q, {field, is_nil}),                 %% IS NULL
Q1 = kura_query:where(Q, {'or', [{f1, v1}, {f2, v2}]}),   %% OR

%% Ordering, pagination
Q2 = kura_query:order_by(Q, [{field, asc}]),
Q3 = kura_query:limit(Q, 10),
Q4 = kura_query:offset(Q, 20),

%% Preloading associations
Q5 = kura_query:preload(Q, [author, {comments, [author]}]).
```

## Kura — Repository API

```erlang
%% Read
blog_repo:all(Query)                 -> {ok, [Map]}
blog_repo:get(Schema, Id)            -> {ok, Map} | {error, not_found}
blog_repo:get_by(Schema, Clauses)    -> {ok, Map} | {error, not_found}
blog_repo:one(Query)                 -> {ok, Map} | {error, not_found}

%% Write
blog_repo:insert(Changeset)          -> {ok, Map} | {error, Changeset}
blog_repo:insert(Changeset, Opts)    -> {ok, Map} | {error, Changeset}
blog_repo:update(Changeset)          -> {ok, Map} | {error, Changeset}
blog_repo:delete(Changeset)          -> {ok, Map} | {error, Changeset}

%% Bulk
blog_repo:insert_all(Schema, [Map])  -> {ok, Count}
blog_repo:update_all(Query, Updates) -> {ok, Count}
blog_repo:delete_all(Query)          -> {ok, Count}

%% Preloading
blog_repo:preload(Schema, Records, Assocs) -> Records

%% Transactions
blog_repo:transaction(Fun)           -> {ok, Result} | {error, Reason}
blog_repo:multi(Multi)               -> {ok, Results} | {error, Step, Value, Completed}
```

### Upsert options

```erlang
blog_repo:insert(CS, #{on_conflict => {field, nothing}})
blog_repo:insert(CS, #{on_conflict => {field, replace_all}})
blog_repo:insert(CS, #{on_conflict => {field, {replace, [fields]}}})
```

## Kura — Multi (transaction pipelines)

```erlang
M = kura_multi:new(),
M1 = kura_multi:insert(M, step_name, Changeset),
M2 = kura_multi:update(M1, step_name, fun(Results) -> Changeset end),
M3 = kura_multi:delete(M2, step_name, Changeset),
M4 = kura_multi:run(M3, step_name, fun(Results) -> {ok, Value} end),
{ok, #{step1 := V1, step2 := V2}} = blog_repo:multi(M4).
```

## Common rebar3 commands

| Command | Description |
|---|---|
| `rebar3 compile` | Compile the project (also triggers kura migration generation) |
| `rebar3 shell` | Start interactive shell |
| `rebar3 nova serve` | Dev server with hot-reload |
| `rebar3 nova routes` | List registered routes |
| `rebar3 eunit` | Run EUnit tests |
| `rebar3 ct` | Run Common Test suites |
| `rebar3 do eunit, ct` | Run both |
| `rebar3 as prod release` | Build production release |
| `rebar3 as prod tar` | Build release tarball |
| `rebar3 dialyzer` | Run type checker |

## rebar3_nova commands

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

## rebar3_kura commands

| Command | Description |
|---|---|
| `rebar3 kura setup --name REPO` | Generate a repo module and migrations directory |
| `rebar3 kura compile` | Diff schemas vs migrations and generate new migrations |

### Generator options

```shell
# Controller with specific actions
rebar3 nova gen_controller --name products --actions list,show,create

# OpenAPI with custom output
rebar3 nova openapi --output priv/assets/openapi.json --title "My API" --api-version 1.0.0

# Kura setup with custom repo name
rebar3 kura setup --name my_repo
```
