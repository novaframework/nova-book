# Observability

When the blog is slow or failing in production you want three things: a trace of each request with the queries it ran, metrics you can graph and alert on, and log lines you can tie to that trace. This chapter wires up all three with OpenTelemetry, using `opentelemetry_nova` for HTTP and `opentelemetry_kura` for the database, and tests the result in Common Test.

## Dependencies

```erlang
{deps, [
    ...
    {opentelemetry_nova, "~> 0.2"},
    {opentelemetry_kura, "~> 0.2"},
    {opentelemetry, "~> 1.7"},
    {opentelemetry_exporter, "~> 1.11"},
    ...
]}.
```

The lock file resolves them to opentelemetry_nova 0.2.2, opentelemetry_kura 0.2.2, the SDK `opentelemetry` 1.7.0, `opentelemetry_experimental` 0.6.0 (the metrics SDK, pulled in by opentelemetry_nova) and `opentelemetry_exporter` 1.11.0. The exporter is the OTLP client: `{traces_exporter, otlp}` (and the SDK's default exporter) is that module, and opentelemetry_nova does not depend on it.

All five go into `blog.app.src` **before** `nova` and `kura`, so the SDK is running before the first request or query is instrumented:

```erlang
{applications, [
    kernel,
    stdlib,
    opentelemetry_exporter,
    opentelemetry,
    opentelemetry_experimental,
    opentelemetry_nova,
    opentelemetry_kura,
    nova,
    kura,
    ...
]},
```

`blog_app:start/2` registers the instruments and the Kura telemetry handler:

```erlang
ok = opentelemetry_nova:setup(),
ok = opentelemetry_kura:setup(),
```

`opentelemetry_nova:setup/0` creates four metric instruments (`http.server.request.duration`, `http.server.active_requests` and the request and response body sizes). `opentelemetry_kura:setup/0` attaches to Kura's `[kura, repo, query]` telemetry event and turns every query into a client span.

## Traces: one span per request

opentelemetry_nova's tracing lives in a Cowboy stream handler, `otel_nova_stream_h`. It reads an incoming W3C `traceparent` header, starts a server span with the method, path, scheme, host, port, peer address and user agent, and ends it with the status code once the response is sent. It also records the metrics.

The package's README lists two components for this: the stream handler, which goes first in `stream_handlers`, and `otel_nova_plugin`, a pre-request plugin that renames the span to `GET blog_posts_controller:index` and adds `nova.app`, `nova.controller` and `nova.action`. Used that way, two things go wrong in 0.2.2:

- **The plugin cannot see the span, and queries lose their parent.** Cowboy runs stream handlers in the *connection* process, but plugins, controllers and Kura run in a separate *request* process. The span context the stream handler attached lives in the connection process's dictionary, so in the request process there is no current span: the plugin's attribute calls are no-ops and every Kura query becomes a separate root trace.
- **Static files return 500.** For `sendfile` responses (every file Nova serves from `priv/`), the stream handler calls `iolist_size/1` on the `{sendfile, ...}` tuple, crashes, and Cowboy answers 500. The crash report also dumps the stream handler state, request cookies included.

The blog keeps the stream handler but wraps it, and replaces the plugin with a middleware.

### `blog_otel_h`: a wrapping stream handler

`blog_otel_h` is a stream handler that calls `otel_nova_stream_h` on the side and then passes everything to the rest of the chain. It fixes both problems at the one place they can be fixed: it copies the span context into the request map (which Cowboy hands to the request process), and it shows the OpenTelemetry handler a plain `headers` event instead of a `sendfile` body.

```erlang
-module(blog_otel_h).
-behaviour(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-record(state, {otel, next}).

init(StreamID, Req, Opts) ->
    {[], Otel} = otel_nova_stream_h:init(StreamID, Req, Opts#{stream_handlers => []}),
    Req1 = Req#{otel_ctx => otel_ctx:get_current()},
    {Commands, Next} = cowboy_stream:init(StreamID, Req1, Opts),
    {Commands, #state{otel = Otel, next = Next}}.

data(StreamID, IsFin, Data, #state{otel = Otel, next = Next} = State) ->
    {[], Otel1} = otel_nova_stream_h:data(StreamID, IsFin, Data, Otel),
    {Commands, Next1} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{otel = Otel1, next = Next1}}.

info(StreamID, Info, #state{otel = Otel, next = Next} = State) ->
    {[], Otel1} = otel_nova_stream_h:info(StreamID, without_sendfile(Info), Otel),
    {Commands, Next1} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{otel = Otel1, next = Next1}}.

terminate(StreamID, Reason, #state{otel = Otel, next = Next}) ->
    Result = cowboy_stream:terminate(StreamID, Reason, Next),
    otel_nova_stream_h:terminate(StreamID, Reason, Otel),
    Result.

early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

without_sendfile({response, Status, Headers, {sendfile, _Offset, _Length, _Path}}) ->
    {headers, Status, Headers};
without_sendfile(Info) ->
    Info.
```

Passing `stream_handlers => []` makes `otel_nova_stream_h` the last handler in its own little chain, so it returns `{[], State}` and never calls the next handler itself; `blog_otel_h` does that with the real `Opts`. The OpenTelemetry handler still sees every event, so the span and the metrics come out the same.

### `blog_otel_middleware`: enriching the span

In the request process the context arrives in the request map. A Cowboy middleware placed right after `nova_router` (which has just put the matched controller into `Env`) attaches it and then calls `otel_nova_plugin:pre_request/4` directly to rename the span:

```erlang
-module(blog_otel_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(#{otel_ctx := Ctx} = Req, Env) ->
    _ = otel_ctx:attach(Ctx),
    {ok, _Req, _State} = otel_nova_plugin:pre_request(Req, Env, #{}, undefined),
    {ok, Req, Env};
execute(Req, Env) ->
    {ok, Req, Env}.
```

A middleware rather than a plugin, because global plugins only run for route groups that have no `plugins` of their own, and most of the blog's groups do. Every request passes through the middleware chain.

### The handler lists

In all three configs:

```erlang
{cowboy_configuration, #{
    port => 8080,
    stream_handlers => [
        blog_otel_h,
        nova_stream_h,
        blog_session_h,
        cowboy_compress_h,
        cowboy_stream_h
    ],
    middleware_handlers => [
        arizona_cowboy_middleware,
        nova_router,
        blog_otel_middleware,
        nova_plugin_handler,
        nova_security_handler,
        nova_handler,
        nova_plugin_handler
    ]
}},
```

`blog_otel_h` comes first so that the span covers the whole request and sees the final status. Arizona's live routes are handled by `arizona_cowboy_middleware` before `nova_router`, so they get the server span from the stream handler but not the controller name.

## Where traces go

The SDK is configured per environment:

```erlang
%% dev: build spans, export none
{opentelemetry, [{span_processor, batch}, {traces_exporter, none}]},

%% test: hand each span to the test process as soon as it ends
{opentelemetry, [{span_processor, simple}, {traces_exporter, none}]},

%% prod: batch and send over OTLP
{opentelemetry, [
    {span_processor, batch},
    {traces_exporter, otlp}
]},
```

The OTLP exporter sends HTTP/protobuf to `http://localhost:4318` unless told otherwise, and like every OpenTelemetry SDK it reads the standard environment variables, so the endpoint and the service name are set where the release runs rather than in the config:

```shell
OTEL_EXPORTER_OTLP_ENDPOINT=http://collector:4318 bin/blog foreground
```

`service.name` defaults to the release name (`blog`) and `service.version` to the release version. With no collector listening, the exporter logs one line at `info` per failed batch (`client error exporting {failed_connect, ...}`) and drops the spans; the blog keeps serving.

### Seeing it in a collector

To check the production setup end to end, run Grafana's all-in-one OpenTelemetry image (a collector with Tempo, Prometheus and Loki behind it) and point the release at it:

```shell
docker run -d --name otel -p 4318:4318 -p 3200:3200 grafana/otel-lgtm:latest
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 bin/blog daemon
curl -s localhost:8080/api/posts > /dev/null
curl -s 'localhost:3200/api/search?tags=service.name%3Dblog&limit=5'
```

```json
{"traces":[{"traceID":"64962a6f7ba24137b7276be3c3deddfb","rootServiceName":"blog","rootTraceName":"GET blog_posts_controller:index","startTimeUnixNano":"1793699368800296389","durationMs":7}],...}
```

Fetching that trace from Tempo (`/api/traces/<id>`) shows the request span and the two queries behind the post listing, each a child of the request span:

| Scope | Span | Kind | `db.statement` |
|---|---|---|---|
| opentelemetry_nova | `GET blog_posts_controller:index` | server | |
| opentelemetry_kura | `SELECT posts` | client | `SELECT * FROM "posts" WHERE "status" = $1 AND "deleted_at" IS NULL ORDER BY "inserted_at" DESC, "id" DESC LIMIT $2 OFFSET $3` |
| opentelemetry_kura | `SELECT posts` | client | `SELECT count(*) AS "count" FROM "posts" WHERE "status" = $1 AND "deleted_at" IS NULL` |

The statements carry placeholders, not values; the spans only record how many parameters there were. That matters for the [Security](security.md#logging-hygiene) chapter: a trace never contains the email address someone logged in with.

## Metrics for Prometheus

opentelemetry_nova also ships a Prometheus exporter (`otel_nova_prom_exporter`) and a Cowboy handler that renders its table (`otel_nova_prom_handler`). Two more problems in 0.2.2:

- The metrics SDK (`opentelemetry_experimental` 0.6.0) calls `Exporter:export/3` and `Exporter:shutdown/1`, while `otel_nova_prom_exporter` only exports `export/4` and `shutdown/0`. Configured as the README says, the reader never exports anything and `/metrics` stays empty.
- `otel_nova_prom_server`, the listener the README starts with `opentelemetry_nova:setup(#{prometheus => #{port => 9464}})`, does not trap exits, so its Ranch listener survives an application restart and the next start fails with `already_started`.

`blog_prom_exporter` adapts the exporter to the behaviour the reader expects:

```erlang
-module(blog_prom_exporter).
-behaviour(otel_exporter_metrics).

-export([init/1, export/3, shutdown/1]).

init(Config) ->
    otel_nova_prom_exporter:init(Config).

export(Metrics, Resource, State) ->
    otel_nova_prom_exporter:export(metrics, Metrics, Resource, State).

shutdown(_State) ->
    otel_nova_prom_exporter:shutdown().
```

and `blog_metrics`, a small gen_server under `blog_sup`, owns the `/metrics` listener and stops it when it terminates:

```erlang
init(Port) ->
    process_flag(trap_exit, true),
    Dispatch = cowboy_router:compile([{'_', [{"/metrics", otel_nova_prom_handler, []}]}]),
    {ok, _} = cowboy:start_clear(?MODULE, [{port, Port}], #{env => #{dispatch => Dispatch}}),
    {ok, Port}.

terminate(_Reason, _Port) ->
    cowboy:stop_listener(?MODULE).
```

It is started only when `{blog, [{metrics_port, Port}]}` is set (9464 in dev, `${METRICS_PORT:-9464}` in prod, `0` in test). The metrics listener is separate from the blog's port on purpose: Prometheus scrapes it from inside your network, and the public load balancer never routes to it.

The reader is configured in `opentelemetry_experimental`:

```erlang
{opentelemetry_experimental, [
    {readers, [
        #{
            module => otel_metric_reader,
            config => #{export_interval_ms => 10000, exporter => {blog_prom_exporter, #{}}}
        }
    ]}
]}
```

Every 10 seconds the reader collects and the exporter refreshes the table that `/metrics` renders. After a few requests to the release:

```shell
curl -s localhost:9464/metrics | grep -v '^#' | head -4
```

```
http_server_active_requests{http_request_method="GET",server_address="127.0.0.1",server_port="58107",url_scheme="http"} 0
http_server_request_duration_seconds_bucket{http_request_method="GET",http_response_status_code="200",server_address="127.0.0.1",server_port="58107",url_scheme="http",le="0.005"} 2
http_server_request_duration_seconds_bucket{http_request_method="GET",http_response_status_code="200",server_address="127.0.0.1",server_port="58107",url_scheme="http",le="0.01"} 3
http_server_request_duration_seconds_bucket{http_request_method="GET",http_response_status_code="200",server_address="127.0.0.1",server_port="58107",url_scheme="http",le="0.025"} 4
```

`server_address` and `server_port` come from the request's `Host` header, so they show the address the client used, not the port the node listens on.

```admonish note
The metric labels have no route or controller, only method, status, scheme and host. That keeps the number of series small, but you cannot graph one endpoint's latency from these metrics. Use the traces for that.
```

## Logs that point at traces

Once the context is attached in the request process, the SDK puts `otel_trace_id`, `otel_span_id` and `otel_trace_flags` into the process's logger metadata. Every `?LOG_*` call made while handling the request carries them, with no code in the controller.

The production config logs one JSON object per line with `nova_jsonlogger`, a formatter that ships inside Nova (it encodes with the `json_lib` you configured, so OTP `json` here):

```erlang
{kernel, [
    {logger_level, info},
    {logger, [
        {handler, default, logger_std_h, #{
            formatter => {nova_jsonlogger, #{new_line => true}},
            filters => [{progress, {fun logger_filters:progress/2, stop}}]
        }}
    ]}
]},
```

The `progress` filter drops OTP's supervisor and application progress reports, about 150 lines at every boot that say nothing a log reader needs. A request line from the request logger plugin built in the [next chapter](plugins-cors.md):

```json
{"line":20,"pid":"<0.1402.0>","status":200,"time":1793698949603639,"file":".../src/plugins/blog_request_logger.erl","path":"/health","gl":"<0.1297.0>","msg":"request","level":"info","mfa":"blog_request_logger:post_request/4","method":"GET","otel_span_id":"2be424fb003bdcc6","otel_trace_flags":"01","otel_trace_id":"0dcb2eaa63888b6ae85d41e6905288c8","duration_us":1813}
```

Search your trace backend for `otel_trace_id` and you have the spans of exactly that request.

## Testing spans

The test config uses the `simple` span processor, which exports each span as soon as it ends. The SDK has an exporter that sends spans as messages to a pid, so a suite can point it at the test process and `receive` the spans:

```erlang
-include_lib("opentelemetry/include/otel_span.hrl").

-define(TRACE_ID, 16#4bf92f3577b34da6a3ce929d0e0e4736).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    ok = otel_simple_processor:set_exporter(otel_exporter_pid, self()),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = otel_simple_processor:set_exporter(none, []),
    blog_data_fixtures:checkin().

receive_span(Name) ->
    receive
        {span, #span{name = Name} = Span} -> Span
    after 5000 -> ct:fail({no_span, Name})
    end.
```

`blog_prod_telemetry_SUITE` checks the request span, the parent of the query spans and trace propagation:

```erlang
request_gets_a_server_span(_Config) ->
    {200, _} = blog_data_fixtures:request(get, "/api/posts"),
    #span{kind = server, attributes = Attributes} = receive_span(
        ~"GET blog_posts_controller:index"
    ),
    #{
        'http.request.method' := ~"GET",
        'url.path' := ~"/api/posts",
        'http.response.status_code' := 200,
        'nova.app' := ~"blog",
        'nova.controller' := ~"blog_posts_controller",
        'nova.action' := ~"index"
    } = otel_attributes:map(Attributes).

queries_are_children_of_the_request_span(_Config) ->
    {200, _} = blog_data_fixtures:request(get, "/api/posts"),
    Query = receive_span(~"SELECT posts"),
    Request = receive_span(~"GET blog_posts_controller:index"),
    ?assertEqual(Request#span.trace_id, Query#span.trace_id),
    ?assertEqual(Request#span.span_id, Query#span.parent_span_id),
    #{'db.system' := ~"postgresql", 'db.statement' := Statement} =
        otel_attributes:map(Query#span.attributes),
    ?assertMatch({match, _}, re:run(Statement, ~"\\$1")).

traceparent_continues_the_callers_trace(_Config) ->
    Traceparent = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01",
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        get, {blog_data_fixtures:url("/api/posts"), [{"traceparent", Traceparent}]}, [], []
    ),
    #span{trace_id = TraceId, parent_span_id = Parent} =
        receive_span(~"GET blog_posts_controller:index"),
    ?assertEqual(?TRACE_ID, TraceId),
    ?assertEqual(16#00f067aa0ba902b7, Parent).
```

Two tests pin the upstream problems on a bare Cowboy listener with only `otel_nova_stream_h`: a handler there sees `undefined` as its current span, and a `sendfile` reply comes back as 500. When a fixed opentelemetry_nova is released, those two fail, and that is your cue to delete `blog_otel_h`. The others check that `/robots.txt` gets a span named `GET nova_file_controller:get_file`, that after `otel_meter_server:force_flush()` the `/metrics` page has `http_server_request_duration_seconds`, that `otel_nova_prom_exporter` still lacks `export/3`, and that `blog` can be stopped and started again with the metrics listener coming back.

```shell
rebar3 ct --suite=test/blog_prod_telemetry_SUITE
```

```
All 9 tests passed.
```

---

Next: [Custom Plugins & CORS](plugins-cors.md), including the request logger whose lines you saw above.
