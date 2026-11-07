# Observability

When the blog is slow or failing in production you want three things: a trace of each request with the queries it ran, metrics you can graph and alert on, and log lines you can tie to that trace. This chapter wires up all three with OpenTelemetry, using `opentelemetry_nova` for HTTP and `opentelemetry_kura` for the database, and tests the result in Common Test.

## Dependencies

```erlang
{deps, [
    ...
    {opentelemetry_nova, "~> 0.5"},
    {opentelemetry_kura, "~> 0.2"},
    {opentelemetry, "~> 1.7"},
    {opentelemetry_exporter, "~> 1.11"},
    ...
]}.
```

The lock file resolves them to opentelemetry_nova 0.5.0, opentelemetry_kura 0.2.2, the SDK `opentelemetry` 1.7.0, `opentelemetry_experimental` 0.6.0 (the metrics SDK, pulled in by opentelemetry_nova) and `opentelemetry_exporter` 1.11.0. The exporter is the OTLP client: `{traces_exporter, otlp}` (and the SDK's default exporter) is that module, and opentelemetry_nova does not depend on it.

opentelemetry_nova's own `rebar.config` has a shell `pre_hooks` entry meant for building the library on its own. It fails when the library is compiled as a dependency, so the blog's `rebar.config` empties it:

```erlang
{overrides, [{override, opentelemetry_nova, [{pre_hooks, []}]}]}.
```

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

opentelemetry_nova needs two pieces in the request path, and the reason is the way Cowboy and Nova split the work between processes.

- **`otel_nova_stream_h`**, a Cowboy stream handler. It reads an incoming W3C `traceparent` header, starts a server span with the method, path, scheme, host, port, peer address and user agent, and ends it with the status code once the response is sent. It also records the metrics. Cowboy runs stream handlers in the *connection* process.
- **`otel_nova_plugin`**, a pre-request plugin. Plugins, security functions, controllers and Kura run in a separate *request* process, where there is no current span. The stream handler puts the span's context into the request map, and the plugin attaches it in the request process. From then on every span created there (a Kura query, a span of your own) is a child of the server span. The plugin also renames the span after the route template, adds `http.route`, `nova.app`, `nova.controller` and `nova.action`, and puts `trace_id` and `span_id` in the logger metadata.

### The handler lists

In all three configs, the stream handler goes first in `stream_handlers` so that the span covers the whole request and sees the final status, and the plugin goes first in the global plugin list so that everything after it runs with the context attached:

```erlang
{cowboy_configuration, #{
    port => 8080,
    stream_handlers => [
        otel_nova_stream_h,
        nova_stream_h,
        blog_session_h,
        cowboy_compress_h,
        cowboy_stream_h
    ],
    middleware_handlers => [
        arizona_cowboy_middleware,
        nova_router,
        nova_plugin_handler,
        nova_security_handler,
        nova_handler,
        nova_plugin_handler
    ]
}},
{bootstrap_application, blog},
{plugins, [
    {pre_request, otel_nova_plugin, #{}},
    {pre_request, blog_request_logger, #{}},
    {pre_request, nova_request_plugin, #{decode_json_body => true,
                                         read_urlencoded_body => true}},
    {post_request, blog_request_logger, #{}}
]}
```

A plugin listed before `otel_nova_plugin` runs without a trace context, so its log lines carry no trace id. `blog_prod_config_SUITE:every_config_wires_tracing` checks the order in every config file.

### Route groups with their own plugins

The global list only applies to route groups without a `plugins` key ([Custom Plugins & CORS](plugins-cors.md#where-plugins-run) has the rules). Most of the blog's groups have one, and in those groups the plugin would never run: the span would keep the name `HTTP GET` and every query would start a trace of its own. Nova's `plugin_strategy` option can merge the two lists, but only for applications mounted through `nova_apps`, not for the bootstrap application. So the router puts the plugin at the front of every group that lists its own plugins:

```erlang
routes(_Environment) ->
    [traced(Group) || Group <- route_groups()].
```

```erlang
traced(#{plugins := Plugins} = Group) ->
    Group#{plugins => [{pre_request, otel_nova_plugin, #{}} | Plugins]};
traced(Group) ->
    Group.
```

`route_groups/0` is the list of groups you have been writing since Part I. Doing it in one place means a group added later cannot forget it. Without `traced/1`, the request and query tests below fail, because `/api/posts` lives in a group with its own plugins.

Arizona's live routes are handled by `arizona_cowboy_middleware` before `nova_router`, so Nova's plugins never run for them. They get the server span from the stream handler, named `HTTP GET`, but no route and no child spans.

### Span names and `http.route`

The plugin names the span `{METHOD} {route}` after the route template: a request for `/posts/42` is `GET /posts/:id`, and every file under the static `/assets/[...]` route is `GET /assets/[...]`. Nova does not hand the template to plugins, so the plugin rebuilds it from the path and the bindings. The README lists the two cases where the text comes out wrong (a literal segment equal to a binding value, and two bindings with the same value); the number of distinct names stays bounded by the route table either way, which matters for the metrics below. A request that matches no route keeps the name `HTTP GET` and has no `http.route`.

### Whose trace context to trust

Three settings decide how much of the incoming request the spans believe.

**`trust_incoming_context`** (default `true`) continues a `traceparent` the client sends. That is what you want between your own services. The blog, though, faces the internet: any browser can send a `traceparent`, choose the trace id, force its request to be sampled, and attach `baggage` that would be passed to every service the request reaches. The production config turns it off:

```erlang
{opentelemetry_nova, [{trust_incoming_context, false}]},
```

With `false`, the server span starts a new trace and keeps the caller's span as a *link*, so a trace backend can still show where the request came from. The test config keeps the default, because the test for propagation needs it, and `untrusted_traceparent_starts_a_new_trace` checks the production setting.

**`trust_forwarded_for`** (default `false`) takes `client.address` from `X-Forwarded-For` or `Forwarded`. The blog leaves it off. Behind the reverse proxy from [Deployment](deployment.md), `client.address` is then the proxy's address. Turning it on is only safe when the proxy replaces these headers; many proxies (nginx with `$proxy_add_x_forwarded_for`, Traefik, ingress-nginx) append to `X-Forwarded-For`, so the first entry is still whatever the client sent.

**`metric_server_hosts`** (unset) is covered under [metrics](#metrics-for-prometheus).

### Leaving probes out

The load balancer calls `/heartbeat` and `/health` every few seconds. Each call would be a trace, and they would bury the requests you care about. `otel_nova_sampler` drops the server span of listed requests; the test and production configs use it:

```erlang
{opentelemetry, [
    {span_processor, batch},
    {traces_exporter, otlp},
    {sampler, {parent_based, #{root => {otel_nova_sampler, [~"GET /heartbeat", ~"GET /health"]}}}}
]},
```

The sampler decides when the stream handler starts the span, before Nova has routed the request, so an entry is the method and the raw request path, not a route template: `~"GET /posts/:id"` would not match `/posts/42`. Wrapped in `parent_based`, it only decides for requests that start a new trace; a request with a sampled `traceparent` follows its parent (unless `trust_incoming_context` is `false`, as in production). A dropped request still counts in the metrics, and its log lines still carry a trace id, with `otel_trace_flags` `"00"` (not sampled).

## Where traces go

The SDK is configured per environment:

```erlang
%% dev: build spans, export none
{opentelemetry, [{span_processor, batch}, {traces_exporter, none}]},

%% test: hand each span to the test process as soon as it ends
{opentelemetry, [
    {span_processor, simple},
    {traces_exporter, none},
    {sampler, {parent_based, #{root => {otel_nova_sampler, [~"GET /heartbeat", ~"GET /health"]}}}}
]},

%% prod: batch and send over OTLP
{opentelemetry, [
    {span_processor, batch},
    {traces_exporter, otlp},
    {sampler, {parent_based, #{root => {otel_nova_sampler, [~"GET /heartbeat", ~"GET /health"]}}}}
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
{"traces":[{"traceID":"12447de5323200ed1e67c3480dfd3a20","rootServiceName":"blog","rootTraceName":"GET /api/posts","startTimeUnixNano":"1794090061709389931","durationMs":4},...
```

Calls to `/health` and `/heartbeat` made in between do not show up: the sampler dropped them. Fetching a trace from Tempo (`/api/traces/<id>`) shows the request span and the two queries behind the post listing, each a child of the request span:

| Scope | Span | Kind | `db.statement` |
|---|---|---|---|
| opentelemetry_nova | `GET /api/posts` | server | |
| opentelemetry_kura | `SELECT posts` | client | `SELECT * FROM "posts" WHERE "status" = $1 AND "deleted_at" IS NULL ORDER BY "inserted_at" DESC, "id" DESC LIMIT $2 OFFSET $3` |
| opentelemetry_kura | `SELECT posts` | client | `SELECT count(*) AS "count" FROM "posts" WHERE "status" = $1 AND "deleted_at" IS NULL` |

The statements carry placeholders, not values; the spans only record how many parameters there were. That matters for the [Security](security.md#logging-hygiene) chapter: a trace never contains the email address someone logged in with.

## Metrics for Prometheus

opentelemetry_nova ships a metric exporter that renders the Prometheus text format (`otel_nova_prom_exporter`) and a small HTTP listener that serves it on `/metrics` (`otel_nova_prom_server`).

The metrics SDK's reader is configured in `opentelemetry_experimental` with that exporter:

```erlang
{opentelemetry_experimental, [
    {readers, [
        #{
            module => otel_metric_reader,
            config => #{export_interval_ms => 10000, exporter => {otel_nova_prom_exporter, #{}}}
        }
    ]}
]}
```

The listener is a child of `blog_sup`, so it stops with the application:

```erlang
metrics_server() ->
    case application:get_env(blog, metrics) of
        {ok, Opts} -> [otel_nova_prom_server:child_spec(Opts)];
        undefined -> []
    end.
```

`Opts` comes from `{blog, [{metrics, Opts}]}` and goes to the listener as is: `port` (default 9464) and `ip` (default `{127, 0, 0, 1}`). Dev uses `#{port => 9464}`, test `#{port => 0}` (any free port, so suites running at the same time never collide). The README also shows `opentelemetry_nova:setup(#{prometheus => #{port => 9464}})`, which links the listener to the caller instead; under a supervisor is the better place.

The metrics listener is separate from the blog's port on purpose: Prometheus scrapes it from inside your network, and the public load balancer never routes to it. `/metrics` has no authentication, which is why the listener binds loopback unless told otherwise. In a container, loopback is the container's own, and a Prometheus outside it gets connection refused. So production binds all interfaces:

```erlang
{metrics, #{port => ${METRICS_PORT:-9464}, ip => {0, 0, 0, 0}}},
```

and keeps the port private another way. [Deployment](deployment.md) publishes it on the host's loopback only (`-p 127.0.0.1:9464:9464`). In Kubernetes, add a `NetworkPolicy` that only admits the Prometheus pods to port 9464; the opentelemetry_nova README has one. If you run the release directly on a host instead of in a container, leave `ip` out.

Every 10 seconds the reader collects and the exporter refreshes the table that `/metrics` renders. After a few requests to the release:

```shell
curl -s localhost:9464/metrics | grep -v '^#' | head -4
```

```
http_server_active_requests{http_request_method="GET",url_scheme="http"} 0
http_server_request_duration_seconds_bucket{http_request_method="GET",http_response_status_code="200",http_route="/api/posts",url_scheme="http",le="0.005"} 5
http_server_request_duration_seconds_bucket{http_request_method="GET",http_response_status_code="200",http_route="/api/posts",url_scheme="http",le="0.01"} 5
http_server_request_duration_seconds_bucket{http_request_method="GET",http_response_status_code="200",http_route="/api/posts",url_scheme="http",le="0.025"} 6
```

The labels are the method, the status, the scheme and `http_route`, the route template from the plugin. Because the route is a template and not the raw path, `/posts/1` and `/posts/2` share one series, and you can graph one endpoint's latency without the number of series growing with your data. Methods outside the nine standard ones become `_OTHER`. Each metric keeps at most 2000 series; anything beyond goes into one series labelled `otel_metric_overflow="true"`.

`server_address` and `server_port` are not there. They come from the request's `Host` header, which the client chooses, so opentelemetry_nova only records them for hosts you list in `{opentelemetry_nova, [{metric_server_hosts, [...]}]}` (others become `_OTHER`). The blog has one host and does not set it.

## Logs that point at traces

When `otel_nova_plugin` attaches the context in the request process, the SDK puts `otel_trace_id`, `otel_span_id` and `otel_trace_flags` into the process's logger metadata, and the plugin adds the same ids as `trace_id` and `span_id`. Every `?LOG_*` call made while handling the request carries them, with no code in the controller.

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
{"line":20,"pid":"<0.1524.0>","status":200,"time":1794090061721075,"file":".../src/plugins/blog_request_logger.erl","path":"/api/posts","gl":"<0.1373.0>","msg":"request","level":"info","mfa":"blog_request_logger:post_request/4","method":"GET","span_id":"ea0de880798a649c","trace_id":"ecfce5117418849fe60bccea453d8d24","otel_span_id":"ea0de880798a649c","otel_trace_flags":"01","otel_trace_id":"ecfce5117418849fe60bccea453d8d24","duration_us":2864}
```

Search your trace backend for the `trace_id` and you have the spans of exactly that request. Log lines written before `otel_nova_plugin` runs (by a plugin listed ahead of it, or by Nova's router) have no trace id.

## Testing spans

The test config uses the `simple` span processor, which exports each span as soon as it ends. The SDK has an exporter that sends spans as messages to a pid, so a suite can point it at the test process and `receive` the spans:

```erlang
-include_lib("opentelemetry/include/otel_span.hrl").

-define(TRACE_ID, 16#4bf92f3577b34da6a3ce929d0e0e4736).
-define(TRACEPARENT, "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01").

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

flush() ->
    receive
        Message -> [Message | flush()]
    after 0 -> []
    end.
```

`blog_prod_telemetry_SUITE` checks the request span, the parent of the query spans and trace propagation:

```erlang
request_gets_a_server_span(_Config) ->
    {200, _} = blog_data_fixtures:request(get, "/api/posts"),
    #span{kind = server, attributes = Attributes} = receive_span(~"GET /api/posts"),
    #{
        'http.request.method' := ~"GET",
        'http.route' := ~"/api/posts",
        'url.path' := ~"/api/posts",
        'http.response.status_code' := 200,
        'nova.app' := ~"blog",
        'nova.controller' := ~"blog_posts_controller",
        'nova.action' := ~"index"
    } = otel_attributes:map(Attributes).

queries_are_children_of_the_request_span(_Config) ->
    {200, _} = blog_data_fixtures:request(get, "/api/posts"),
    Query = receive_span(~"SELECT posts"),
    Request = receive_span(~"GET /api/posts"),
    ?assertEqual(Request#span.trace_id, Query#span.trace_id),
    ?assertEqual(Request#span.span_id, Query#span.parent_span_id),
    #{'db.system' := ~"postgresql", 'db.statement' := Statement} =
        otel_attributes:map(Query#span.attributes),
    ?assertMatch({match, _}, re:run(Statement, ~"\\$1")).

traceparent_continues_the_callers_trace(_Config) ->
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        get, {blog_data_fixtures:url("/api/posts"), [{"traceparent", ?TRACEPARENT}]}, [], []
    ),
    #span{trace_id = TraceId, parent_span_id = Parent} = receive_span(~"GET /api/posts"),
    ?assertEqual(?TRACE_ID, TraceId),
    ?assertEqual(16#00f067aa0ba902b7, Parent).
```

The production value of `trust_incoming_context` is tested by setting it for one case. opentelemetry_nova reads it on every request, so `application:set_env/3` is enough:

```erlang
untrusted_traceparent_starts_a_new_trace(_Config) ->
    ok = application:set_env(opentelemetry_nova, trust_incoming_context, false),
    try
        {ok, {{_, 200, _}, _, _}} = httpc:request(
            get, {blog_data_fixtures:url("/api/posts"), [{"traceparent", ?TRACEPARENT}]}, [], []
        ),
        #span{trace_id = TraceId, parent_span_id = Parent, links = Links} =
            receive_span(~"GET /api/posts"),
        ?assertNotEqual(?TRACE_ID, TraceId),
        ?assertEqual(undefined, Parent),
        [#link{trace_id = ?TRACE_ID, span_id = 16#00f067aa0ba902b7}] = otel_links:list(Links)
    after
        application:unset_env(opentelemetry_nova, trust_incoming_context)
    end.
```

The sampler test makes a probe request and then a normal one, waits for the normal span and checks that no server span for `/heartbeat` arrived before it:

```erlang
probes_are_not_traced(_Config) ->
    {200, _} = blog_data_fixtures:request(get, "/heartbeat"),
    {200, _} = blog_data_fixtures:request(get, "/api/posts"),
    _ = receive_span(~"GET /api/posts"),
    Probes = [
        Span
     || {span, #span{kind = server, attributes = Attributes} = Span} <- flush(),
        maps:get('url.path', otel_attributes:map(Attributes)) =:= ~"/heartbeat"
    ],
    ?assertEqual([], Probes).
```

The metrics test reads the listener's address from Ranch. `otel_nova_prom_listener` is the listener's name inside opentelemetry_nova, not a documented API, but with port 0 there is no other way to learn the port. It also checks that the listener stayed on loopback:

```erlang
metrics_are_served_for_prometheus(_Config) ->
    {200, _} = blog_data_fixtures:request(get, "/api/posts"),
    ok = otel_meter_server:force_flush(),
    {{127, 0, 0, 1}, Port} = ranch:get_addr(otel_nova_prom_listener),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/metrics",
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, []}, [], [{body_format, binary}]),
    ?assertMatch(
        {match, _}, re:run(Body, ~"# TYPE http_server_request_duration_seconds histogram")
    ),
    ?assertMatch(
        {match, _},
        re:run(Body, ~"http_server_request_duration_seconds_count\\{[^}]*http_route=\"/api/posts\"")
    ).
```

The rest check that `/robots.txt` and a file under `/assets` (both `sendfile` replies) get spans named `GET /robots.txt` and `GET /assets/[...]`, that the `request` log line carries the caller's trace id as both `otel_trace_id` and `trace_id`, and that stopping `blog` stops the metrics listener and starting it brings the listener back.

```shell
rebar3 ct --suite=test/blog_prod_telemetry_SUITE
```

```
All 9 tests passed.
```

---

Next: [Custom Plugins & CORS](plugins-cors.md), including the request logger whose lines you saw above.
