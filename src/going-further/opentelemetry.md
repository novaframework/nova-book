# OpenTelemetry

When your Nova application is in production, you need visibility into what it is doing. [OpenTelemetry](https://opentelemetry.io/) is the industry standard for collecting traces and metrics. The [opentelemetry_nova](https://github.com/novaframework/opentelemetry_nova) library gives you automatic instrumentation — every HTTP request gets a trace span and metrics are recorded without manual instrumentation code.

## What you get

Once configured, `opentelemetry_nova` provides:

**Distributed traces** — Every incoming request creates a span with attributes like method, path, status code, controller, and action. If the caller sends a W3C `traceparent` header, the span is linked to the upstream trace.

**HTTP metrics** — Four metrics recorded for every request:

| Metric | Type | Description |
|---|---|---|
| `http.server.request.duration` | Histogram | Request duration in seconds |
| `http.server.active_requests` | Gauge | Number of in-flight requests |
| `http.server.request.body.size` | Histogram | Request body size in bytes |
| `http.server.response.body.size` | Histogram | Response body size in bytes |

## Adding the dependency

Add `opentelemetry_nova` and the OpenTelemetry SDK to `rebar.config`:

```erlang
{deps, [
    nova,
    {opentelemetry, "~> 1.5"},
    {opentelemetry_experimental, "~> 0.5"},
    {opentelemetry_exporter, "~> 1.8"},
    opentelemetry_nova
]}.
```

## Configuring the stream handler

`opentelemetry_nova` uses a Cowboy stream handler to intercept requests. Add `otel_nova_stream_h` to the Nova cowboy configuration:

```erlang
{nova, [
    {cowboy_configuration, #{
        port => 8080,
        stream_handlers => [otel_nova_stream_h, cowboy_stream_h]
    }}
]}
```

```admonish warning
The order matters — `otel_nova_stream_h` must come **before** `cowboy_stream_h` to wrap the full request lifecycle.
```

## Setting up tracing

Configure the SDK to export traces via OTLP HTTP:

```erlang
{opentelemetry, [
    {span_processor, batch},
    {traces_exporter, {opentelemetry_exporter, #{
        protocol => http_protobuf,
        endpoints => [#{host => "localhost", port => 4318, path => "/v1/traces"}]
    }}}
]},

{opentelemetry_exporter, [
    {otlp_protocol, http_protobuf},
    {otlp_endpoint, "http://localhost:4318"}
]}
```

This sends traces to any OTLP-compatible backend — Grafana Tempo, Jaeger, or any OpenTelemetry Collector.

## Setting up Prometheus metrics

Configure a metric reader with the Prometheus exporter:

```erlang
{opentelemetry_experimental, [
    {readers, [
        #{module => otel_metric_reader,
          config => #{
              export_interval_ms => 5000,
              exporter => {otel_nova_prom_exporter, #{}}
          }}
    ]}
]}
```

In your application's `start/2`, initialize metrics and start the Prometheus HTTP server:

```erlang
start(_StartType, _StartArgs) ->
    opentelemetry_nova:setup(#{prometheus => #{port => 9464}}),
    my_app_sup:start_link().
```

This starts a Prometheus endpoint at `http://localhost:9464/metrics`. Point your Prometheus server or Grafana Agent at it.

```admonish tip
If you only want metrics without the Prometheus HTTP server (e.g., pushing via OTLP instead), call `opentelemetry_nova:setup()` with no arguments.
```

## Span enrichment with the Nova plugin

The stream handler creates spans with basic HTTP attributes. To also get the controller and action on each span, add the `otel_nova_plugin` as a pre-request plugin:

```erlang
routes(_Environment) ->
    [#{
        plugins => [{pre_request, otel_nova_plugin, #{}}],
        routes => [
            {"/hello", fun my_controller:hello/1, #{methods => [get]}},
            {"/users", fun my_controller:users/1, #{methods => [get, post]}}
        ]
    }].
```

Spans get enriched with `nova.app`, `nova.controller`, and `nova.action` attributes, and the span name becomes `GET my_controller:hello` instead of just `HTTP GET`.

## Full sys.config example

```erlang
[
  {nova, [
      {cowboy_configuration, #{
          port => 8080,
          stream_handlers => [otel_nova_stream_h, cowboy_stream_h]
      }}
  ]},

  {opentelemetry, [
      {span_processor, batch},
      {traces_exporter, {opentelemetry_exporter, #{
          protocol => http_protobuf,
          endpoints => [#{host => "localhost", port => 4318, path => "/v1/traces"}]
      }}}
  ]},

  {opentelemetry_experimental, [
      {readers, [
          #{module => otel_metric_reader,
            config => #{
                export_interval_ms => 5000,
                exporter => {otel_nova_prom_exporter, #{}}
            }}
      ]}
  ]},

  {opentelemetry_exporter, [
      {otlp_protocol, http_protobuf},
      {otlp_endpoint, "http://localhost:4318"}
  ]}
].
```

## Verifying it works

Make some requests:

```shell
curl http://localhost:8080/hello
curl -X POST -d '{"name":"nova"}' http://localhost:8080/users
```

Check the Prometheus endpoint:

```shell
curl http://localhost:9464/metrics
```

You should see output like:

```
# HELP http_server_request_duration_seconds Duration of HTTP server requests
# TYPE http_server_request_duration_seconds histogram
http_server_request_duration_seconds_bucket{method="GET",...,le="0.005"} 1
...
```

For traces, check your configured backend (Tempo, Jaeger, etc.).

## How it works under the hood

The `otel_nova_stream_h` stream handler sits in Cowboy's stream pipeline. When a request arrives it:

1. Extracts trace context from the `traceparent` header
2. Creates a server span named `HTTP <method>`
3. Sets request attributes (method, path, scheme, host, port, peer address, user agent)
4. Increments the active requests counter

When the request terminates it:

1. Sets the response status code attribute
2. Marks the span as error if status >= 500
3. Ends the span
4. Records duration, request body size, and response body size metrics
5. Decrements the active requests counter

## Running with a full observability stack

The [nova_otel_demo](https://github.com/novaframework/nova_otel_demo) repository has a complete example with Docker Compose including:

- **OpenTelemetry Collector** — receives traces and metrics via OTLP
- **Grafana Tempo** — stores and queries traces
- **Grafana Mimir** — stores Prometheus metrics
- **Grafana** — dashboards and trace exploration

Clone it and run `docker-compose up` from the `docker/` directory.

---

That wraps up the main content. For quick reference, see the [Erlang Essentials](../appendix/erlang-essentials.md) appendix and the [Cheat Sheet](../appendix/cheat-sheet.md).
