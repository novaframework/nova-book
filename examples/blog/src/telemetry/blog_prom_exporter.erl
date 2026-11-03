-module(blog_prom_exporter).
-behaviour(otel_exporter_metrics).

-export([init/1, export/3, shutdown/1]).

init(Config) ->
    otel_nova_prom_exporter:init(Config).

export(Metrics, Resource, State) ->
    otel_nova_prom_exporter:export(metrics, Metrics, Resource, State).

shutdown(_State) ->
    otel_nova_prom_exporter:shutdown().
