-module(blog_prod_telemetry_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([log/2]).
-export([
    request_gets_a_server_span/1,
    queries_are_children_of_the_request_span/1,
    traceparent_continues_the_callers_trace/1,
    untrusted_traceparent_starts_a_new_trace/1,
    request_logs_carry_the_trace_id/1,
    static_files_get_a_span/1,
    probes_are_not_traced/1,
    metrics_are_served_for_prometheus/1,
    metrics_listener_stops_with_the_application/1
]).

-define(TRACE_ID, 16#4bf92f3577b34da6a3ce929d0e0e4736).
-define(TRACEPARENT, "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01").

all() ->
    [
        request_gets_a_server_span,
        queries_are_children_of_the_request_span,
        traceparent_continues_the_callers_trace,
        untrusted_traceparent_starts_a_new_trace,
        request_logs_carry_the_trace_id,
        static_files_get_a_span,
        probes_are_not_traced,
        metrics_are_served_for_prometheus,
        metrics_listener_stops_with_the_application
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    ok = otel_simple_processor:set_exporter(otel_exporter_pid, self()),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = otel_simple_processor:set_exporter(none, []),
    blog_data_fixtures:checkin().

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

request_logs_carry_the_trace_id(_Config) ->
    #{level := Level} = logger:get_primary_config(),
    ok = logger:add_handler(?MODULE, ?MODULE, #{config => self()}),
    ok = logger:set_primary_config(level, info),
    try
        {ok, {{_, 200, _}, _, _}} = httpc:request(
            get, {blog_data_fixtures:url("/api/posts"), [{"traceparent", ?TRACEPARENT}]}, [], []
        ),
        receive
            {log, #{msg := {report, #{msg := ~"request"}}, meta := Meta}} ->
                #{
                    otel_trace_id := ~"4bf92f3577b34da6a3ce929d0e0e4736",
                    trace_id := ~"4bf92f3577b34da6a3ce929d0e0e4736",
                    otel_span_id := SpanId,
                    span_id := SpanId
                } = Meta
        after 5000 -> ct:fail(no_request_log)
        end
    after
        ok = logger:set_primary_config(level, Level),
        ok = logger:remove_handler(?MODULE)
    end.

static_files_get_a_span(_Config) ->
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        get, {blog_data_fixtures:url("/robots.txt"), []}, [], []
    ),
    #span{attributes = Robots} = receive_span(~"GET /robots.txt"),
    #{'url.path' := ~"/robots.txt", 'http.response.status_code' := 200} =
        otel_attributes:map(Robots),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        get, {blog_data_fixtures:url("/assets/js/notifications.js"), []}, [], []
    ),
    #span{attributes = Asset} = receive_span(~"GET /assets/[...]"),
    #{'url.path' := ~"/assets/js/notifications.js", 'http.route' := ~"/assets/[...]"} =
        otel_attributes:map(Asset).

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

metrics_listener_stops_with_the_application(_Config) ->
    blog_data_fixtures:checkin(),
    ok = application:stop(blog),
    false = maps:is_key(otel_nova_prom_listener, ranch:info()),
    {ok, _} = application:ensure_all_started(blog),
    true = maps:is_key(otel_nova_prom_listener, ranch:info()),
    blog_data_fixtures:checkout().

log(Event, #{config := Pid}) ->
    Pid ! {log, Event}.

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
