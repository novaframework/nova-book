-module(blog_prod_telemetry_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([init/2]).
-export([
    request_gets_a_server_span/1,
    queries_are_children_of_the_request_span/1,
    traceparent_continues_the_callers_trace/1,
    stream_handler_context_stays_in_the_connection_process/1,
    nova_stream_h_fails_on_sendfile/1,
    static_files_get_a_span/1,
    metrics_are_served_for_prometheus/1,
    nova_prom_exporter_misses_export_3/1,
    metrics_listener_survives_an_app_restart/1
]).

-define(TRACE_ID, 16#4bf92f3577b34da6a3ce929d0e0e4736).

all() ->
    [
        request_gets_a_server_span,
        queries_are_children_of_the_request_span,
        traceparent_continues_the_callers_trace,
        stream_handler_context_stays_in_the_connection_process,
        nova_stream_h_fails_on_sendfile,
        static_files_get_a_span,
        metrics_are_served_for_prometheus,
        nova_prom_exporter_misses_export_3,
        metrics_listener_survives_an_app_restart
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

stream_handler_context_stays_in_the_connection_process(_Config) ->
    Url = bare_listener(),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, []}, [], []),
    ok = cowboy:stop_listener(?MODULE),
    receive
        {current_span, SpanCtx} -> ?assertEqual(undefined, SpanCtx)
    after 5000 -> ct:fail(handler_not_called)
    end,
    #span{name = ~"HTTP GET"} = receive_span(~"HTTP GET").

nova_stream_h_fails_on_sendfile(_Config) ->
    Url = bare_listener(),
    {ok, {{_, Status, _}, _, _}} = httpc:request(get, {Url ++ "file", []}, [], []),
    ok = cowboy:stop_listener(?MODULE),
    ?assertEqual(500, Status).

static_files_get_a_span(_Config) ->
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        get, {blog_data_fixtures:url("/robots.txt"), []}, [], []
    ),
    #span{attributes = Attributes} = receive_span(~"GET nova_file_controller:get_file"),
    #{'url.path' := ~"/robots.txt", 'http.response.status_code' := 200} =
        otel_attributes:map(Attributes).

metrics_are_served_for_prometheus(_Config) ->
    {200, _} = blog_data_fixtures:request(get, "/api/posts"),
    ok = otel_meter_server:force_flush(),
    Port = blog_metrics:port(),
    Url = "http://localhost:" ++ integer_to_list(Port) ++ "/metrics",
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, []}, [], [{body_format, binary}]),
    ?assertMatch(
        {match, _}, re:run(Body, ~"# TYPE http_server_request_duration_seconds histogram")
    ),
    ?assertMatch({match, _}, re:run(Body, ~"http_server_request_duration_seconds_count\\{")).

nova_prom_exporter_misses_export_3(_Config) ->
    {module, _} = code:ensure_loaded(otel_nova_prom_exporter),
    false = erlang:function_exported(otel_nova_prom_exporter, export, 3),
    true = erlang:function_exported(otel_nova_prom_exporter, export, 4).

metrics_listener_survives_an_app_restart(_Config) ->
    blog_data_fixtures:checkin(),
    ok = application:stop(blog),
    {ok, _} = application:ensure_all_started(blog),
    true = is_integer(blog_metrics:port()),
    blog_data_fixtures:checkout().

init(#{path := ~"/file"} = Req, Parent) ->
    File = code:which(?MODULE),
    Body = {sendfile, 0, filelib:file_size(File), File},
    {ok, cowboy_req:reply(200, #{}, Body, Req), Parent};
init(Req, Parent) ->
    Parent ! {current_span, otel_tracer:current_span_ctx()},
    {ok, cowboy_req:reply(204, Req), Parent}.

bare_listener() ->
    Routes = [{"/", ?MODULE, self()}, {"/file", ?MODULE, self()}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_clear(?MODULE, [{port, 0}], #{
        env => #{dispatch => Dispatch},
        stream_handlers => [otel_nova_stream_h, cowboy_stream_h]
    }),
    "http://localhost:" ++ integer_to_list(ranch:get_port(?MODULE)) ++ "/".

receive_span(Name) ->
    receive
        {span, #span{name = Name} = Span} -> Span
    after 5000 -> ct:fail({no_span, Name})
    end.
