-module(blog_prod_plugins_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([log/2]).
-export([
    request_logger_writes_one_report/1,
    request_logger_runs_only_where_listed/1,
    request_logger_misses_security_refusals/1,
    cors_preflight_from_an_allowed_origin/1,
    cors_preflight_from_a_foreign_origin/1,
    cors_simple_request_echoes_the_origin/1,
    nova_cors_plugin_allows_everything/1,
    break_from_a_plugin_crashes_the_request/1
]).

-define(ORIGIN, "https://app.example").

all() ->
    [
        request_logger_writes_one_report,
        request_logger_runs_only_where_listed,
        request_logger_misses_security_refusals,
        cors_preflight_from_an_allowed_origin,
        cors_preflight_from_a_foreign_origin,
        cors_simple_request_echoes_the_origin,
        nova_cors_plugin_allows_everything,
        break_from_a_plugin_crashes_the_request
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    ok = nova_router:add_routes(blog, #{
        prefix => "/test-break",
        security => false,
        plugins => [{pre_request, blog_prod_break_plugin, #{}}],
        routes => [{"/", fun blog_pages_controller:version/1, #{methods => [get]}}]
    }),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    #{level := Level} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, info),
    ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{pid => self()}}),
    [{primary_level, Level} | Config].

end_per_testcase(_Case, Config) ->
    ok = logger:remove_handler(?MODULE),
    ok = logger:set_primary_config(level, ?config(primary_level, Config)),
    blog_data_fixtures:checkin().

log(#{msg := {report, #{msg := ~"request"} = Report}}, #{config := #{pid := Pid}}) ->
    Pid ! {request_log, Report};
log(_Event, _Config) ->
    ok.

request_logger_writes_one_report(_Config) ->
    #{id := UserId} = User = blog_data_fixtures:user(~"alice"),
    Token = blog_data_fixtures:token(User),
    Headers = [
        {"authorization", "Bearer " ++ binary_to_list(Token)},
        {"cookie", "session_id=not-for-logs"}
    ],
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        get, {blog_data_fixtures:url("/api/posts?api_key=hunter2"), Headers}, [], []
    ),
    Report = receive_report(),
    #{
        method := ~"GET",
        path := ~"/api/posts",
        status := 200,
        user_id := UserId,
        duration_us := Duration
    } = Report,
    ?assert(is_integer(Duration)),
    Logged = iolist_to_binary(io_lib:format("~p", [Report])),
    [nomatch = binary:match(Logged, Secret) || Secret <- [Token, ~"not-for-logs", ~"hunter2"]].

request_logger_runs_only_where_listed(_Config) ->
    {200, _} = get_with_origin("/about", ?ORIGIN),
    #{path := ~"/about"} = receive_report(),
    {200, _} = get_with_origin("/api/tags", ?ORIGIN),
    receive
        {request_log, Unexpected} -> ct:fail({logged, Unexpected})
    after 500 -> ok
    end.

request_logger_misses_security_refusals(_Config) ->
    {401, _} = get_with_origin("/ops/status", ?ORIGIN),
    receive
        {request_log, Unexpected} -> ct:fail({logged, Unexpected})
    after 500 -> ok
    end.

cors_preflight_from_an_allowed_origin(_Config) ->
    {204, Headers} = options("/api/posts", ?ORIGIN),
    ?ORIGIN = proplists:get_value("access-control-allow-origin", Headers),
    "GET" = proplists:get_value("access-control-allow-methods", Headers),
    "authorization" = proplists:get_value("access-control-allow-headers", Headers),
    "600" = proplists:get_value("access-control-max-age", Headers),
    ?assertMatch({match, _}, re:run(proplists:get_value("vary", Headers), ~"^origin")).

cors_preflight_from_a_foreign_origin(_Config) ->
    {403, Headers} = options("/api/posts", "https://evil.example"),
    undefined = proplists:get_value("access-control-allow-origin", Headers).

cors_simple_request_echoes_the_origin(_Config) ->
    {200, Allowed} = get_with_origin("/api/posts", ?ORIGIN),
    ?ORIGIN = proplists:get_value("access-control-allow-origin", Allowed),
    ?assertMatch({match, _}, re:run(proplists:get_value("vary", Allowed), ~"^origin")),
    {200, Foreign} = get_with_origin("/api/posts", "https://evil.example"),
    undefined = proplists:get_value("access-control-allow-origin", Foreign).

nova_cors_plugin_allows_everything(_Config) ->
    {ok, Socket} = gen_tcp:connect("localhost", port(), [binary, {active, false}]),
    ok = gen_tcp:send(Socket, [
        ~"OPTIONS /hello/ada HTTP/1.1\r\nhost: localhost\r\n",
        ~"origin: https://evil.example\r\nconnection: close\r\n\r\n"
    ]),
    Response = recv_all(Socket, <<>>),
    {match, _} = re:run(Response, ~"^HTTP/1.1 200 OK"),
    {match, _} = re:run(Response, ~"\r\nAccess-Control-Allow-Origin: \\*\r\n"),
    {match, _} = re:run(Response, ~"\r\nAccess-Control-Allow-Headers: \\*\r\n"),
    nomatch = re:run(Response, ~"(?i)\r\nvary:[^\r]*origin").

break_from_a_plugin_crashes_the_request(_Config) ->
    {500, _} = blog_data_fixtures:request(get, "/test-break/").

receive_report() ->
    receive
        {request_log, Report} -> Report
    after 2000 -> ct:fail(no_request_log)
    end.

options(Path, Origin) ->
    Headers = [{"origin", Origin}, {"access-control-request-method", "GET"}],
    {ok, {{_, Status, _}, RespHeaders, _}} =
        httpc:request(options, {blog_data_fixtures:url(Path), Headers}, [], []),
    {Status, RespHeaders}.

get_with_origin(Path, Origin) ->
    {ok, {{_, Status, _}, RespHeaders, _}} =
        httpc:request(get, {blog_data_fixtures:url(Path), [{"origin", Origin}]}, [], []),
    {Status, RespHeaders}.

port() ->
    {ok, #{port := Port}} = application:get_env(nova, cowboy_configuration),
    Port.

recv_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} -> recv_all(Socket, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.
