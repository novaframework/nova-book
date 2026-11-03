-module(blog_api_errors_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    crash_is_json_500/1,
    crash_is_html_500_for_browsers/1,
    render_error_pages_false_gives_bare_status/1,
    bare_status_return_skips_status_handler/1,
    unexpected_error_reaches_fallback/1,
    unguarded_schema_route_crashes_without_json/1,
    unknown_api_path_is_json_404/1,
    plugin_crash_uses_500_handler/1,
    security_crash_skips_500_handler/1
]).

all() ->
    [
        crash_is_json_500,
        crash_is_html_500_for_browsers,
        render_error_pages_false_gives_bare_status,
        bare_status_return_skips_status_handler,
        unexpected_error_reaches_fallback,
        unguarded_schema_route_crashes_without_json,
        unknown_api_path_is_json_404,
        plugin_crash_uses_500_handler,
        security_crash_skips_500_handler
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    ok = nova_router:add_routes(blog, #{
        prefix => "/errors",
        security => false,
        plugins => [
            {pre_request, nova_request_plugin, #{decode_json_body => true}},
            {pre_request, nova_json_schemas, #{render_errors => false}}
        ],
        routes => [
            {"/crash/:id", fun blog_api_errors_test_controller:crash/1, #{methods => [get]}},
            {"/bare", fun blog_api_errors_test_controller:bare_status/1, #{methods => [get]}},
            {"/unexpected", fun blog_api_errors_test_controller:unexpected/1, #{methods => [get]}},
            {"/echo", fun blog_api_errors_test_controller:echo/1, #{
                methods => [post], extra_state => #{json_schema => "schemas/tag.json"}
            }}
        ]
    }),
    ok = nova_router:add_routes(blog, [
        #{
            prefix => "/errors/plugin",
            security => false,
            plugins => [{pre_request, blog_api_errors_test_plugin, #{}}],
            routes => [
                {"/", fun blog_api_errors_test_controller:bare_status/1, #{methods => [get]}}
            ]
        },
        #{
            prefix => "/errors/security",
            security => fun blog_api_errors_test_controller:crashing_security/1,
            routes => [
                {"/", fun blog_api_errors_test_controller:bare_status/1, #{methods => [get]}}
            ]
        }
    ]),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

crash_is_json_500(_Config) ->
    {500, Headers, Body} = get("/errors/crash/abc", "application/json"),
    "application/json" = proplists:get_value("content-type", Headers),
    #{~"error" := ~"internal_error"} = json:decode(Body).

crash_is_html_500_for_browsers(_Config) ->
    {500, Headers, Body} = get("/errors/crash/abc", "text/html"),
    "text/html" ++ _ = proplists:get_value("content-type", Headers),
    {_, _} = binary:match(Body, ~"Something went wrong").

render_error_pages_false_gives_bare_status(_Config) ->
    ok = application:set_env(nova, render_error_pages, false),
    try
        {500, _, <<>>} = get("/errors/crash/abc", "application/json")
    after
        application:unset_env(nova, render_error_pages)
    end.

bare_status_return_skips_status_handler(_Config) ->
    {404, _, <<>>} = get("/errors/bare", "application/json").

unexpected_error_reaches_fallback(_Config) ->
    {500, _, Body} = get("/errors/unexpected", "application/json"),
    #{~"error" := ~"internal_error"} = json:decode(Body).

unguarded_schema_route_crashes_without_json(_Config) ->
    {ok, {{_, 500, _}, _, <<>>}} =
        httpc:request(
            post,
            {blog_data_fixtures:url("/errors/echo"), [], "text/plain", ~"otp"},
            [],
            [{body_format, binary}]
        ).

unknown_api_path_is_json_404(_Config) ->
    {404, _, Body} = get("/api/nothing-here", "application/json"),
    #{~"error" := ~"not_found"} = json:decode(Body).

plugin_crash_uses_500_handler(_Config) ->
    {500, _, Body} = get("/errors/plugin", "application/json"),
    #{~"error" := ~"internal_error"} = json:decode(Body).

security_crash_skips_500_handler(_Config) ->
    {500, _, <<>>} = get("/errors/security", "application/json").

get(Path, Accept) ->
    {ok, {{_, Status, _}, Headers, Body}} =
        httpc:request(get, {blog_data_fixtures:url(Path), [{"accept", Accept}]}, [], [
            {body_format, binary}
        ]),
    {Status, Headers, Body}.
