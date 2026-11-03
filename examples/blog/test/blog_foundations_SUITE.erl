-module(blog_foundations_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([
    index_renders_welcome_page/1,
    heartbeat_returns_200/1,
    about_renders_template/1,
    trailing_slash_is_ignored/1,
    old_about_redirects/1,
    hello_binds_name/1,
    hello_reads_query_string/1,
    hello_rejects_invalid_name_via_fallback/1,
    wrong_method_returns_405/1,
    unknown_path_returns_json_404/1,
    unknown_path_returns_html_404/1,
    static_file_is_served/1,
    static_directory_is_served/1,
    ops_without_token_is_401/1,
    ops_with_wrong_token_is_401/1,
    ops_with_token_passes_auth_data/1,
    about_markdown_is_sent_as_file/1,
    custom_text_handler/1,
    create_from_json_returns_201/1,
    create_from_form_returns_201/1,
    create_without_name_returns_400/1,
    create_with_non_binary_name_returns_422/1,
    malformed_json_returns_400/1,
    empty_json_body_returns_400/1,
    json_with_req_sets_cookie/1,
    thrown_status_renders_404/1,
    unsupported_return_is_500/1,
    request_map_keys/1,
    bindings_are_raw_path_segments/1,
    correlation_id_is_echoed/1,
    correlation_id_is_generated/1,
    cors_preflight_is_answered/1,
    cors_header_on_get/1,
    contact_form_has_csrf_token/1,
    contact_post_without_token_is_403/1,
    contact_post_with_token_succeeds/1,
    contact_post_escapes_output/1,
    contact_post_missing_message_is_422/1,
    contact_attachment_too_large_is_413/1
]).

all() ->
    [{group, routing}, {group, controllers}, {group, plugins}].

groups() ->
    [
        {routing, [], [
            index_renders_welcome_page,
            heartbeat_returns_200,
            about_renders_template,
            trailing_slash_is_ignored,
            old_about_redirects,
            hello_binds_name,
            hello_reads_query_string,
            hello_rejects_invalid_name_via_fallback,
            wrong_method_returns_405,
            unknown_path_returns_json_404,
            unknown_path_returns_html_404,
            static_file_is_served,
            static_directory_is_served,
            ops_without_token_is_401,
            ops_with_wrong_token_is_401,
            ops_with_token_passes_auth_data
        ]},
        {controllers, [], [
            about_markdown_is_sent_as_file,
            custom_text_handler,
            create_from_json_returns_201,
            create_from_form_returns_201,
            create_without_name_returns_400,
            create_with_non_binary_name_returns_422,
            malformed_json_returns_400,
            empty_json_body_returns_400,
            json_with_req_sets_cookie,
            thrown_status_renders_404,
            unsupported_return_is_500,
            request_map_keys,
            bindings_are_raw_path_segments
        ]},
        {plugins, [], [
            correlation_id_is_echoed,
            correlation_id_is_generated,
            cors_preflight_is_answered,
            cors_header_on_get,
            contact_form_has_csrf_token,
            contact_post_without_token_is_403,
            contact_post_with_token_succeeds,
            contact_post_escapes_output,
            contact_post_missing_message_is_422,
            contact_attachment_too_large_is_413
        ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(blog),
    {ok, _} = application:ensure_all_started(inets),
    ok = nova_router:add_routes(blog, #{
        prefix => "/reference",
        security => false,
        routes => [
            {"/remember", fun blog_reference_controller:remember/1, #{methods => [get]}},
            {"/missing", fun blog_reference_controller:missing/1, #{methods => [get]}},
            {"/broken", fun blog_reference_controller:broken/1, #{methods => [get]}},
            {"/keys/:id", fun blog_reference_controller:keys/1, #{methods => [get]}}
        ]
    }),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

%% Routing

index_renders_welcome_page(_Config) ->
    {200, Headers, Body} = http_get("/"),
    ?assertEqual("text/html", header("content-type", Headers)),
    ?assertNotEqual(nomatch, string:find(Body, "Hello world!")).

heartbeat_returns_200(_Config) ->
    {200, _, ""} = http_get("/heartbeat").

about_renders_template(_Config) ->
    {200, Headers, Body} = http_get("/about"),
    ?assertEqual("text/html", header("content-type", Headers)),
    ?assertNotEqual(nomatch, string:find(Body, "<h1>About this blog</h1>")).

trailing_slash_is_ignored(_Config) ->
    {200, _, _} = http_get("/about/").

old_about_redirects(_Config) ->
    {302, Headers, _} = http_get("/about-us"),
    ?assertEqual("/about", header("location", Headers)).

hello_binds_name(_Config) ->
    {200, Headers, Body} = http_get("/hello/nova"),
    ?assertEqual("application/json", header("content-type", Headers)),
    ?assertEqual(#{~"greeting" => ~"Hello, nova!"}, decode(Body)).

hello_reads_query_string(_Config) ->
    {200, _, Body} = http_get("/hello/nova?shout=true"),
    ?assertEqual(#{~"greeting" => ~"HELLO, NOVA!"}, decode(Body)).

hello_rejects_invalid_name_via_fallback(_Config) ->
    {422, _, Body} = http_get("/hello/J%C3%B6rgen"),
    ?assertEqual(#{~"error" => ~"invalid_name"}, decode(Body)).

wrong_method_returns_405(_Config) ->
    {405, Headers, _} = request(delete, "/hello/nova", []),
    ?assertEqual("GET, OPTIONS", header("allow", Headers)).

unknown_path_returns_json_404(_Config) ->
    {404, Headers, Body} = http_get("/no/such/page", [{"accept", "application/json"}]),
    ?assertEqual("application/json", header("content-type", Headers)),
    ?assertEqual(#{~"error" => ~"not_found"}, decode(Body)).

unknown_path_returns_html_404(_Config) ->
    {404, Headers, Body} = http_get("/no/such/page", [{"accept", "text/html"}]),
    ?assertEqual("text/html", header("content-type", Headers)),
    ?assertNotEqual(nomatch, string:find(Body, "<code>/no/such/page</code>")).

static_file_is_served(_Config) ->
    {200, Headers, Body} = http_get("/robots.txt"),
    ?assertEqual("text/plain", header("content-type", Headers)),
    ?assertNotEqual(nomatch, string:find(Body, "Disallow: /ops/")).

static_directory_is_served(_Config) ->
    {200, Headers, _} = http_get("/assets/favicon.ico"),
    ?assertEqual("image/x-icon", header("content-type", Headers)).

ops_without_token_is_401(_Config) ->
    {401, Headers, "unauthorized\n"} = http_get("/ops/status"),
    ?assertEqual("X-Ops-Token", header("www-authenticate", Headers)).

ops_with_wrong_token_is_401(_Config) ->
    {401, _, _} = http_get("/ops/status", [{"x-ops-token", "guess"}]).

ops_with_token_passes_auth_data(_Config) ->
    {200, _, Body} = http_get("/ops/status", [{"x-ops-token", "test-ops-token"}]),
    ?assertMatch(#{~"status" := ~"ok", ~"role" := ~"ops"}, decode(Body)).

%% Controllers

about_markdown_is_sent_as_file(_Config) ->
    {200, Headers, Body} = http_get("/about.md"),
    ?assertEqual("text/markdown; charset=utf-8", header("content-type", Headers)),
    ?assertMatch("# About this blog" ++ _, Body).

custom_text_handler(_Config) ->
    {200, Headers, "blog " ++ _} = http_get("/version"),
    ?assertEqual("text/plain; charset=utf-8", header("content-type", Headers)).

create_from_json_returns_201(_Config) ->
    {201, Headers, Body} = http_post("/hello", "application/json", ~"{\"name\":\"ada\"}"),
    ?assertEqual("/hello/ada", header("location", Headers)),
    ?assertEqual(#{~"name" => ~"ada"}, decode(Body)).

create_from_form_returns_201(_Config) ->
    {201, Headers, _} = http_post("/hello", "application/x-www-form-urlencoded", ~"name=grace"),
    ?assertEqual("/hello/grace", header("location", Headers)).

create_without_name_returns_400(_Config) ->
    {400, _, Body} = http_post("/hello", "application/json", ~"{\"title\":\"x\"}"),
    ?assertEqual(#{~"error" => ~"missing_name"}, decode(Body)).

create_with_non_binary_name_returns_422(_Config) ->
    {422, _, _} = http_post("/hello", "application/json", ~"{\"name\":42}").

malformed_json_returns_400(_Config) ->
    {400, _, ""} = http_post("/hello", "application/json", ~"{\"name\":").

empty_json_body_returns_400(_Config) ->
    {400, _, ""} = http_post("/hello", "application/json", ~"").

json_with_req_sets_cookie(_Config) ->
    {200, Headers, Body} = http_get("/reference/remember"),
    ?assertEqual(#{~"remembered" => true}, decode(Body)),
    Cookies = [V || {"set-cookie", V} <- Headers],
    ?assert(lists:any(fun(C) -> lists:prefix("seen=1", C) end, Cookies)).

thrown_status_renders_404(_Config) ->
    {404, _, Body} = http_get("/reference/missing", [{"accept", "application/json"}]),
    ?assertEqual(#{~"error" => ~"not_found"}, decode(Body)).

unsupported_return_is_500(_Config) ->
    {500, _, _} = http_get("/reference/broken").

request_map_keys(_Config) ->
    {200, _, Body} = http_get("/reference/keys/42?page=2"),
    #{~"keys" := Keys} = decode(Body),
    Expected = [
        ~"bindings",
        ~"body",
        ~"headers",
        ~"host",
        ~"method",
        ~"path",
        ~"peer",
        ~"port",
        ~"qs",
        ~"scheme"
    ],
    ?assertEqual([], Expected -- Keys).

bindings_are_raw_path_segments(_Config) ->
    {200, _, Body} = http_get("/reference/keys/J%C3%B6rgen"),
    ?assertMatch(#{~"bindings" := #{~"id" := ~"J%C3%B6rgen"}}, decode(Body)),
    ?assertEqual(~"Jörgen", uri_string:percent_decode(~"J%C3%B6rgen")).

%% Plugins

correlation_id_is_echoed(_Config) ->
    {200, Headers, _} = http_get("/hello/nova", [{"x-correlation-id", "abc-123"}]),
    ?assertEqual("abc-123", header("x-correlation-id", Headers)).

correlation_id_is_generated(_Config) ->
    {200, Headers, _} = http_get("/hello/nova"),
    ?assertEqual(36, length(header("x-correlation-id", Headers))).

cors_preflight_is_answered(_Config) ->
    {200, Headers, _} = request(options, "/hello/nova", [{"origin", "https://example.com"}]),
    ?assertEqual("*", header("access-control-allow-origin", Headers)).

cors_header_on_get(_Config) ->
    {200, Headers, _} = http_get("/hello/nova", [{"origin", "https://example.com"}]),
    ?assertEqual("*", header("access-control-allow-origin", Headers)).

contact_form_has_csrf_token(_Config) ->
    {Cookie, Token} = contact_session(),
    ?assertMatch("session_id=" ++ _, Cookie),
    ?assertEqual(44, length(Token)).

contact_post_without_token_is_403(_Config) ->
    {Cookie, _Token} = contact_session(),
    Body = multipart([{field, "name", "Ada"}, {field, "message", "Hi"}]),
    {403, _, "Forbidden - CSRF token invalid"} = post_multipart("/contact", Cookie, Body).

contact_post_with_token_succeeds(_Config) ->
    {Cookie, Token} = contact_session(),
    Body = multipart([
        {field, "_csrf_token", Token},
        {field, "name", "Ada"},
        {field, "message", "Hello there"},
        {file, "attachment", "notes.txt", "text/plain", "twelve bytes"}
    ]),
    {200, _, Html} = post_multipart("/contact", Cookie, Body),
    ?assertNotEqual(nomatch, string:find(Html, "Thanks, Ada!")),
    ?assertNotEqual(nomatch, string:find(Html, "Received notes.txt (12 bytes).")).

contact_post_escapes_output(_Config) ->
    {Cookie, Token} = contact_session(),
    Body = multipart([
        {field, "_csrf_token", Token},
        {field, "name", "<script>alert(1)</script>"},
        {field, "message", "Hi"}
    ]),
    {200, _, Html} = post_multipart("/contact", Cookie, Body),
    ?assertEqual(nomatch, string:find(Html, "<script>")),
    ?assertNotEqual(nomatch, string:find(Html, "&lt;script&gt;")).

contact_post_missing_message_is_422(_Config) ->
    {Cookie, Token} = contact_session(),
    Body = multipart([{field, "_csrf_token", Token}, {field, "name", "Ada"}]),
    {422, _, Html} = post_multipart("/contact", Cookie, Body),
    ?assertNotEqual(nomatch, string:find(Html, "Please fill in your name and a message.")).

contact_attachment_too_large_is_413(_Config) ->
    {Cookie, Token} = contact_session(),
    Body = multipart([
        {field, "_csrf_token", Token},
        {file, "attachment", "big.bin", "application/octet-stream", lists:duplicate(1000001, $a)}
    ]),
    {413, _, _} = post_multipart("/contact", Cookie, Body).

%% Helpers

contact_session() ->
    {200, Headers, Html} = http_get("/contact"),
    [Cookie | _] = string:split(header("set-cookie", Headers), ";"),
    {match, [Token]} = re:run(Html, "name=\"_csrf_token\" value=\"([^\"]+)\"", [
        {capture, all_but_first, list}
    ]),
    {Cookie, Token}.

multipart(Parts) ->
    [[part(Part) || Part <- Parts], "--", boundary(), "--\r\n"].

part({field, Name, Value}) ->
    [
        "--",
        boundary(),
        "\r\ncontent-disposition: form-data; name=\"",
        Name,
        "\"\r\n\r\n",
        Value,
        "\r\n"
    ];
part({file, Name, Filename, ContentType, Data}) ->
    [
        "--",
        boundary(),
        "\r\ncontent-disposition: form-data; name=\"",
        Name,
        "\"; filename=\"",
        Filename,
        "\"\r\ncontent-type: ",
        ContentType,
        "\r\n\r\n",
        Data,
        "\r\n"
    ].

boundary() ->
    "foundations-boundary".

post_multipart(Path, Cookie, Body) ->
    ContentType = "multipart/form-data; boundary=" ++ boundary(),
    request(post, Path, [{"cookie", Cookie}], ContentType, iolist_to_binary(Body)).

http_get(Path) ->
    http_get(Path, []).

http_get(Path, Headers) ->
    request(get, Path, Headers).

http_post(Path, ContentType, Body) ->
    request(post, Path, [], ContentType, Body).

request(Method, Path, Headers) ->
    respond(httpc:request(Method, {url(Path), Headers}, [{autoredirect, false}], [])).

request(Method, Path, Headers, ContentType, Body) ->
    respond(
        httpc:request(Method, {url(Path), Headers, ContentType, Body}, [{autoredirect, false}], [])
    ).

respond({ok, {{_, Status, _}, Headers, Body}}) ->
    {Status, Headers, Body}.

header(Name, Headers) ->
    proplists:get_value(Name, Headers).

decode(Body) ->
    json:decode(unicode:characters_to_binary(Body)).

url(Path) ->
    {ok, #{port := Port}} = application:get_env(nova, cowboy_configuration),
    "http://localhost:" ++ integer_to_list(Port) ++ Path.
