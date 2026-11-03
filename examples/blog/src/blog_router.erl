-module(blog_router).
-behaviour(nova_router).

-export([
    routes/1
]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [
        #{
            prefix => "",
            security => false,
            routes => [
                {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
            ]
        },
        #{
            prefix => "",
            security => false,
            routes => [
                {"/about", fun blog_pages_controller:about/1, #{methods => [get]}},
                {"/about-us", fun blog_pages_controller:about_moved/1, #{methods => [get]}},
                {"/about.md", fun blog_pages_controller:about_markdown/1, #{methods => [get]}},
                {"/version", fun blog_pages_controller:version/1, #{methods => [get]}}
            ]
        },
        #{
            prefix => "/hello",
            security => false,
            plugins => [
                {pre_request, nova_correlation_plugin, #{
                    request_correlation_header => ~"x-correlation-id",
                    logger_metadata_key => correlation_id
                }},
                {pre_request, nova_cors_plugin, #{allow_origins => ~"*"}},
                {pre_request, nova_request_plugin, #{
                    decode_json_body => true,
                    read_urlencoded_body => true,
                    parse_qs => true
                }}
            ],
            routes => [
                {"/", fun blog_hello_controller:create/1, #{methods => [post]}},
                {"/:name", fun blog_hello_controller:greet/1, #{methods => [get, options]}}
            ]
        },
        #{
            prefix => "/contact",
            security => false,
            plugins => [
                {pre_request, nova_multipart_plugin, #{
                    handler => {nova_multipart_memory_handler, #{}},
                    max_parts => 8,
                    max_part_size => 1000000
                }},
                {pre_request, nova_csrf_plugin, #{}}
            ],
            routes => [
                {"/", fun blog_contact_controller:show/1, #{methods => [get]}},
                {"/", fun blog_contact_controller:submit/1, #{methods => [post]}}
            ]
        },
        #{
            prefix => "/ops",
            security => fun blog_ops_security:require_token/1,
            routes => [
                {"/status", fun blog_ops_controller:status/1, #{methods => [get]}}
            ]
        },
        #{
            prefix => "/ws",
            security => fun blog_ws_security:same_origin/1,
            routes => [
                {"/echo", blog_echo_ws, #{protocol => ws}},
                {"/notifications", blog_notifications_ws, #{protocol => ws}}
            ]
        },
        #{
            prefix => "",
            security => false,
            routes => [
                {"/robots.txt", "assets/robots.txt", #{mimetype => ~"text/plain"}},
                {"/assets/[...]", "assets"},
                {404, fun blog_error_controller:not_found/1, #{}}
            ]
        }
    ].
