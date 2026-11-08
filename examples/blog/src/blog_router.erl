-module(blog_router).
-behaviour(nova_router).

-export([
    routes/1
]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [traced(Group) || Group <- route_groups()].

route_groups() ->
    [
        #{
            prefix => "",
            security => false,
            routes => [
                {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
                {"/health", fun blog_health_controller:ready/1, #{methods => [get]}}
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
            prefix => "/api",
            security => fun blog_security:optional_bearer/1,
            plugins => [
                {pre_request, blog_cors_plugin, api_cors()},
                {pre_request, blog_request_logger, #{}},
                {post_request, blog_request_logger, #{}}
            ],
            routes => [
                {"/posts", fun blog_posts_controller:index/1, #{methods => [get, options]}},
                {"/posts/:id", fun blog_posts_controller:show/1, #{methods => [get, options]}}
            ]
        },
        #{
            prefix => "/api",
            security => fun blog_security:require_bearer/1,
            routes => [
                {"/posts", fun blog_posts_controller:create/1, #{methods => [post]}},
                {"/posts/:id", fun blog_posts_controller:update/1, #{methods => [put]}},
                {"/posts/:id", fun blog_posts_controller:delete/1, #{methods => [delete]}},
                {"/posts/:id/comments", fun blog_posts_controller:create_comment/1, #{
                    methods => [post]
                }},
                {"/posts/:id/tags", fun blog_posts_controller:update_tags/1, #{methods => [put]}},
                {"/tokens", fun blog_tokens_controller:delete/1, #{methods => [delete]}}
            ]
        },
        #{
            prefix => "/api",
            security => false,
            plugins => [
                {pre_request, nova_request_plugin, #{decode_json_body => true}},
                {pre_request, blog_rate_limit, login_rate_limit()}
            ],
            routes => [
                {"/tokens", fun blog_tokens_controller:create/1, #{methods => [post]}}
            ]
        },
        #{
            prefix => "",
            security => fun blog_security:current_user/1,
            plugins => html_plugins(),
            routes => [
                {"/posts", fun blog_posts_html_controller:index/1, #{methods => [get]}},
                {"/posts/:id", fun blog_posts_html_controller:show/1, #{methods => [get]}},
                {"/login", fun blog_session_controller:new/1, #{methods => [get]}},
                {"/register", fun blog_registration_controller:new/1, #{methods => [get]}}
            ]
        },
        #{
            prefix => "",
            security => false,
            plugins => [
                {pre_request, blog_rate_limit, login_rate_limit()} | html_plugins()
            ],
            routes => [
                {"/login", fun blog_session_controller:create/1, #{methods => [post]}},
                {"/register", fun blog_registration_controller:create/1, #{methods => [post]}}
            ]
        },
        #{
            prefix => "",
            security => fun blog_security:require_user/1,
            plugins => html_plugins(),
            routes => [
                {"/posts/new", fun blog_posts_html_controller:new/1, #{methods => [get]}},
                {"/posts", fun blog_posts_html_controller:create/1, #{methods => [post]}},
                {"/posts/:id/edit", fun blog_posts_html_controller:edit/1, #{methods => [get]}},
                {"/posts/:id", fun blog_posts_html_controller:update/1, #{methods => [post]}},
                {"/posts/:id/delete", fun blog_posts_html_controller:delete/1, #{
                    methods => [post]
                }},
                {"/logout", fun blog_session_controller:delete/1, #{methods => [post]}}
            ]
        },
        #{
            prefix => "/admin",
            security => fun blog_security:require_admin/1,
            plugins => html_plugins(),
            routes => [
                {"/users", fun blog_admin_controller:users/1, #{methods => [get]}}
            ]
        },
        #{
            prefix => "/api",
            security => false,
            plugins => tag_plugins(),
            routes => [
                {"/tags", fun blog_tags_controller:list/1, #{methods => [get]}},
                {"/tags/:id", fun blog_tags_controller:show/1, #{methods => [get]}},
                {"/openapi.json", "openapi/openapi.json", #{mimetype => ~"application/json"}}
            ]
        },
        #{
            prefix => "/api",
            security => fun blog_security:require_admin_bearer/1,
            plugins => tag_plugins(),
            routes => [
                {"/tags", fun blog_tags_controller:create/1, #{
                    methods => [post], extra_state => #{json_schema => "schemas/tag.json"}
                }},
                {"/tags/:id", fun blog_tags_controller:update/1, #{
                    methods => [put], extra_state => #{json_schema => "schemas/tag.json"}
                }},
                {"/tags/:id", fun blog_tags_controller:delete/1, #{methods => [delete]}}
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
            prefix => "/ops",
            security => fun blog_ops_security:require_token/1,
            routes => [
                {"/jobs", fun blog_jobs_controller:health/1, #{methods => [get]}}
            ]
        },
        #{
            prefix => "",
            security => fun blog_security:current_user/1,
            plugins => html_plugins(),
            routes => [
                {"/confirm", fun blog_account_controller:confirm_form/1, #{methods => [get]}},
                {"/confirm", fun blog_account_controller:confirm/1, #{methods => [post]}},
                {"/password/forgot", fun blog_account_controller:forgot_form/1, #{methods => [get]}},
                {"/password/reset", fun blog_account_controller:reset_form/1, #{methods => [get]}},
                {"/password/reset", fun blog_account_controller:reset/1, #{methods => [post]}}
            ]
        },
        #{
            prefix => "",
            security => false,
            plugins => [
                {pre_request, blog_rate_limit, login_rate_limit()} | html_plugins()
            ],
            routes => [
                {"/password/forgot", fun blog_account_controller:send_reset/1, #{methods => [post]}}
            ]
        },
        #{
            prefix => "",
            security => false,
            routes => [
                {"/robots.txt", "assets/robots.txt", #{mimetype => ~"text/plain"}},
                {"/assets/[...]", "assets"},
                {404, fun blog_error_controller:not_found/1, #{}},
                {500, fun blog_error_controller:server_error/1, #{}}
            ]
        }
    ].

traced(#{plugins := Plugins} = Group) ->
    Group#{plugins => [{pre_request, otel_nova_plugin, #{}} | Plugins]};
traced(Group) ->
    Group.

html_plugins() ->
    [
        {pre_request, nova_request_plugin, #{read_urlencoded_body => true}},
        {pre_request, nova_csrf_plugin, #{}}
    ].

api_cors() ->
    #{
        allow_origins => application:get_env(blog, cors_origins, []),
        allow_methods => ~"GET",
        allow_headers => ~"authorization",
        max_age => 600
    }.

tag_plugins() ->
    [
        {pre_request, nova_request_plugin, #{decode_json_body => true}},
        {pre_request, blog_require_json_plugin, #{}},
        {pre_request, nova_json_schemas, #{render_errors => false}}
    ].

login_rate_limit() ->
    #{
        limiter => blog_login_limiter,
        limit => 5,
        window => 60000,
        algorithm => sliding_window,
        key_fun => fun blog_rate_limit:client_ip/1
    }.
