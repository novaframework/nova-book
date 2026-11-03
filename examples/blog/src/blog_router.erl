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
            prefix => "/api",
            security => false,
            routes => [
                {"/posts", fun blog_posts_controller:index/1, #{methods => [get]}},
                {"/posts", fun blog_posts_controller:create/1, #{methods => [post]}},
                {"/posts/:id", fun blog_posts_controller:show/1, #{methods => [get]}},
                {"/posts/:id", fun blog_posts_controller:update/1, #{methods => [put]}},
                {"/posts/:id", fun blog_posts_controller:delete/1, #{methods => [delete]}},
                {"/posts/:id/comments", fun blog_posts_controller:create_comment/1, #{
                    methods => [post]
                }}
            ]
        }
    ].
