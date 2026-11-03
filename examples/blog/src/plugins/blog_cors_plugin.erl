-module(blog_cors_plugin).
-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0]).

pre_request(Req, _Env, Options, State) ->
    Origin = cowboy_req:header(~"origin", Req),
    Allowed = lists:member(Origin, maps:get(allow_origins, Options)),
    Req1 = cowboy_req:set_resp_header(~"vary", ~"origin", Req),
    case {cowboy_req:method(Req), Allowed} of
        {~"OPTIONS", true} ->
            {stop, {reply, 204, preflight_headers(Origin, Options), ~""}, Req1, State};
        {~"OPTIONS", false} ->
            {stop, {reply, 403, #{}, ~""}, Req1, State};
        {_Method, true} ->
            {ok, cowboy_req:set_resp_header(~"access-control-allow-origin", Origin, Req1), State};
        {_Method, false} ->
            {ok, Req1, State}
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
        title => ~"blog_cors_plugin",
        version => ~"1.0.0",
        url => ~"https://github.com/novaframework/nova-book",
        authors => [~"The Nova Book"],
        description => ~"CORS with an origin allowlist and explicit methods and headers",
        options => [
            {allow_origins, ~"Origins that may call the routes, e.g. [~\"https://app.example\"]"},
            {allow_methods, ~"Value of access-control-allow-methods"},
            {allow_headers, ~"Value of access-control-allow-headers"},
            {max_age, ~"Seconds a browser may cache the preflight"}
        ]
    }.

preflight_headers(Origin, Options) ->
    #{
        ~"access-control-allow-origin" => Origin,
        ~"access-control-allow-methods" => maps:get(allow_methods, Options),
        ~"access-control-allow-headers" => maps:get(allow_headers, Options),
        ~"access-control-max-age" => integer_to_binary(maps:get(max_age, Options))
    }.
