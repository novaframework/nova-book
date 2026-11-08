-module(blog_require_json_plugin).
-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0]).

pre_request(#{json := _} = Req, _Env, _Options, State) ->
    {ok, Req, State};
pre_request(#{method := Method} = Req, _Env, _Options, State) when
    Method =:= ~"POST"; Method =:= ~"PUT"; Method =:= ~"PATCH"
->
    Body = json:encode(#{error => ~"expected a JSON body"}),
    {stop, {reply, 415, #{~"content-type" => ~"application/json"}, Body}, Req, State};
pre_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
        title => ~"Require a JSON body",
        version => ~"1.0.0",
        url => ~"https://github.com/novaframework/nova-book",
        authors => [~"Erlang Web Development with Nova"],
        description => ~"Answers 415 when a write request has no decoded JSON body",
        options => []
    }.
