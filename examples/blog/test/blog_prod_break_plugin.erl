-module(blog_prod_break_plugin).
-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0]).

pre_request(Req, _Env, _Options, State) ->
    {break, Req, State}.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
        title => ~"Breaking test plugin",
        version => ~"1.0.0",
        url => ~"https://github.com/novaframework/nova-book",
        authors => [~"The Nova Book"],
        description => ~"Returns break from pre_request",
        options => []
    }.
