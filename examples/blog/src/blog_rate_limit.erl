-module(blog_rate_limit).
-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0, client_ip/1]).

pre_request(Req, Env, Options, State) ->
    case nova_auth_rate_limit:pre_request(Req, Env, Options, State) of
        {stop, Req1} -> {stop, Req1, State};
        Allowed -> Allowed
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
        title => ~"blog_rate_limit",
        version => ~"1.0.0",
        url => ~"https://github.com/novaframework/nova-book",
        authors => [~"The Nova Book"],
        description => ~"nova_auth_rate_limit with a stop reply Nova accepts"
    }.

client_ip(Req) ->
    {IP, _Port} = cowboy_req:peer(Req),
    IP.
