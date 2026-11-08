-module(blog_api_errors_test_plugin).
-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0]).

pre_request(_Req, _Env, _Options, _State) ->
    error(plugin_failed).

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
        title => ~"Crashing test plugin",
        version => ~"1.0.0",
        url => ~"https://github.com/novaframework/nova-book",
        authors => [~"Erlang Web Development with Nova"],
        description => ~"Raises in pre_request",
        options => []
    }.
