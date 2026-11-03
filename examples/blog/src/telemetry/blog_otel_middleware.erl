-module(blog_otel_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(#{otel_ctx := Ctx} = Req, Env) ->
    _ = otel_ctx:attach(Ctx),
    {ok, _Req, _State} = otel_nova_plugin:pre_request(Req, Env, #{}, undefined),
    {ok, Req, Env};
execute(Req, Env) ->
    {ok, Req, Env}.
