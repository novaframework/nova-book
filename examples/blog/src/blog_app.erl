-module(blog_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = kura_migrator:migrate(blog_repo),
    blog_sup:start_link().

stop(_State) ->
    ok.
