-module(blog_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = kura_migrator:migrate(blog_repo),
    ok = nova_handlers:register_handler(text, fun blog_text_handler:handle/3),
    blog_sup:start_link().

stop(_State) ->
    ok.
