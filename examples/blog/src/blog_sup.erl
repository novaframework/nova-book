-module(blog_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children =
        [
            #{id => blog_notify_log, start => {blog_notify_log, start_link, []}}
        ] ++ metrics_server(),
    {ok, {#{strategy => one_for_all}, Children}}.

metrics_server() ->
    case application:get_env(blog, metrics) of
        {ok, Opts} -> [otel_nova_prom_server:child_spec(Opts)];
        undefined -> []
    end.
