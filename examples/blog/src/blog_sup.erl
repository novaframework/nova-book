-module(blog_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{id => blog_notify_log, start => {blog_notify_log, start_link, []}}
    ],
    {ok, {#{strategy => one_for_all}, Children}}.
