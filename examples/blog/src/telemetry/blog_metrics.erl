-module(blog_metrics).
-behaviour(gen_server).

-export([start_link/1, port/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

port() ->
    ranch:get_port(?MODULE).

init(Port) ->
    process_flag(trap_exit, true),
    Dispatch = cowboy_router:compile([{'_', [{"/metrics", otel_nova_prom_handler, []}]}]),
    {ok, _} = cowboy:start_clear(?MODULE, [{port, Port}], #{env => #{dispatch => Dispatch}}),
    {ok, Port}.

handle_call(_Request, _From, Port) ->
    {reply, ok, Port}.

handle_cast(_Msg, Port) ->
    {noreply, Port}.

terminate(_Reason, _Port) ->
    cowboy:stop_listener(?MODULE).
