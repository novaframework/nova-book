-module(blog_notify_log).
-behaviour(gen_server).

-include_lib("nova/include/nova_pubsub.hrl").

-export([start_link/0, recent/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(KEEP, 10).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

recent() ->
    gen_server:call(?MODULE, recent).

init([]) ->
    ok = nova_pubsub:join(blog_notify:channel()),
    {ok, []}.

handle_call(recent, _From, Recent) ->
    {reply, lists:reverse(Recent), Recent}.

handle_cast(_Message, Recent) ->
    {noreply, Recent}.

handle_info(#nova_pubsub{topic = Event, payload = Payload}, Recent) ->
    {noreply, lists:sublist([{Event, Payload} | Recent], ?KEEP)};
handle_info(_Message, Recent) ->
    {noreply, Recent}.
