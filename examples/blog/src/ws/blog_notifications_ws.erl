-module(blog_notifications_ws).
-behaviour(nova_websocket).

-include_lib("nova/include/nova_pubsub.hrl").

-export([init/1, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(State) ->
    {ok, State}.

websocket_init(State) ->
    ok = nova_pubsub:join(blog_notify:channel()),
    {reply, [frame(Event, Payload) || {Event, Payload} <- blog_notify_log:recent()], State}.

websocket_handle({text, ~"ping"}, State) ->
    {reply, {text, ~"pong"}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(#nova_pubsub{topic = Event, payload = Payload}, State) ->
    {reply, frame(Event, Payload), State};
websocket_info(_Message, State) ->
    {ok, State}.

frame(Event, Payload) ->
    {text, json:encode(#{~"event" => Event, ~"data" => Payload})}.
