-module(my_first_nova_notifications_handler).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(State) ->
    nova_pubsub:join(notes),
    {ok, State}.

websocket_handle({text, <<"ping">>}, State) ->
    {reply, {text, <<"pong">>}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({nova_pubsub, notes, _Sender, Topic, Payload}, State) ->
    Msg = thoas:encode(#{
        event => list_to_binary(Topic),
        data => Payload
    }),
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.
