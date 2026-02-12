-module(my_first_nova_chat_handler).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(State) ->
    nova_pubsub:join(chat),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    nova_pubsub:broadcast(chat, "message", Msg),
    {ok, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({nova_pubsub, chat, _Sender, "message", Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.
