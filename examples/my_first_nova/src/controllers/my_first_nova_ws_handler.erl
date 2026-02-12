-module(my_first_nova_ws_handler).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    {reply, {text, <<"Echo: ", Msg/binary>>}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
