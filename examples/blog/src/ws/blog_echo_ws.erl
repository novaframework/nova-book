-module(blog_echo_ws).
-behaviour(nova_websocket).

-export([init/1, websocket_handle/2, websocket_info/2]).

init(State) ->
    {ok, State#{received => 0}}.

websocket_handle({text, ~"stats"}, #{received := Received} = State) ->
    {reply, {text, <<"received ", (integer_to_binary(Received))/binary>>}, State};
websocket_handle({text, Text}, #{received := Received} = State) ->
    {reply, {text, <<"echo: ", Text/binary>>}, State#{received => Received + 1}};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Message, State) ->
    {ok, State}.
