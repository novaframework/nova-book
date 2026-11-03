-module(blog_whoami_ws).
-behaviour(nova_websocket).

-export([authenticate/1, init/1, websocket_handle/2, websocket_info/2]).

authenticate(Req) ->
    case cowboy_req:header(~"x-test-user", Req) of
        undefined -> {false, 401, #{}};
        Id -> {true, #{id => binary_to_integer(Id)}}
    end.

init(#{auth_data := User} = State) ->
    {ok, State#{user_id => maps:get(id, User)}}.

websocket_handle({text, ~"whoami"}, #{user_id := UserId} = State) ->
    {reply, {text, integer_to_binary(UserId)}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Message, State) ->
    {ok, State}.
