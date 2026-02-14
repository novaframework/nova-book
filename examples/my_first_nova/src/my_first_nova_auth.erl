-module(my_first_nova_auth).
-export([
    username_password/1,
    session_auth/1
]).

%% Used for the login POST
username_password(#{params := Params}) ->
    case Params of
        #{
            <<"username">> := Username,
            <<"password">> := <<"password">>
        } ->
            {true, #{authed => true, username => Username}};
        _ ->
            false
    end.

%% Used for pages that need an active session
session_auth(Req) ->
    case nova_session:get(Req, <<"username">>) of
        {ok, Username} ->
            {true, #{authed => true, username => Username}};
        {error, _} ->
            false
    end.
