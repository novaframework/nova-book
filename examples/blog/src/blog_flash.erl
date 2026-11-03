-module(blog_flash).

-export([put/3, take/1]).

put(Req, Kind, Message) when Kind =:= info; Kind =:= error ->
    nova_session:set(Req, key(Kind), Message).

take(Req) ->
    [Message || Kind <- [info, error], Message <- take(Req, Kind)].

take(Req, Kind) ->
    case nova_session:get(Req, key(Kind)) of
        {ok, Text} ->
            {ok, _} = nova_session:delete(Req, key(Kind)),
            [#{kind => atom_to_binary(Kind), text => Text}];
        {error, _} ->
            []
    end.

key(info) -> ~"flash_info";
key(error) -> ~"flash_error".
