-module(blog_json).

-export([encode/1, decode/1]).

encode(Term) ->
    json:encode(Term).

decode(Binary) ->
    try
        {ok, json:decode(Binary)}
    catch
        error:Reason -> {error, Reason}
    end.
