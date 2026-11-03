-module(blog_ws_security).
-export([same_origin/1]).

same_origin(Req) ->
    Host = cowboy_req:header(~"host", Req),
    case cowboy_req:header(~"origin", Req) of
        undefined -> true;
        <<"http://", Host/binary>> -> true;
        <<"https://", Host/binary>> -> true;
        _Other -> {false, 403, #{}}
    end.
