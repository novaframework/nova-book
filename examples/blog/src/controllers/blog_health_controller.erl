-module(blog_health_controller).

-export([ready/1]).

ready(_Req) ->
    case blog_repo:query(~"SELECT 1", []) of
        {ok, [_Row]} -> {json, #{status => ~"ok"}};
        {error, _Reason} -> {json, 503, #{}, #{status => ~"unavailable"}}
    end.
