-module(blog_reference_controller).

-export([
    remember/1,
    missing/1,
    broken/1,
    keys/1
]).

remember(Req) ->
    Req1 = cowboy_req:set_resp_cookie(~"seen", ~"1", Req, #{http_only => true}),
    {json, 200, #{}, Req1, #{remembered => true}}.

missing(_Req) ->
    throw({404, page_missing}).

broken(_Req) ->
    oops.

keys(#{bindings := Bindings} = Req) ->
    {json, #{keys => lists:sort(maps:keys(Req)), bindings => Bindings}}.
