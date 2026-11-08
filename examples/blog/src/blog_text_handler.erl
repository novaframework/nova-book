-module(blog_text_handler).

-export([handle/3]).

handle({text, Body}, Callback, Req) ->
    handle({text, 200, Body}, Callback, Req);
handle({text, Status, Body}, _Callback, Req) ->
    Req1 = cowboy_req:set_resp_headers(#{~"content-type" => ~"text/plain; charset=utf-8"}, Req),
    Req2 = cowboy_req:set_resp_body(Body, Req1),
    {ok, Req2#{resp_status_code => Status}}.
