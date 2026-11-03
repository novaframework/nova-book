-module(blog_error_controller).

-export([not_found/1, server_error/1]).

not_found(Req) ->
    case wants_html(cowboy_req:header(~"accept", Req, ~"")) of
        true ->
            {ok, [{path, cowboy_req:path(Req)}], #{view => blog_not_found, status_code => 404}};
        false ->
            {status, 404, #{}, #{error => ~"not_found"}}
    end.

server_error(Req) ->
    case wants_html(cowboy_req:header(~"accept", Req, ~"")) of
        true -> {ok, [], #{view => blog_server_error, status_code => 500}};
        false -> {status, 500, #{}, #{error => ~"internal_error"}}
    end.

wants_html(Accept) ->
    binary:match(Accept, ~"text/html") =/= nomatch.
