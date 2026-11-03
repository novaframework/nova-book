-module(blog_views_reference_controller).

-export([gone/1]).

gone(_Req) ->
    {status, 404}.
