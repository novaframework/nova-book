-module(blog_ops_controller).

-export([status/1]).

status(#{auth_data := #{role := Role}}) ->
    {json, #{status => ~"ok", role => Role, node => node()}}.
