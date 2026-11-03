-module(blog_ops_security).

-export([require_token/1]).

require_token(Req) ->
    Expected = application:get_env(blog, ops_token, undefined),
    case matches(cowboy_req:header(~"x-ops-token", Req), Expected) of
        true ->
            {true, #{role => ops}};
        false ->
            {false, 401, #{~"www-authenticate" => ~"X-Ops-Token"}, ~"unauthorized\n"}
    end.

matches(Given, Expected) when is_binary(Given), is_binary(Expected) ->
    crypto:hash_equals(crypto:hash(sha256, Given), crypto:hash(sha256, Expected));
matches(_Given, _Expected) ->
    false.
