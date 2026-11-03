-module(blog_security_tests).
-include_lib("nova_test/include/nova_test.hrl").

bearer_token_test_() ->
    Token = fun(Value) ->
        blog_security:bearer_token(
            nova_test_req:with_header(~"authorization", Value, nova_test_req:new(get, "/api/me"))
        )
    end,
    [
        ?_assertEqual({ok, ~"abc123"}, Token(~"Bearer abc123")),
        ?_assertEqual({ok, ~"abc123"}, Token(~"bearer  abc123 ")),
        ?_assertEqual(error, Token(~"Basic YWxhZGRpbjpvcGVu")),
        ?_assertEqual(error, blog_security:bearer_token(nova_test_req:new(get, "/api/me")))
    ].

missing_bearer_token_is_401_test() ->
    {false, 401, Headers, Body} = blog_security:require_bearer(nova_test_req:new(get, "/api/me")),
    ?assertEqual(~"Bearer", maps:get(~"www-authenticate", Headers)),
    ?assertEqual(#{~"error" => ~"missing bearer token"}, json:decode(Body)).

ops_token_test_() ->
    {setup, fun set_ops_token/0, fun unset_ops_token/1, [
        ?_assertEqual({true, #{role => ops}}, blog_ops_security:require_token(ops_req(~"s3cret"))),
        ?_assertMatch({false, 401, _, _}, blog_ops_security:require_token(ops_req(~"guess"))),
        ?_assertMatch(
            {false, 401, _, _}, blog_ops_security:require_token(nova_test_req:new(get, "/ops"))
        )
    ]}.

set_ops_token() ->
    application:set_env(blog, ops_token, ~"s3cret").

unset_ops_token(_) ->
    application:unset_env(blog, ops_token).

ops_req(Token) ->
    nova_test_req:with_header(~"x-ops-token", Token, nova_test_req:new(get, "/ops/jobs")).
