-module(blog_test_mail_adapter).
-behaviour(hikyaku_adapter).

-export([deliver/2]).

deliver(_Email, #{reply := Reply}) ->
    Reply.
