-module(blog_auth).
-behaviour(nova_auth).

-export([config/0]).

config() ->
    #{
        repo => blog_repo,
        user_schema => blog_user,
        token_schema => blog_user_token,
        session_validity_days => 14
    }.
