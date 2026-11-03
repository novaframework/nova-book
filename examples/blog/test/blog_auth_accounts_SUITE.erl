-module(blog_auth_accounts_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    config_merges_defaults/1,
    passwords_are_hashed_with_pbkdf2/1,
    authenticate_checks_the_password/1,
    confirm_token_sets_confirmed_at/1,
    reset_token_changes_password_and_ends_sessions/1,
    tokens_need_url_encoding/1
]).

all() ->
    [
        config_merges_defaults,
        passwords_are_hashed_with_pbkdf2,
        authenticate_checks_the_password,
        confirm_token_sets_confirmed_at,
        reset_token_changes_password_and_ends_sessions,
        tokens_need_url_encoding
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

config_merges_defaults(_Config) ->
    #{
        repo := blog_repo,
        user_identity_field := email,
        user_password_field := hashed_password,
        session_validity_days := 14,
        confirm_validity_days := 3,
        reset_validity_hours := 1
    } = nova_auth:config(blog_auth).

passwords_are_hashed_with_pbkdf2(Config) ->
    #{hashed_password := Hash} = ?config(alice, Config),
    <<"$pbkdf2-sha256$1000$", _/binary>> = Hash,
    true = nova_auth_password:verify(~"correct horse battery", Hash),
    <<"$pbkdf2-sha256$", _/binary>> = nova_auth_password:hash(~"secret", bcrypt).

authenticate_checks_the_password(_Config) ->
    {ok, #{username := ~"alice"}} =
        blog_accounts:authenticate(~"alice@example.com", ~"correct horse battery"),
    {error, invalid_credentials} = blog_accounts:authenticate(~"alice@example.com", ~"nope"),
    {error, invalid_credentials} = blog_accounts:authenticate(~"bob@example.com", ~"nope"),
    {error, invalid_credentials} = blog_accounts:authenticate(undefined, undefined).

confirm_token_sets_confirmed_at(Config) ->
    #{confirmed_at := undefined} = Alice = ?config(alice, Config),
    {ok, Token} = nova_auth_confirm:generate_confirm_token(blog_auth, Alice),
    {ok, #{confirmed_at := {{_, _, _}, {_, _, _}}}} = nova_auth_confirm:confirm_user(
        blog_auth, Token
    ),
    {error, not_found} = nova_auth_confirm:confirm_user(blog_auth, Token).

reset_token_changes_password_and_ends_sessions(Config) ->
    Alice = ?config(alice, Config),
    {ok, Session} = blog_accounts:issue_token(Alice),
    {ok, Reset} = nova_auth_reset:generate_reset_token(blog_auth, Alice),
    New = ~"a brand new passphrase",
    {ok, _} = nova_auth_reset:reset_password(blog_auth, Reset, #{
        ~"password" => New, ~"password_confirmation" => New
    }),
    {error, not_found} = blog_accounts:user_for_token(Session),
    {ok, _} = blog_accounts:authenticate(~"alice@example.com", New),
    {error, not_found} = nova_auth_reset:reset_password(blog_auth, Reset, #{}).

tokens_need_url_encoding(Config) ->
    Tokens = [
        T
     || _ <- lists:seq(1, 50), {ok, T} <- [blog_accounts:issue_token(?config(alice, Config))]
    ],
    ?assert(lists:any(fun(T) -> binary:match(T, [~"/", ~"+"]) =/= nomatch end, Tokens)),
    [Token | _] = Tokens,
    Query = uri_string:compose_query([{~"token", Token}]),
    [{~"token", Token}] = uri_string:dissect_query(Query).
