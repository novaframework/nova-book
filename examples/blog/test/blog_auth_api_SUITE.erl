-module(blog_auth_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    token_is_issued_for_valid_credentials/1,
    token_request_with_wrong_password_is_401/1,
    token_request_without_fields_is_400/1,
    token_is_stored_hashed/1,
    write_without_token_is_401/1,
    write_with_unknown_token_is_401/1,
    reads_are_public/1,
    post_author_comes_from_token/1,
    revoked_token_is_rejected/1,
    token_requests_share_the_login_rate_limit/1
]).

all() ->
    [
        token_is_issued_for_valid_credentials,
        token_request_with_wrong_password_is_401,
        token_request_without_fields_is_400,
        token_is_stored_hashed,
        write_without_token_is_401,
        write_with_unknown_token_is_401,
        reads_are_public,
        post_author_comes_from_token,
        revoked_token_is_rejected,
        token_requests_share_the_login_rate_limit
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    blog_web:reset_rate_limit(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

token_is_issued_for_valid_credentials(_Config) ->
    {201, Headers, Body, _} = blog_web:post_json(#{}, "/api/tokens", #{
        email => ~"alice@example.com", password => ~"correct horse battery"
    }),
    "application/json" = blog_web:header("content-type", Headers),
    #{~"token" := Token, ~"token_type" := ~"Bearer", ~"expires_in" := 1209600} = json:decode(Body),
    ?assertEqual(44, byte_size(Token)).

token_request_with_wrong_password_is_401(_Config) ->
    {401, _, Body, _} = blog_web:post_json(#{}, "/api/tokens", #{
        email => ~"alice@example.com", password => ~"wrong"
    }),
    #{~"error" := ~"invalid email or password"} = json:decode(Body).

token_request_without_fields_is_400(_Config) ->
    {400, _, _, _} = blog_web:post_json(#{}, "/api/tokens", #{email => ~"alice@example.com"}).

token_is_stored_hashed(_Config) ->
    Token = blog_web:api_token("alice@example.com", "correct horse battery"),
    {ok, [#{token := Stored, context := ~"session"}]} =
        blog_repo:all(kura_query:from(blog_user_token)),
    ?assertNotEqual(Token, Stored),
    ?assertEqual(nova_auth_token:hash(Token), Stored).

write_without_token_is_401(_Config) ->
    {401, Headers, Body, _} = blog_web:post_json(#{}, "/api/posts", #{title => ~"Nope"}),
    "Bearer" = blog_web:header("www-authenticate", Headers),
    #{~"error" := ~"missing bearer token"} = json:decode(Body).

write_with_unknown_token_is_401(_Config) ->
    Forged = [{"authorization", "Bearer bm90IGEgcmVhbCB0b2tlbg=="}],
    {401, _, Body, _} = blog_web:post_json(#{}, "/api/posts", #{title => ~"Nope"}, Forged),
    #{~"error" := ~"invalid or expired token"} = json:decode(Body),
    Garbage = [{"authorization", "Bearer !!!"}],
    {401, _, _, _} = blog_web:post_json(#{}, "/api/posts", #{title => ~"Nope"}, Garbage).

reads_are_public(Config) ->
    #{id := Id} = blog_data_fixtures:post(?config(alice, Config), #{
        ~"title" => ~"Public", ~"status" => ~"published"
    }),
    {200, _} = blog_data_fixtures:request(get, "/api/posts"),
    {200, #{~"title" := ~"Public"}} = blog_data_fixtures:request(
        get, "/api/posts/" ++ integer_to_list(Id)
    ).

post_author_comes_from_token(Config) ->
    #{id := AliceId} = ?config(alice, Config),
    Token = blog_web:api_token("alice@example.com", "correct horse battery"),
    {201, #{~"user_id" := AliceId}} = blog_data_fixtures:request_as(Token, post, "/api/posts", #{
        title => ~"Mine", body => ~"Written by Alice", user_id => AliceId + 1000
    }).

revoked_token_is_rejected(_Config) ->
    Token = blog_web:api_token("alice@example.com", "correct horse battery"),
    {204, <<>>} = blog_data_fixtures:request_as(Token, delete, "/api/tokens"),
    {401, #{~"error" := ~"invalid or expired token"}} =
        blog_data_fixtures:request_as(Token, post, "/api/posts", #{title => ~"Late", body => ~"x"}),
    {ok, []} = blog_repo:all(kura_query:from(blog_user_token)).

token_requests_share_the_login_rate_limit(_Config) ->
    Wrong = #{email => ~"alice@example.com", password => ~"guess"},
    [{401, _, _, _} = blog_web:post_json(#{}, "/api/tokens", Wrong) || _ <- lists:seq(1, 5)],
    {429, _, Limited, _} = blog_web:post_json(#{}, "/api/tokens", Wrong),
    #{~"error" := ~"too many requests"} = json:decode(Limited),
    {200, _, Form, Jar} = blog_web:get(#{}, "/login"),
    {429, _, _, _} = blog_web:post_form(Jar, "/login", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "alice@example.com"},
        {"password", "correct horse battery"}
    ]).
