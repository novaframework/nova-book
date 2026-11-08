-module(blog_auth_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").

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
    blog_data_fixtures:start(Config).

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    blog_data_fixtures:reset_rate_limit(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

token_is_issued_for_valid_credentials(Config) ->
    Credentials = #{~"email" => ~"alice@example.com", ~"password" => ~"correct horse battery"},
    {ok, Resp} = nova_test:post("/api/tokens", #{json => Credentials}, Config),
    ?assertStatus(201, Resp),
    ?assertHeader("content-type", "application/json", Resp),
    #{~"token" := Token, ~"token_type" := ~"Bearer", ~"expires_in" := 1209600} =
        nova_test:json(Resp),
    ?assertEqual(44, byte_size(Token)).

token_request_with_wrong_password_is_401(Config) ->
    Credentials = #{~"email" => ~"alice@example.com", ~"password" => ~"wrong"},
    {ok, Resp} = nova_test:post("/api/tokens", #{json => Credentials}, Config),
    ?assertStatus(401, Resp),
    ?assertJson(#{~"error" := ~"invalid email or password"}, Resp).

token_request_without_fields_is_400(Config) ->
    Credentials = #{json => #{~"email" => ~"alice@example.com"}},
    {ok, Resp} = nova_test:post("/api/tokens", Credentials, Config),
    ?assertStatus(400, Resp).

token_is_stored_hashed(Config) ->
    Token = blog_data_fixtures:api_token(Config, ~"alice@example.com", ~"correct horse battery"),
    {ok, [#{token := Stored, context := ~"session"}]} =
        blog_repo:all(kura_query:from(blog_user_token)),
    ?assertNotEqual(Token, Stored),
    ?assertEqual(nova_auth_token:hash(Token), Stored).

write_without_token_is_401(Config) ->
    {ok, Resp} = nova_test:post("/api/posts", #{json => #{~"title" => ~"Nope"}}, Config),
    ?assertStatus(401, Resp),
    ?assertHeader("www-authenticate", "Bearer", Resp),
    ?assertJson(#{~"error" := ~"missing bearer token"}, Resp).

write_with_unknown_token_is_401(Config) ->
    Forged = #{
        json => #{~"title" => ~"Nope"},
        headers => [{~"authorization", ~"Bearer bm90IGEgcmVhbCB0b2tlbg=="}]
    },
    {ok, Resp} = nova_test:post("/api/posts", Forged, Config),
    ?assertStatus(401, Resp),
    ?assertJson(#{~"error" := ~"invalid or expired token"}, Resp),
    Garbage = Forged#{headers => [{~"authorization", ~"Bearer !!!"}]},
    {ok, #{status := 401}} = nova_test:post("/api/posts", Garbage, Config).

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
    Token = blog_data_fixtures:api_token(Config, ~"alice@example.com", ~"correct horse battery"),
    {201, #{~"user_id" := AliceId}} = blog_data_fixtures:request_as(Token, post, "/api/posts", #{
        title => ~"Mine", body => ~"Written by Alice", user_id => AliceId + 1000
    }).

revoked_token_is_rejected(Config) ->
    Token = blog_data_fixtures:api_token(Config, ~"alice@example.com", ~"correct horse battery"),
    {204, <<>>} = blog_data_fixtures:request_as(Token, delete, "/api/tokens"),
    {401, #{~"error" := ~"invalid or expired token"}} =
        blog_data_fixtures:request_as(Token, post, "/api/posts", #{title => ~"Late", body => ~"x"}),
    {ok, []} = blog_repo:all(kura_query:from(blog_user_token)).

token_requests_share_the_login_rate_limit(Config) ->
    Wrong = #{json => #{~"email" => ~"alice@example.com", ~"password" => ~"guess"}},
    [{ok, #{status := 401}} = nova_test:post("/api/tokens", Wrong, Config) || _ <- lists:seq(1, 5)],
    {ok, Limited} = nova_test:post("/api/tokens", Wrong, Config),
    ?assertStatus(429, Limited),
    ?assertJson(#{~"error" := ~"too many requests"}, Limited),
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/login", Session),
    {ok, Login} = nova_test:post(
        "/login",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"email", ~"alice@example.com"},
                {~"password", ~"correct horse battery"}
            ]
        },
        Session
    ),
    ?assertStatus(429, Login).
