-module(blog_auth_login_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    login_page_has_csrf_token/1,
    login_without_csrf_token_is_403/1,
    login_with_wrong_password_rerenders_form/1,
    login_with_unknown_email_rerenders_form/1,
    login_sets_session_and_redirects/1,
    email_is_case_insensitive/1,
    logout_revokes_the_session/1,
    logout_requires_csrf_token/1,
    register_creates_account_and_logs_in/1,
    register_shows_changeset_errors/1,
    register_rejects_duplicate_email/1,
    login_is_rate_limited/1
]).

-define(PASSWORD, ~"correct horse battery").

all() ->
    [
        login_page_has_csrf_token,
        login_without_csrf_token_is_403,
        login_with_wrong_password_rerenders_form,
        login_with_unknown_email_rerenders_form,
        login_sets_session_and_redirects,
        email_is_case_insensitive,
        logout_revokes_the_session,
        logout_requires_csrf_token,
        register_creates_account_and_logs_in,
        register_shows_changeset_errors,
        register_rejects_duplicate_email,
        login_is_rate_limited
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

login_page_has_csrf_token(Config) ->
    {ok, Resp} = nova_test:get("/login", Config),
    ?assertStatus(200, Resp),
    ?assertMatch(<<_:8, _/binary>>, nova_test:csrf_token(Resp)),
    ?assertHeader("content-type", "text/html", Resp).

login_without_csrf_token_is_403(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/login", Session),
    ?assert(is_binary(nova_test:csrf_token(Form))),
    Login = #{form => [{~"email", ~"alice@example.com"}, {~"password", ?PASSWORD}]},
    {ok, Resp} = nova_test:post("/login", Login, Session),
    ?assertStatus(403, Resp),
    ?assertBody(~"Forbidden - CSRF token invalid", Resp).

login_with_wrong_password_rerenders_form(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/login", Session),
    {ok, Resp} = nova_test:post(
        "/login",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"email", ~"alice@example.com"},
                {~"password", ~"not the password"}
            ]
        },
        Session
    ),
    ?assertStatus(422, Resp),
    Body = nova_test:body(Resp),
    ?assertNotEqual(nomatch, binary:match(Body, ~"Invalid email or password.")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"value=\"alice@example.com\"")),
    ?assertEqual(nomatch, binary:match(Body, ~"not the password")),
    {ok, NewPost} = nova_test:get("/posts/new", Session),
    ?assertStatus(302, NewPost).

login_with_unknown_email_rerenders_form(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/login", Session),
    {ok, Resp} = nova_test:post(
        "/login",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"email", ~"nobody@example.com"},
                {~"password", ?PASSWORD}
            ]
        },
        Session
    ),
    ?assertStatus(422, Resp),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(Resp), ~"Invalid email or password.")).

login_sets_session_and_redirects(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/login", Session),
    AnonymousId = nova_test:cookie(~"session_id", Session),
    {ok, Resp} = nova_test:post(
        "/login",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"email", ~"alice@example.com"},
                {~"password", ?PASSWORD}
            ]
        },
        Session
    ),
    ?assertStatus(302, Resp),
    ?assertHeader("location", "/posts", Resp),
    SessionId = nova_test:cookie(~"session_id", Session),
    ?assertNotEqual(AnonymousId, SessionId),
    {ok, Page} = nova_test:get("/posts", Session),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(Page), ~"Welcome back!")),
    ?assertNotEqual(
        nomatch, binary:match(nova_test:body(Page), ~"<span class=\"who\">alice</span>")
    ),
    {ok, NewPost} = nova_test:get("/posts/new", Session),
    ?assertStatus(200, NewPost),
    Anonymous = nova_test:set_cookie(~"session_id", AnonymousId, Config),
    {ok, Refused} = nova_test:get("/posts/new", Anonymous),
    ?assertStatus(302, Refused),
    {ok, #{id := _, provider := password, username := ~"alice", role := author}} =
        nova_auth_actor:fetch(#{nova_session_id => SessionId}).

email_is_case_insensitive(Config) ->
    Session = blog_data_fixtures:log_in(Config, ~"  Alice@Example.COM ", ?PASSWORD),
    {ok, Resp} = nova_test:get("/posts/new", Session),
    ?assertStatus(200, Resp).

logout_revokes_the_session(Config) ->
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    LoggedIn = nova_test:set_cookie(
        ~"session_id", nova_test:cookie(~"session_id", Session), Config
    ),
    {ok, Page} = nova_test:get("/posts", Session),
    {ok, Logout} = nova_test:post(
        "/logout", #{form => [{~"_csrf_token", nova_test:csrf_token(Page)}]}, Session
    ),
    ?assertStatus(302, Logout),
    ?assertHeader("location", "/posts", Logout),
    {ok, After} = nova_test:get("/posts", Session),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(After), ~"You have been logged out.")),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(After), ~"href=\"/login\"")),
    {ok, Old} = nova_test:get("/posts/new", LoggedIn),
    ?assertStatus(302, Old),
    ?assertHeader("location", "/login", Old),
    {ok, []} = blog_repo:all(kura_query:from(blog_user_token)).

logout_requires_csrf_token(Config) ->
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, Logout} = nova_test:post("/logout", #{form => []}, Session),
    ?assertStatus(403, Logout),
    {ok, Page} = nova_test:get("/posts/new", Session),
    ?assertStatus(200, Page).

register_creates_account_and_logs_in(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/register", Session),
    {ok, Resp} = nova_test:post(
        "/register",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"email", ~"Bob@Example.com"},
                {~"username", ~"bob"},
                {~"password", ?PASSWORD},
                {~"password_confirmation", ?PASSWORD}
            ]
        },
        Session
    ),
    ?assertStatus(302, Resp),
    ?assertHeader("location", "/posts", Resp),
    {ok, Page} = nova_test:get("/posts", Session),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(Page), ~"Welcome to the blog!")),
    {ok, #{email := ~"bob@example.com", role := author}} =
        blog_repo:get_by(blog_user, [{username, ~"bob"}]).

register_shows_changeset_errors(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/register", Session),
    {ok, Resp} = nova_test:post(
        "/register",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"email", ~"not-an-email"},
                {~"username", ~"Bob Smith"},
                {~"password", ~"short"},
                {~"password_confirmation", ~"different"}
            ]
        },
        Session
    ),
    ?assertStatus(422, Resp),
    Body = nova_test:body(Resp),
    ?assertNotEqual(nomatch, binary:match(Body, ~"value=\"Bob Smith\"")),
    ?assertNotEqual(
        nomatch, binary:match(Body, ~"<span class=\"error\">has invalid format</span>")
    ),
    ?assertEqual(nomatch, binary:match(Body, ~"value=\"short\"")),
    ?assertEqual(nomatch, binary:match(Body, ~"undefined")).

register_rejects_duplicate_email(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/register", Session),
    {ok, Resp} = nova_test:post(
        "/register",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"email", ~"alice@example.com"},
                {~"username", ~"alice2"},
                {~"password", ?PASSWORD},
                {~"password_confirmation", ?PASSWORD}
            ]
        },
        Session
    ),
    ?assertStatus(422, Resp),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(Resp), ~"has already been taken")).

login_is_rate_limited(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/login", Session),
    Attempt = #{
        form => [
            {~"_csrf_token", nova_test:csrf_token(Form)},
            {~"email", ~"alice@example.com"},
            {~"password", ~"guess"}
        ]
    },
    [{ok, #{status := 422}} = nova_test:post("/login", Attempt, Session) || _ <- lists:seq(1, 5)],
    {ok, Resp} = nova_test:post("/login", Attempt, Session),
    ?assertStatus(429, Resp),
    ?assertMatch([_ | _], nova_test:header("retry-after", Resp)),
    ?assertEqual(#{~"error" => ~"too many requests"}, nova_test:json(Resp)).
