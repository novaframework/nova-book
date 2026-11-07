-module(blog_email_accounts_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").
-include_lib("hikyaku/include/hikyaku.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    registration_mails_a_confirmation_link/1,
    reset_link_changes_the_password/1,
    unknown_email_gets_the_same_answer/1,
    one_reset_email_per_five_minutes/1,
    no_email_for_confirmed_or_deleted_users/1,
    links_url_encode_the_token/1
]).

-define(PASSWORD, ~"a brand new passphrase").

all() ->
    [
        registration_mails_a_confirmation_link,
        reset_link_changes_the_password,
        unknown_email_gets_the_same_answer,
        one_reset_email_per_five_minutes,
        no_email_for_confirmed_or_deleted_users,
        links_url_encode_the_token
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(Config).

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    ok = blog_jobs_fixtures:reset_jobs(),
    blog_jobs_fixtures:capture_mail(),
    blog_data_fixtures:reset_rate_limit(),
    blog_data_fixtures:checkout(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_jobs_fixtures:testing(disabled),
    blog_data_fixtures:checkin().

registration_mails_a_confirmation_link(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/register", Session),
    {ok, #{status := 302}} = nova_test:post(
        "/register",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"email", ~"carol@example.com"},
                {~"username", ~"carol"},
                {~"password", ?PASSWORD},
                {~"password_confirmation", ?PASSWORD}
            ]
        },
        Session
    ),
    {ok, #{id := CarolId, confirmed_at := undefined}} =
        blog_repo:get_by(blog_user, [{email, ~"carol@example.com"}]),
    shigoto_testing:assert_enqueued(#{
        worker => blog_account_email_worker,
        args => #{~"email" => ~"confirm", ~"user_id" => CarolId}
    }),
    ok = shigoto:drain_queue(~"mailers"),
    #hikyaku_email{
        subject = ~"Confirm your email address",
        to = [#hikyaku_address{address = ~"carol@example.com"}],
        text_body = Text
    } = blog_jobs_fixtures:receive_email(),
    "/confirm?" ++ _ = Link = link_in(Text),
    {ok, Page} = nova_test:get(Link, Session),
    ?assertHeader("referrer-policy", "no-referrer", Page),
    Confirm = #{form => [{~"_csrf_token", nova_test:csrf_token(Page)}, {~"token", token_in(Link)}]},
    {ok, #{status := 302}} = nova_test:post("/confirm", Confirm, Session),
    {ok, #{confirmed_at := {{_, _, _}, {_, _, _}}}} = blog_repo:get(blog_user, CarolId),
    {ok, #{status := 302}} = nova_test:post("/confirm", Confirm, Session),
    {ok, Posts} = nova_test:get("/posts", Session),
    ?assertNotEqual(
        nomatch, binary:match(nova_test:body(Posts), ~"That link is invalid or has expired.")
    ).

reset_link_changes_the_password(Config) ->
    #{id := AliceId} = ?config(alice, Config),
    {ok, ApiToken} = blog_accounts:issue_token(?config(alice, Config)),
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/password/forgot", Session),
    {ok, Forgot} = nova_test:post(
        "/password/forgot",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)}, {~"email", ~" Alice@Example.com "}
            ]
        },
        Session
    ),
    ?assertStatus(302, Forgot),
    ?assertHeader("location", "/login", Forgot),
    shigoto_testing:assert_enqueued(#{
        worker => blog_account_email_worker,
        args => #{~"email" => ~"reset", ~"user_id" => AliceId}
    }),
    ok = shigoto:drain_queue(~"mailers"),
    #hikyaku_email{subject = ~"Reset your password", text_body = Text} =
        blog_jobs_fixtures:receive_email(),
    "/password/reset?" ++ _ = Link = link_in(Text),
    {ok, Page} = nova_test:get(Link, Session),
    Csrf = nova_test:csrf_token(Page),
    Token = token_in(Link),
    Mismatch = [
        {~"_csrf_token", Csrf},
        {~"token", Token},
        {~"password", ?PASSWORD},
        {~"password_confirmation", ~"something else entirely"}
    ],
    {ok, Rejected} = nova_test:post("/password/reset", #{form => Mismatch}, Session),
    ?assertStatus(422, Rejected),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(Rejected), ~"does not match")),
    Reset = #{
        form => [
            {~"_csrf_token", Csrf},
            {~"token", Token},
            {~"password", ?PASSWORD},
            {~"password_confirmation", ?PASSWORD}
        ]
    },
    {ok, Done} = nova_test:post("/password/reset", Reset, Session),
    ?assertStatus(302, Done),
    ?assertHeader("location", "/login", Done),
    {ok, #{id := AliceId}} = blog_accounts:authenticate(~"alice@example.com", ?PASSWORD),
    {error, not_found} = blog_accounts:user_for_token(ApiToken),
    {ok, Again} = nova_test:post("/password/reset", Reset, Session),
    ?assertStatus(302, Again),
    ?assertHeader("location", "/password/forgot", Again).

unknown_email_gets_the_same_answer(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/password/forgot", Session),
    {ok, Forgot} = nova_test:post(
        "/password/forgot",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)}, {~"email", ~"nobody@example.com"}
            ]
        },
        Session
    ),
    ?assertStatus(302, Forgot),
    ?assertHeader("location", "/login", Forgot),
    {ok, Login} = nova_test:get("/login", Session),
    ?assertNotEqual(
        nomatch, binary:match(nova_test:body(Login), ~"If that address has an account")
    ),
    shigoto_testing:refute_enqueued(#{worker => blog_account_email_worker}).

one_reset_email_per_five_minutes(Config) ->
    #{id := AliceId} = ?config(alice, Config),
    ok = blog_accounts:request_password_reset(~"alice@example.com"),
    ok = shigoto:drain_queue(~"mailers"),
    #hikyaku_email{} = blog_jobs_fixtures:receive_email(),
    ok = blog_accounts:request_password_reset(~"alice@example.com"),
    [#{state := ~"completed"}] = shigoto_testing:all_enqueued(#{
        worker => blog_account_email_worker, args => #{~"user_id" => AliceId}
    }),
    ok = shigoto:drain_queue(~"mailers"),
    ok = blog_jobs_fixtures:refute_email().

no_email_for_confirmed_or_deleted_users(Config) ->
    Alice = ?config(alice, Config),
    {ok, Token} = nova_auth_confirm:generate_confirm_token(blog_auth, Alice),
    {ok, #{id := AliceId}} = blog_accounts:confirm(Token),
    Confirm = #{~"email" => ~"confirm", ~"user_id" => AliceId},
    ok = shigoto:perform_job(blog_account_email_worker, Confirm),
    ok = shigoto:perform_job(blog_account_email_worker, Confirm#{~"user_id" => 0}),
    ok = blog_jobs_fixtures:refute_email().

links_url_encode_the_token(Config) ->
    Token = ~"ab+c/d=",
    #hikyaku_email{text_body = Confirm, html_body = Html} =
        blog_emails:confirmation(?config(alice, Config), Token),
    "/confirm?token=ab%2Bc%2Fd%3D" = link_in(Confirm),
    {_, _} = binary:match(Html, ~"/confirm?token=ab%2Bc%2Fd%3D\""),
    #hikyaku_email{text_body = Reset} = blog_emails:password_reset(?config(alice, Config), Token),
    "/password/reset?token=ab%2Bc%2Fd%3D" = link_in(Reset),
    Token = list_to_binary(token_in(link_in(Reset))).

link_in(Text) ->
    {match, [Path]} = re:run(Text, ~"http://localhost:8080(/\\S+)", [{capture, all_but_first, list}]),
    Path.

token_in(Link) ->
    #{query := Query} = uri_string:parse(Link),
    [{"token", Token}] = uri_string:dissect_query(Query),
    Token.
