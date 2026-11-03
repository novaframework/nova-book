-module(blog_email_accounts_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
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
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    ok = blog_jobs_fixtures:reset_jobs(),
    blog_jobs_fixtures:capture_mail(),
    blog_web:reset_rate_limit(),
    blog_data_fixtures:checkout(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_jobs_fixtures:testing(disabled),
    blog_data_fixtures:checkin().

registration_mails_a_confirmation_link(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/register"),
    {302, _, _, Jar1} = blog_web:post_form(Jar, "/register", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "carol@example.com"},
        {"username", "carol"},
        {"password", ?PASSWORD},
        {"password_confirmation", ?PASSWORD}
    ]),
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
    {200, Headers, Page, Jar2} = blog_web:get(Jar1, Link),
    "no-referrer" = blog_web:header("referrer-policy", Headers),
    Fields = [{"_csrf_token", blog_web:csrf_token(Page)}, {"token", token_in(Link)}],
    {302, _, _, Jar3} = blog_web:post_form(Jar2, "/confirm", Fields),
    {ok, #{confirmed_at := {{_, _, _}, {_, _, _}}}} = blog_repo:get(blog_user, CarolId),
    {302, _, _, Jar4} = blog_web:post_form(Jar3, "/confirm", Fields),
    {200, _, Posts, _} = blog_web:get(Jar4, "/posts"),
    ?assertNotEqual(nomatch, binary:match(Posts, ~"That link is invalid or has expired.")).

reset_link_changes_the_password(Config) ->
    #{id := AliceId} = ?config(alice, Config),
    {ok, Session} = blog_accounts:issue_token(?config(alice, Config)),
    {200, _, Form, Jar} = blog_web:get(#{}, "/password/forgot"),
    {302, Headers, _, Jar1} = blog_web:post_form(Jar, "/password/forgot", [
        {"_csrf_token", blog_web:csrf_token(Form)}, {"email", " Alice@Example.com "}
    ]),
    "/login" = blog_web:header("location", Headers),
    shigoto_testing:assert_enqueued(#{
        worker => blog_account_email_worker,
        args => #{~"email" => ~"reset", ~"user_id" => AliceId}
    }),
    ok = shigoto:drain_queue(~"mailers"),
    #hikyaku_email{subject = ~"Reset your password", text_body = Text} =
        blog_jobs_fixtures:receive_email(),
    "/password/reset?" ++ _ = Link = link_in(Text),
    {200, _, Page, Jar2} = blog_web:get(Jar1, Link),
    Csrf = blog_web:csrf_token(Page),
    Token = token_in(Link),
    {422, _, Mismatch, Jar3} = blog_web:post_form(Jar2, "/password/reset", [
        {"_csrf_token", Csrf},
        {"token", Token},
        {"password", ?PASSWORD},
        {"password_confirmation", ~"something else entirely"}
    ]),
    ?assertNotEqual(nomatch, binary:match(Mismatch, ~"does not match")),
    Reset = [
        {"_csrf_token", Csrf},
        {"token", Token},
        {"password", ?PASSWORD},
        {"password_confirmation", ?PASSWORD}
    ],
    {302, Done, _, Jar4} = blog_web:post_form(Jar3, "/password/reset", Reset),
    "/login" = blog_web:header("location", Done),
    {ok, #{id := AliceId}} = blog_accounts:authenticate(~"alice@example.com", ?PASSWORD),
    {error, not_found} = blog_accounts:user_for_token(Session),
    {302, Again, _, _} = blog_web:post_form(Jar4, "/password/reset", Reset),
    "/password/forgot" = blog_web:header("location", Again).

unknown_email_gets_the_same_answer(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/password/forgot"),
    {302, Headers, _, Jar1} = blog_web:post_form(Jar, "/password/forgot", [
        {"_csrf_token", blog_web:csrf_token(Form)}, {"email", "nobody@example.com"}
    ]),
    "/login" = blog_web:header("location", Headers),
    {200, _, Login, _} = blog_web:get(Jar1, "/login"),
    ?assertNotEqual(nomatch, binary:match(Login, ~"If that address has an account")),
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
