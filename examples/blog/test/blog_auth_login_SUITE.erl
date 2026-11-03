-module(blog_auth_login_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

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

-define(PASSWORD, "correct horse battery").

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

login_page_has_csrf_token(_Config) ->
    {200, Headers, Body, _} = blog_web:get(#{}, "/login"),
    ?assertMatch(<<_:8, _/binary>>, blog_web:csrf_token(Body)),
    ?assertEqual("text/html", blog_web:header("content-type", Headers)).

login_without_csrf_token_is_403(_Config) ->
    {200, _, _, Jar} = blog_web:get(#{}, "/login"),
    {403, _, <<"Forbidden - CSRF token invalid">>, _} =
        blog_web:post_form(Jar, "/login", [{"email", "alice@example.com"}, {"password", ?PASSWORD}]).

login_with_wrong_password_rerenders_form(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/login"),
    {422, _, Body, Jar1} = blog_web:post_form(Jar, "/login", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "alice@example.com"},
        {"password", "not the password"}
    ]),
    ?assertNotEqual(nomatch, binary:match(Body, ~"Invalid email or password.")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"value=\"alice@example.com\"")),
    ?assertEqual(nomatch, binary:match(Body, ~"not the password")),
    {302, _, _, _} = blog_web:get(Jar1, "/posts/new").

login_with_unknown_email_rerenders_form(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/login"),
    {422, _, Body, _} = blog_web:post_form(Jar, "/login", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "nobody@example.com"},
        {"password", ?PASSWORD}
    ]),
    ?assertNotEqual(nomatch, binary:match(Body, ~"Invalid email or password.")).

login_sets_session_and_redirects(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/login"),
    AnonymousId = maps:get("session_id", Jar),
    {302, Headers, _, Jar1} = blog_web:post_form(Jar, "/login", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "alice@example.com"},
        {"password", ?PASSWORD}
    ]),
    "/posts" = blog_web:header("location", Headers),
    ?assertNotEqual(AnonymousId, maps:get("session_id", Jar1)),
    {200, _, Page, _} = blog_web:get(Jar1, "/posts"),
    ?assertNotEqual(nomatch, binary:match(Page, ~"Welcome back!")),
    ?assertNotEqual(nomatch, binary:match(Page, ~"<span class=\"who\">alice</span>")),
    {200, _, _, _} = blog_web:get(Jar1, "/posts/new"),
    {302, _, _, _} = blog_web:get(Jar, "/posts/new"),
    Req = #{nova_session_id => list_to_binary(maps:get("session_id", Jar1))},
    {ok, #{id := _, provider := password, username := ~"alice", role := author}} =
        nova_auth_actor:fetch(Req).

email_is_case_insensitive(_Config) ->
    Jar = blog_web:log_in("  Alice@Example.COM ", ?PASSWORD),
    {200, _, _, _} = blog_web:get(Jar, "/posts/new").

logout_revokes_the_session(_Config) ->
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {200, _, Page, _} = blog_web:get(Jar, "/posts"),
    {302, Headers, _, Jar1} =
        blog_web:post_form(Jar, "/logout", [{"_csrf_token", blog_web:csrf_token(Page)}]),
    "/posts" = blog_web:header("location", Headers),
    {200, _, After, _} = blog_web:get(Jar1, "/posts"),
    ?assertNotEqual(nomatch, binary:match(After, ~"You have been logged out.")),
    ?assertNotEqual(nomatch, binary:match(After, ~"href=\"/login\"")),
    {302, NewHeaders, _, _} = blog_web:get(Jar, "/posts/new"),
    "/login" = blog_web:header("location", NewHeaders),
    {ok, []} = blog_repo:all(kura_query:from(blog_user_token)).

logout_requires_csrf_token(_Config) ->
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {403, _, _, _} = blog_web:post_form(Jar, "/logout", []),
    {200, _, _, _} = blog_web:get(Jar, "/posts/new").

register_creates_account_and_logs_in(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/register"),
    {302, Headers, _, Jar1} = blog_web:post_form(Jar, "/register", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "Bob@Example.com"},
        {"username", "bob"},
        {"password", ?PASSWORD},
        {"password_confirmation", ?PASSWORD}
    ]),
    "/posts" = blog_web:header("location", Headers),
    {200, _, Page, _} = blog_web:get(Jar1, "/posts"),
    ?assertNotEqual(nomatch, binary:match(Page, ~"Welcome to the blog!")),
    {ok, #{email := ~"bob@example.com", role := author}} =
        blog_repo:get_by(blog_user, [{username, ~"bob"}]).

register_shows_changeset_errors(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/register"),
    {422, _, Body, _} = blog_web:post_form(Jar, "/register", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "not-an-email"},
        {"username", "Bob Smith"},
        {"password", "short"},
        {"password_confirmation", "different"}
    ]),
    ?assertNotEqual(nomatch, binary:match(Body, ~"value=\"Bob Smith\"")),
    ?assertNotEqual(
        nomatch, binary:match(Body, ~"<span class=\"error\">has invalid format</span>")
    ),
    ?assertEqual(nomatch, binary:match(Body, ~"value=\"short\"")),
    ?assertEqual(nomatch, binary:match(Body, ~"undefined")).

register_rejects_duplicate_email(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/register"),
    {422, _, Body, _} = blog_web:post_form(Jar, "/register", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "alice@example.com"},
        {"username", "alice2"},
        {"password", ?PASSWORD},
        {"password_confirmation", ?PASSWORD}
    ]),
    ?assertNotEqual(nomatch, binary:match(Body, ~"has already been taken")).

login_is_rate_limited(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/login"),
    Attempt = [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "alice@example.com"},
        {"password", "guess"}
    ],
    [{422, _, _, _} = blog_web:post_form(Jar, "/login", Attempt) || _ <- lists:seq(1, 5)],
    {429, Headers, Body, _} = blog_web:post_form(Jar, "/login", Attempt),
    ?assertMatch([_ | _], blog_web:header("retry-after", Headers)),
    ?assertEqual(#{~"error" => ~"too many requests"}, json:decode(Body)).
