-module(blog_auth_session_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    nova_default_cookie_has_no_flags/1,
    first_visit_gets_hardened_cookie/1,
    returning_visitor_keeps_session/1,
    session_api_round_trip/1,
    flash_is_shown_once/1,
    login_replaces_attacker_chosen_session/1,
    logout_rotates_session/1,
    password_change_ends_other_sessions/1,
    expired_session_token_logs_out/1
]).

-define(PASSWORD, "correct horse battery").

all() ->
    [
        nova_default_cookie_has_no_flags,
        first_visit_gets_hardened_cookie,
        returning_visitor_keeps_session,
        session_api_round_trip,
        flash_is_shown_once,
        login_replaces_attacker_chosen_session,
        logout_rotates_session,
        password_change_ends_other_sessions,
        expired_session_token_logs_out
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

nova_default_cookie_has_no_flags(_Config) ->
    Req = #{headers => #{}},
    {[], State} = nova_stream_h:init(1, Req, #{stream_handlers => []}),
    #{resp_cookies := #{~"session_id" := Cookie}} = element(3, State),
    [<<"session_id=", _/binary>>] = binary:split(iolist_to_binary(Cookie), ~";", [global]).

first_visit_gets_hardened_cookie(_Config) ->
    {200, Headers, _, Jar} = blog_web:get(#{}, "/posts"),
    [Cookie] = blog_web:set_cookie("session_id", Headers),
    #{"session_id" := Id} = Jar,
    "session_id=" ++ Rest = Cookie,
    [Id | Attributes] = [string:trim(A) || A <- string:split(Rest, ";", all)],
    ?assertEqual(["HttpOnly", "Path=/", "SameSite=Lax", "Secure"], lists:sort(Attributes)),
    ?assertEqual(43, length(Id)).

returning_visitor_keeps_session(_Config) ->
    {200, _, First, Jar} = blog_web:get(#{}, "/login"),
    {200, Headers, Second, Jar} = blog_web:get(Jar, "/login"),
    [] = blog_web:set_cookie("session_id", Headers),
    ?assertEqual(blog_web:csrf_token(First), blog_web:csrf_token(Second)).

session_api_round_trip(_Config) ->
    Req = #{nova_session_id => blog_session:new_id()},
    {error, not_found} = nova_session:get(Req, ~"theme"),
    ok = nova_session:set(Req, ~"theme", ~"dark"),
    {ok, ~"dark"} = nova_session:get(Req, ~"theme"),
    {ok, Req} = nova_session:delete(Req, ~"theme"),
    {error, not_found} = nova_session:get(Req, ~"theme"),
    ok = nova_session:set(Req, ~"theme", ~"light"),
    {ok, Req1} = nova_session:delete(Req),
    {error, not_found} = nova_session:get(Req, ~"theme"),
    #{resp_cookies := #{~"session_id" := Expired}} = Req1,
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Expired), ~"Max-Age=0")),
    {error, session_id_not_set} = nova_session:set(#{headers => #{}}, ~"theme", ~"dark").

flash_is_shown_once(_Config) ->
    {302, _, _, Jar} = blog_web:get(#{}, "/posts/new"),
    {200, _, Login, Jar} = blog_web:get(Jar, "/login"),
    ?assertNotEqual(nomatch, binary:match(Login, ~"Please log in first.")),
    {200, _, Again, Jar} = blog_web:get(Jar, "/login"),
    ?assertEqual(nomatch, binary:match(Again, ~"Please log in first.")).

login_replaces_attacker_chosen_session(_Config) ->
    Planted = #{"session_id" => "attacker-knows-this"},
    {200, _, Form, Planted} = blog_web:get(Planted, "/login"),
    {302, _, _, Jar} = blog_web:post_form(Planted, "/login", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "alice@example.com"},
        {"password", ?PASSWORD}
    ]),
    ?assertNotEqual("attacker-knows-this", maps:get("session_id", Jar)),
    {200, _, _, _} = blog_web:get(Jar, "/posts/new"),
    {302, _, _, _} = blog_web:get(Planted, "/posts/new").

logout_rotates_session(_Config) ->
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {200, _, Page, _} = blog_web:get(Jar, "/posts"),
    {302, Headers, _, Jar1} =
        blog_web:post_form(Jar, "/logout", [{"_csrf_token", blog_web:csrf_token(Page)}]),
    [Cookie] = blog_web:set_cookie("session_id", Headers),
    ?assertNotEqual(nomatch, string:find(Cookie, "HttpOnly")),
    ?assertNotEqual(maps:get("session_id", Jar), maps:get("session_id", Jar1)).

password_change_ends_other_sessions(Config) ->
    Laptop = blog_web:log_in("alice@example.com", ?PASSWORD),
    Phone = blog_web:log_in("alice@example.com", ?PASSWORD),
    {200, _, _, _} = blog_web:get(Phone, "/posts/new"),
    {ok, _} = nova_auth_accounts:change_password(
        blog_auth, ?config(alice, Config), ~"correct horse battery", #{
            ~"password" => ~"an even better passphrase",
            ~"password_confirmation" => ~"an even better passphrase"
        }
    ),
    {302, _, _, _} = blog_web:get(Laptop, "/posts/new"),
    {302, _, _, _} = blog_web:get(Phone, "/posts/new"),
    Jar = blog_web:log_in("alice@example.com", "an even better passphrase"),
    {200, _, _, _} = blog_web:get(Jar, "/posts/new").

expired_session_token_logs_out(_Config) ->
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {ok, [Token]} = blog_repo:all(kura_query:from(blog_user_token)),
    Fifteen = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 15 * 86400
    ),
    {ok, _} = blog_repo:update(
        kura_changeset:cast(blog_user_token, Token, #{inserted_at => Fifteen}, [inserted_at])
    ),
    Session = #{nova_session_id => list_to_binary(maps:get("session_id", Jar))},
    {ok, #{id := _}} = nova_auth_actor:fetch(Session),
    {302, Headers, _, _} = blog_web:get(Jar, "/posts/new"),
    "/login" = blog_web:header("location", Headers),
    {error, not_found} = nova_auth_actor:fetch(Session).
