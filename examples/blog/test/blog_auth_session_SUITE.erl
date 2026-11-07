-module(blog_auth_session_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").

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

-define(PASSWORD, ~"correct horse battery").

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
    blog_data_fixtures:start(Config).

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    blog_data_fixtures:reset_rate_limit(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

nova_default_cookie_has_no_flags(_Config) ->
    Req = #{headers => #{}},
    {[], State} = nova_stream_h:init(1, Req, #{stream_handlers => []}),
    #{resp_cookies := #{~"session_id" := Cookie}} = element(3, State),
    [<<"session_id=", _/binary>>] = binary:split(iolist_to_binary(Cookie), ~";", [global]).

first_visit_gets_hardened_cookie(Config) ->
    Session = nova_test:session(Config),
    {ok, Resp} = nova_test:get("/posts", Session),
    ?assertStatus(200, Resp),
    [#{value := Id, attributes := Attributes}] =
        [Cookie || #{name := ~"session_id"} = Cookie <- nova_test:set_cookies(Resp)],
    ?assertCookie(~"session_id", Id, Session),
    ?assertEqual(
        [{~"httponly", ~""}, {~"path", ~"/"}, {~"samesite", ~"Lax"}, {~"secure", ~""}],
        lists:sort(Attributes)
    ),
    ?assertEqual(43, byte_size(Id)).

returning_visitor_keeps_session(Config) ->
    Session = nova_test:session(Config),
    {ok, First} = nova_test:get("/login", Session),
    {ok, Second} = nova_test:get("/login", Session),
    [] = [Cookie || #{name := ~"session_id"} = Cookie <- nova_test:set_cookies(Second)],
    ?assertEqual(nova_test:csrf_token(First), nova_test:csrf_token(Second)).

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

flash_is_shown_once(Config) ->
    Session = nova_test:session(Config),
    {ok, Redirect} = nova_test:get("/posts/new", Session),
    ?assertStatus(302, Redirect),
    Id = nova_test:cookie(~"session_id", Session),
    {ok, Login} = nova_test:get("/login", Session),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(Login), ~"Please log in first.")),
    {ok, Again} = nova_test:get("/login", Session),
    ?assertEqual(nomatch, binary:match(nova_test:body(Again), ~"Please log in first.")),
    ?assertCookie(~"session_id", Id, Session).

login_replaces_attacker_chosen_session(Config) ->
    Planted = nova_test:set_cookie(~"session_id", ~"attacker-knows-this", Config),
    Session = nova_test:session(Planted),
    {ok, Form} = nova_test:get("/login", Session),
    ?assertCookie(~"session_id", ~"attacker-knows-this", Session),
    {ok, Login} = nova_test:post(
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
    ?assertStatus(302, Login),
    ?assertNotEqual(~"attacker-knows-this", nova_test:cookie(~"session_id", Session)),
    {ok, Mine} = nova_test:get("/posts/new", Session),
    ?assertStatus(200, Mine),
    {ok, Attacker} = nova_test:get("/posts/new", Planted),
    ?assertStatus(302, Attacker).

logout_rotates_session(Config) ->
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    Before = nova_test:cookie(~"session_id", Session),
    {ok, Page} = nova_test:get("/posts", Session),
    {ok, Logout} = nova_test:post(
        "/logout", #{form => [{~"_csrf_token", nova_test:csrf_token(Page)}]}, Session
    ),
    ?assertStatus(302, Logout),
    [#{attributes := Attributes}] =
        [Cookie || #{name := ~"session_id"} = Cookie <- nova_test:set_cookies(Logout)],
    ?assert(lists:keymember(~"httponly", 1, Attributes)),
    ?assertNotEqual(Before, nova_test:cookie(~"session_id", Session)).

password_change_ends_other_sessions(Config) ->
    Laptop = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    Phone = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, #{status := 200}} = nova_test:get("/posts/new", Phone),
    {ok, _} = nova_auth_accounts:change_password(
        blog_auth, ?config(alice, Config), ?PASSWORD, #{
            ~"password" => ~"an even better passphrase",
            ~"password_confirmation" => ~"an even better passphrase"
        }
    ),
    {ok, #{status := 302}} = nova_test:get("/posts/new", Laptop),
    {ok, #{status := 302}} = nova_test:get("/posts/new", Phone),
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ~"an even better passphrase"),
    {ok, #{status := 200}} = nova_test:get("/posts/new", Session).

expired_session_token_logs_out(Config) ->
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, [Token]} = blog_repo:all(kura_query:from(blog_user_token)),
    Fifteen = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 15 * 86400
    ),
    {ok, _} = blog_repo:update(
        kura_changeset:cast(blog_user_token, Token, #{inserted_at => Fifteen}, [inserted_at])
    ),
    Req = #{nova_session_id => nova_test:cookie(~"session_id", Session)},
    {ok, #{id := _}} = nova_auth_actor:fetch(Req),
    {ok, Resp} = nova_test:get("/posts/new", Session),
    ?assertStatus(302, Resp),
    ?assertHeader("location", "/login", Resp),
    {error, not_found} = nova_auth_actor:fetch(Req).
