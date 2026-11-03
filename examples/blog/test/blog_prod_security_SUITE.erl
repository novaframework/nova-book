-module(blog_prod_security_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    drafts_are_hidden_from_anonymous_readers/1,
    drafts_are_shown_to_their_author/1,
    drafts_are_hidden_from_other_users/1,
    admins_see_every_draft/1,
    bad_token_on_a_public_read_is_401/1,
    live_pages_reject_a_revoked_token/1,
    live_pages_ignore_a_cached_actor_without_a_token/1,
    live_websocket_rejects_a_revoked_token/1
]).

all() ->
    [
        drafts_are_hidden_from_anonymous_readers,
        drafts_are_shown_to_their_author,
        drafts_are_hidden_from_other_users,
        admins_see_every_draft,
        bad_token_on_a_public_read_is_401,
        live_pages_reject_a_revoked_token,
        live_pages_ignore_a_cached_actor_without_a_token,
        live_websocket_rejects_a_revoked_token
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    {ok, _} = application:ensure_all_started(gun),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    Alice = blog_data_fixtures:user(~"alice"),
    Draft = blog_data_fixtures:post(Alice, #{~"title" => ~"Secret draft"}),
    Published = blog_data_fixtures:post(Alice, #{
        ~"title" => ~"Out now", ~"status" => ~"published"
    }),
    [{alice, Alice}, {draft, Draft}, {published, Published} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

drafts_are_hidden_from_anonymous_readers(Config) ->
    {404, #{~"error" := ~"post not found"}} =
        blog_data_fixtures:request(get, post_path(?config(draft, Config))),
    {200, _} = blog_data_fixtures:request(get, post_path(?config(published, Config))),
    {200, #{~"posts" := [#{~"title" := ~"Out now"}]}} =
        blog_data_fixtures:request(get, "/api/posts"),
    {200, #{~"posts" := [], ~"total_entries" := 0}} =
        blog_data_fixtures:request(get, "/api/posts?status=draft").

drafts_are_shown_to_their_author(Config) ->
    Token = blog_data_fixtures:token(?config(alice, Config)),
    {200, #{~"title" := ~"Secret draft"}} =
        blog_data_fixtures:request_as(Token, get, post_path(?config(draft, Config))),
    {200, #{~"posts" := [#{~"title" := ~"Secret draft"}]}} =
        blog_data_fixtures:request_as(Token, get, "/api/posts?status=draft"),
    {200, #{~"total_entries" := 2}} = blog_data_fixtures:request_as(Token, get, "/api/posts").

drafts_are_hidden_from_other_users(Config) ->
    Bob = blog_data_fixtures:user(~"bob"),
    _ = blog_data_fixtures:post(Bob, #{~"title" => ~"Bob's draft"}),
    Token = blog_data_fixtures:token(Bob),
    {404, _} = blog_data_fixtures:request_as(Token, get, post_path(?config(draft, Config))),
    {200, #{~"posts" := [#{~"title" := ~"Bob's draft"}]}} =
        blog_data_fixtures:request_as(Token, get, "/api/posts?status=draft").

admins_see_every_draft(Config) ->
    Token = blog_data_fixtures:token(blog_data_fixtures:admin(~"carol")),
    {200, #{~"title" := ~"Secret draft"}} =
        blog_data_fixtures:request_as(Token, get, post_path(?config(draft, Config))),
    {200, #{~"total_entries" := 1}} =
        blog_data_fixtures:request_as(Token, get, "/api/posts?status=draft").

bad_token_on_a_public_read_is_401(Config) ->
    {401, #{~"error" := ~"invalid or expired token"}} =
        blog_data_fixtures:request_as(~"forged", get, post_path(?config(published, Config))).

live_pages_reject_a_revoked_token(Config) ->
    Alice = ?config(alice, Config),
    {ok, SessionId} = nova_session:generate_session_id(),
    Token = blog_data_fixtures:token(Alice),
    ok = nova_session_ets:set_value(SessionId, ~"user_token", Token),
    ok = nova_session_ets:set_value(
        SessionId, nova_auth_actor:session_key(), term_to_binary(blog_accounts:actor(Alice))
    ),
    {200, _} = live_get("/live/editor", SessionId),
    ok = blog_accounts:revoke_token(Token),
    {302, Location} = live_get("/live/editor", SessionId),
    ?assertEqual("/login", Location),
    ?assertEqual(
        {error, not_found}, nova_session_ets:get_value(SessionId, nova_auth_actor:session_key())
    ).

live_pages_ignore_a_cached_actor_without_a_token(Config) ->
    {ok, SessionId} = nova_session:generate_session_id(),
    Actor = blog_accounts:actor(?config(alice, Config)),
    ok = nova_session_ets:set_value(
        SessionId, nova_auth_actor:session_key(), term_to_binary(Actor)
    ),
    {302, "/login"} = live_get("/live/editor", SessionId).

live_websocket_rejects_a_revoked_token(Config) ->
    Alice = ?config(alice, Config),
    Token = blog_data_fixtures:token(Alice),
    {ok, SessionId} = nova_session:generate_session_id(),
    ok = nova_session_ets:set_value(SessionId, ~"user_token", Token),
    Cookie = [{~"cookie", <<"session_id=", SessionId/binary>>}],
    {ok, Client} = blog_ws_client:connect("/live/ws?_az_path=/live/editor", Cookie),
    blog_ws_client:close(Client),
    ok = blog_accounts:revoke_token(Token),
    ?assertEqual(
        {error, 302}, blog_ws_client:connect("/live/ws?_az_path=/live/editor", Cookie)
    ).

post_path(#{id := Id}) ->
    "/api/posts/" ++ integer_to_list(Id).

live_get(Path, SessionId) ->
    Headers = [{"cookie", "session_id=" ++ binary_to_list(SessionId)}],
    {ok, {{_, Status, _}, RespHeaders, _Body}} = httpc:request(
        get, {blog_data_fixtures:url(Path), Headers}, [{autoredirect, false}], []
    ),
    {Status, proplists:get_value("location", RespHeaders)}.
