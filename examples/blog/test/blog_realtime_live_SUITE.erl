-module(blog_realtime_live_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("nova/include/nova_pubsub.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
    post_page_renders_comments/1,
    only_published_posts_are_shown/1,
    signed_in_reader_gets_a_comment_form/1,
    comment_reaches_every_reader/1,
    anonymous_comment_event_is_ignored/1,
    invalid_comment_reports_error_and_focuses/1,
    broadcasts_are_merged_idempotently/1,
    api_comments_are_broadcast/1,
    editor_saves_and_updates_a_draft/1,
    editor_reports_changeset_errors/1,
    editor_reports_a_stale_draft/1,
    editor_publish_navigates_with_flash/1,
    editor_opens_only_own_posts/1,
    dashboard_requires_login/1,
    dashboard_lists_posts_by_section/1
]).

all() ->
    [{group, comments}, {group, editor}, {group, dashboard}].

groups() ->
    [
        {comments, [], [
            post_page_renders_comments,
            only_published_posts_are_shown,
            signed_in_reader_gets_a_comment_form,
            comment_reaches_every_reader,
            anonymous_comment_event_is_ignored,
            invalid_comment_reports_error_and_focuses,
            broadcasts_are_merged_idempotently,
            api_comments_are_broadcast
        ]},
        {editor, [], [
            editor_saves_and_updates_a_draft,
            editor_reports_changeset_errors,
            editor_reports_a_stale_draft,
            editor_publish_navigates_with_flash,
            editor_opens_only_own_posts
        ]},
        {dashboard, [], [
            dashboard_requires_login,
            dashboard_lists_posts_by_section
        ]}
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
    Bob = blog_data_fixtures:user(~"bob"),
    Draft = blog_data_fixtures:post(Alice, #{~"title" => ~"Hello live", ~"body" => ~"One\n\nTwo"}),
    {ok, Post} = blog_posts:publish(Draft, []),
    [{alice, Alice}, {bob, Bob}, {post, Post} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

%% Comments

post_page_renders_comments(Config) ->
    Post = ?config(post, Config),
    {ok, _} = blog_posts:add_comment(Post, ?config(bob, Config), #{~"body" => ~"Great <post>"}),
    {200, Body} = page(Post),
    ?assertMatch({match, _}, re:run(Body, ~"<title>Hello live - Nova Blog</title>")),
    ?assertMatch({match, _}, re:run(Body, ~"<strong[^>]*>(<!--[^>]+-->)?bob<")),
    ?assertMatch({match, _}, re:run(Body, ~"Great &lt;post&gt;")),
    ?assertMatch({match, _}, re:run(Body, ~"id=\"comment-login\"")),
    ?assertEqual(nomatch, re:run(Body, ~"id=\"comment-form\"")).

only_published_posts_are_shown(Config) ->
    Draft = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Not yet"}),
    ?assertMatch({404, _}, page(Draft)),
    ?assertEqual(
        {error, 404}, blog_ws_client:connect("/live/ws?_az_path=" ++ live_path(Draft))
    ),
    ?assertMatch({404, _}, http_get("/live/posts/999999", [])),
    ?assertMatch({404, _}, http_get("/live/posts/abc", [])).

signed_in_reader_gets_a_comment_form(Config) ->
    {200, Body} = page(?config(post, Config), ?config(bob, Config)),
    ?assertMatch({match, _}, re:run(Body, ~"<form [^>]*id=\"comment-form\"")),
    ?assertEqual(nomatch, re:run(Body, ~"id=\"comment-login\"")).

comment_reaches_every_reader(Config) ->
    #{id := PostId} = Post = ?config(post, Config),
    ok = nova_pubsub:join(blog_notify:channel()),
    {ok, Bob} = connect(Post, ?config(bob, Config)),
    {ok, Reader} = connect(Post, undefined),
    ok = blog_ws_client:send_json(Bob, [~"post", ~"add_comment", #{~"body" => ~"Hi from Bob"}]),
    BobFrame = recv_text(Bob),
    ?assertMatch({_, _}, binary:match(BobFrame, ~"Hi from Bob")),
    ?assertMatch({_, _}, binary:match(BobFrame, ~"#comment-form")),
    ?assertMatch({_, _}, binary:match(recv_text(Reader), ~"Hi from Bob")),
    receive
        #nova_pubsub{topic = ~"comment_created", payload = Payload} ->
            ?assertEqual(
                #{~"post_id" => PostId, ~"post_title" => ~"Hello live", ~"author" => ~"bob"},
                Payload
            )
    after 2000 -> ct:fail(no_notification)
    end,
    {ok, #{comments := [#{body := ~"Hi from Bob"}]}} = blog_posts:get_post(PostId, [comments]),
    blog_ws_client:close(Bob),
    blog_ws_client:close(Reader).

anonymous_comment_event_is_ignored(Config) ->
    #{id := PostId} = Post = ?config(post, Config),
    {Bindings, #{}} = blog_live_post:mount(#{post => with_author(Post), current_user => undefined}),
    ?assertEqual(
        {Bindings, #{}, []},
        blog_live_post:handle_event(~"add_comment", #{~"body" => ~"spam"}, Bindings)
    ),
    ?assertMatch({ok, #{comments := []}}, blog_posts:get_post(PostId, [comments])).

invalid_comment_reports_error_and_focuses(Config) ->
    Post = with_author(?config(post, Config)),
    {Bindings, #{}} = blog_live_post:mount(#{
        post => Post, current_user => actor(?config(bob, Config))
    }),
    {Invalid, #{}, [Focus]} =
        blog_live_post:handle_event(~"add_comment", #{~"body" => ~""}, Bindings),
    ?assertEqual(~"can't be blank", maps:get(comment_error, Invalid)),
    ?assertEqual(arizona_js:focus(~"#comment-body"), Focus),
    ?assertEqual([], arizona_stream:to_list(maps:get(comments, Invalid))).

broadcasts_are_merged_idempotently(Config) ->
    Post = with_author(?config(post, Config)),
    {Bindings, #{}} = blog_live_post:mount(#{post => Post, current_user => undefined}),
    Comment = #{id => 7, body => ~"Hi", author => ~"bob", inserted_at => ~"2026-11-03T08:00:00Z"},
    Other = whereis(blog_notify_log),
    Envelope = #nova_pubsub{
        channel = blog_notify:post_channel(maps:get(id, Post)),
        sender = Other,
        topic = ~"comment_created",
        payload = Comment
    },
    {Once, #{}, []} = blog_live_post:handle_info(Envelope, Bindings),
    {Twice, #{}, []} = blog_live_post:handle_info(Envelope, Once),
    ?assertEqual([Comment], arizona_stream:to_list(maps:get(comments, Twice))),
    Own = Envelope#nova_pubsub{sender = self(), payload = Comment#{id => 8}},
    ?assertEqual({Twice, #{}, []}, blog_live_post:handle_info(Own, Twice)).

api_comments_are_broadcast(Config) ->
    #{id := PostId} = ?config(post, Config),
    #{id := BobId} = ?config(bob, Config),
    ok = nova_pubsub:join(blog_notify:post_channel(PostId)),
    Path = "/api/posts/" ++ integer_to_list(PostId) ++ "/comments",
    {201, #{~"id" := CommentId}} =
        blog_data_fixtures:request(post, Path, #{~"user_id" => BobId, ~"body" => ~"Via the API"}),
    receive
        #nova_pubsub{topic = ~"comment_created", payload = #{id := Id, author := Author}} ->
            ?assertEqual({CommentId, ~"bob"}, {Id, Author})
    after 2000 -> ct:fail(no_broadcast)
    end.

%% Editor

editor_saves_and_updates_a_draft(Config) ->
    Alice = actor(?config(alice, Config)),
    {Bindings, #{}} = blog_live_editor:mount(#{current_user => Alice}),
    First = #{~"title" => ~"My draft", ~"body" => ~"Text", ~"intent" => ~"save"},
    {Saved, #{}, [Title]} = blog_live_editor:handle_event(~"save", First, Bindings),
    ?assertEqual(arizona_js:set_title(~"My draft"), Title),
    #{post := #{id := Id, status := draft, lock_version := Version}} = Saved,
    ?assertEqual(~"Draft saved", maps:get(status, Saved)),
    Second = First#{~"title" => ~"My draft, revised"},
    {Updated, #{}, _} = blog_live_editor:handle_event(~"save", Second, Saved),
    #{post := #{id := Id, title := ~"My draft, revised", lock_version := Next}} = Updated,
    ?assertEqual(Version + 1, Next),
    receive
        {arizona_view, ~"editor", clear_status} -> ok
    after 4000 -> ct:fail(status_not_cleared)
    end.

editor_reports_changeset_errors(Config) ->
    {Bindings, #{}} = blog_live_editor:mount(#{current_user => actor(?config(alice, Config))}),
    {Invalid, #{}, [Focus]} =
        blog_live_editor:handle_event(~"save", #{~"title" => ~"Hi", ~"body" => ~"Text"}, Bindings),
    ?assertMatch(#{title := <<"should be at least 3", _/binary>>}, maps:get(errors, Invalid)),
    ?assertEqual(arizona_js:focus(~"#post-title"), Focus),
    ?assertEqual(undefined, maps:get(post, Invalid)).

editor_reports_a_stale_draft(Config) ->
    Alice = actor(?config(alice, Config)),
    {Bindings, #{}} = blog_live_editor:mount(#{current_user => Alice}),
    Params = #{~"title" => ~"Shared", ~"body" => ~"Text"},
    {#{post := Post} = Saved, #{}, _} = blog_live_editor:handle_event(~"save", Params, Bindings),
    {ok, _} = blog_posts:update_post(Post, #{~"title" => ~"Changed elsewhere"}),
    {Stale, #{}, []} = blog_live_editor:handle_event(~"save", Params, Saved),
    ?assertEqual(~"Someone else changed this post. Reload to see it.", maps:get(status, Stale)).

editor_publish_navigates_with_flash(Config) ->
    ok = nova_pubsub:join(blog_notify:channel()),
    {Bindings, #{}} = blog_live_editor:mount(#{current_user => actor(?config(alice, Config))}),
    Params = #{~"title" => ~"Going live", ~"body" => ~"Text", ~"intent" => ~"publish"},
    {#{post := #{id := Id}}, #{}, [Navigate]} =
        blog_live_editor:handle_event(~"save", Params, Bindings#{tags => [~"erlang"]}),
    Path = <<"/live/posts/", (integer_to_binary(Id))/binary>>,
    ?assertEqual(
        arizona_js:navigate(Path, #{flash => #{~"info" => ~"Your post is live"}}), Navigate
    ),
    {ok, #{status := published, tags := [#{name := ~"erlang"}]}} = blog_posts:get_post(Id, [tags]),
    receive
        #nova_pubsub{topic = ~"post_published", payload = Payload} ->
            ?assertEqual(#{~"id" => Id, ~"title" => ~"Going live"}, Payload)
    after 2000 -> ct:fail(no_notification)
    end.

editor_opens_only_own_posts(Config) ->
    Draft = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Half done"}),
    Path = "/live/editor/" ++ integer_to_list(maps:get(id, Draft)),
    {200, Body} = http_get(Path, cookie(?config(alice, Config))),
    ?assertMatch({match, _}, re:run(Body, ~"id=\"post-title\" name=\"title\" value=\"Half done\"")),
    ?assertMatch({404, _}, http_get(Path, cookie(?config(bob, Config)))).

%% Dashboard

dashboard_requires_login(_Config) ->
    ?assertMatch({302, _}, http_get("/live/dashboard/drafts", [])).

dashboard_lists_posts_by_section(Config) ->
    Alice = ?config(alice, Config),
    Draft = blog_data_fixtures:post(Alice, #{~"title" => ~"Draft one"}),
    _ = blog_data_fixtures:post(?config(bob, Config), #{~"title" => ~"Not mine"}),
    Mount = #{current_user => actor(Alice), ~"section" => ~"published"},
    {Published, #{}} = blog_live_dashboard:mount(Mount),
    ?assertEqual([~"Hello live"], titles(Published)),
    {Drafts, #{}, [Title]} =
        blog_live_dashboard:handle_update(#{~"section" => ~"drafts"}, Published, []),
    ?assertEqual([maps:get(title, Draft)], titles(Drafts)),
    ?assertEqual(arizona_js:set_title(~"Your drafts - Nova Blog"), Title),
    ?assertEqual(draft, blog_live_dashboard:section(~"anything-else")).

%% Helpers

titles(#{posts := Posts}) ->
    [Title || #{title := Title} <- Posts].

with_author(Post) ->
    blog_repo:preload(blog_post, Post, [author]).

actor(#{id := Id}) ->
    #{id => Id, provider => password}.

login(User) ->
    {ok, SessionId} = nova_session:generate_session_id(),
    ok = nova_session_ets:set_value(
        SessionId, nova_auth_actor:session_key(), term_to_binary(actor(User))
    ),
    SessionId.

cookie(User) ->
    [{"cookie", "session_id=" ++ binary_to_list(login(User))}].

page(Post) ->
    http_get(live_path(Post), []).

page(Post, User) ->
    http_get(live_path(Post), cookie(User)).

live_path(Post) ->
    binary_to_list(blog_live_post:path(Post)).

connect(Post, undefined) ->
    connect_ws(Post, []);
connect(Post, User) ->
    connect_ws(Post, [{~"cookie", <<"session_id=", (login(User))/binary>>}]).

connect_ws(Post, Headers) ->
    {ok, Client} = blog_ws_client:connect("/live/ws?_az_path=" ++ live_path(Post), Headers),
    #{~"a" := _} = blog_ws_client:recv_json(Client),
    {ok, Client}.

recv_text(Client) ->
    {text, Text} = blog_ws_client:recv(Client),
    Text.

http_get(Path, Headers) ->
    {ok, {{_, Status, _}, _, Body}} = httpc:request(
        get, {blog_data_fixtures:url(Path), Headers}, [{autoredirect, false}], [
            {body_format, binary}
        ]
    ),
    {Status, Body}.
