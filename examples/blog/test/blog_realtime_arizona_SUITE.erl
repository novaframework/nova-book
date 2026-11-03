-module(blog_realtime_arizona_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
    counter_page_is_server_rendered/1,
    counter_renders_without_a_server/1,
    arizona_client_is_self_hosted/1,
    nova_static_files_still_served/1,
    counter_events_update_bindings/1,
    counter_round_trip_over_websocket/1,
    foreign_origin_websocket_is_refused/1,
    editor_requires_login/1,
    editor_websocket_requires_login/1,
    editor_mounts_for_session_user/1,
    editor_validates_posts/1,
    editor_splits_paragraphs_and_counts_words/1,
    editor_preview_over_websocket/1,
    editor_save_reports_errors_and_focuses/1,
    tag_input_normalizes_and_notifies_parent/1,
    tag_input_rejects_invalid_and_extra_tags/1
]).

all() ->
    [{group, counter}, {group, editor}].

groups() ->
    [
        {counter, [], [
            counter_page_is_server_rendered,
            counter_renders_without_a_server,
            arizona_client_is_self_hosted,
            nova_static_files_still_served,
            counter_events_update_bindings,
            counter_round_trip_over_websocket,
            foreign_origin_websocket_is_refused
        ]},
        {editor, [], [
            editor_requires_login,
            editor_websocket_requires_login,
            editor_mounts_for_session_user,
            editor_validates_posts,
            editor_splits_paragraphs_and_counts_words,
            editor_preview_over_websocket,
            editor_save_reports_errors_and_focuses,
            tag_input_normalizes_and_notifies_parent,
            tag_input_rejects_invalid_and_extra_tags
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
    Config.

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

%% Counter

counter_page_is_server_rendered(_Config) ->
    {200, Headers, Body} = http_get("/live/counter"),
    ?assertMatch("text/html" ++ _, proplists:get_value("content-type", Headers)),
    ?assertMatch({match, _}, re:run(Body, ~"<title>Counter - Nova Blog</title>")),
    ?assertMatch({match, _}, re:run(Body, ~"<main az=\"[^\"]+\" az-view id=\"counter\">")),
    ?assertMatch({match, _}, re:run(Body, ~"id=\"count\"><!--az:[^>]+-->0<!--/az-->")),
    ?assertMatch(
        {match, _}, re:run(Body, ~"import \\{ connect \\} from '/assets/arizona/arizona.min.js'")
    ).

counter_renders_without_a_server(_Config) ->
    Html = iolist_to_binary(
        arizona_render:render_view_to_iolist(blog_live_counter, #{
            layouts => [{blog_live_layout, render}]
        })
    ),
    ?assertMatch(
        {match, _}, re:run(Html, ~"<button id=\"inc\" az-click=\"\\[0,&quot;inc&quot;\\]\">")
    ).

arizona_client_is_self_hosted(_Config) ->
    {200, Headers, Body} = http_get("/assets/arizona/arizona.min.js"),
    ?assertEqual("text/javascript", proplists:get_value("content-type", Headers)),
    ?assertMatch({match, _}, re:run(Body, ~"^/\\*! Arizona")).

nova_static_files_still_served(_Config) ->
    {200, Headers, _} = http_get("/assets/favicon.ico"),
    ?assertEqual("image/x-icon", proplists:get_value("content-type", Headers)).

counter_events_update_bindings(_Config) ->
    {Bindings, #{}} = blog_live_counter:mount(#{}),
    ?assertMatch(#{id := ~"counter", count := 0}, Bindings),
    {Inc, #{}, []} = blog_live_counter:handle_event(~"inc", #{}, Bindings),
    ?assertMatch(#{count := 1}, Inc),
    {Dec, #{}, []} = blog_live_counter:handle_event(~"dec", #{}, Inc),
    ?assertMatch(#{count := 0}, Dec).

counter_round_trip_over_websocket(_Config) ->
    {ok, Client} = blog_ws_client:connect("/live/ws?_az_path=/live/counter"),
    #{~"a" := Attrs} = blog_ws_client:recv_json(Client),
    ?assert(lists:member(~"az-click", Attrs)),
    ok = blog_ws_client:send_json(Client, [~"counter", ~"inc", #{}]),
    #{~"o" := [[0, Target1, ~"1"]]} = blog_ws_client:recv_json(Client),
    ?assertMatch(<<"counter:", _/binary>>, Target1),
    ok = blog_ws_client:send_json(Client, [~"counter", ~"inc", #{}]),
    #{~"o" := [[0, Target1, ~"2"]]} = blog_ws_client:recv_json(Client),
    blog_ws_client:close(Client).

foreign_origin_websocket_is_refused(_Config) ->
    ?assertEqual(
        {error, 403},
        blog_ws_client:connect("/live/ws?_az_path=/live/counter", [
            {~"origin", ~"http://evil.example"}
        ])
    ).

%% Editor

editor_requires_login(_Config) ->
    {302, Headers, _} = http_get("/live/editor"),
    ?assertEqual("/login", proplists:get_value("location", Headers)).

editor_websocket_requires_login(_Config) ->
    ?assertEqual({error, 302}, blog_ws_client:connect("/live/ws?_az_path=/live/editor")).

editor_mounts_for_session_user(_Config) ->
    Session = blog_data_fixtures:session(blog_data_fixtures:user(~"alice")),
    {200, _, Body} = http_get("/live/editor", [{"cookie", "session_id=" ++ binary_to_list(Session)}]),
    ?assertMatch({match, _}, re:run(Body, ~"<h1>Write a post</h1>")),
    ?assertMatch({match, _}, re:run(Body, ~"az-view id=\"tags\"")).

editor_validates_posts(_Config) ->
    ?assertEqual(
        #{title => ~"A title is required", body => ~"Write something first"},
        blog_live_editor:validate(~"  ", ~"")
    ),
    ?assertEqual(#{}, blog_live_editor:validate(~"Hello", ~"Body")),
    LongTitle = binary:copy(~"a", 121),
    ?assertEqual(
        #{title => ~"Keep the title under 120 characters"},
        blog_live_editor:validate(LongTitle, ~"Body")
    ).

editor_splits_paragraphs_and_counts_words(_Config) ->
    Body = ~"One two three.\n\nFour five.\r\n\r\n\n\nSix",
    ?assertEqual([~"One two three.", ~"Four five.", ~"Six"], blog_live_editor:paragraphs(Body)),
    ?assertEqual(6, blog_live_editor:word_count(Body)),
    ?assertEqual([], blog_live_editor:paragraphs(~"")).

editor_preview_over_websocket(_Config) ->
    Session = blog_data_fixtures:session(blog_data_fixtures:user(~"alice")),
    {ok, Client} = blog_ws_client:connect("/live/ws?_az_path=/live/editor", [
        {~"cookie", <<"session_id=", Session/binary>>}
    ]),
    #{~"a" := _} = blog_ws_client:recv_json(Client),
    Preview = #{~"title" => ~"Hello <b>", ~"body" => ~"First words\n\nSecond"},
    ok = blog_ws_client:send_json(Client, [~"editor", ~"preview", Preview]),
    #{~"o" := Ops} = blog_ws_client:recv_json(Client),
    Texts = [Text || [0, _Target, Text] <- Ops],
    ?assert(lists:member(~"Hello <b>", Texts)),
    ?assert(lists:member(~"3", Texts)),
    blog_ws_client:close(Client).

editor_save_reports_errors_and_focuses(_Config) ->
    {Bindings, #{}} = blog_live_editor:mount(#{current_user => #{id => 1}}),
    Payload = #{~"title" => ~"", ~"body" => ~"Text"},
    {Invalid, #{}, [Focus]} = blog_live_editor:handle_event(~"save", Payload, Bindings),
    ?assertEqual(#{title => ~"A title is required"}, maps:get(errors, Invalid)),
    ?assertEqual(arizona_js:focus(~"#post-title"), Focus),
    {Cleared, #{}, []} = blog_live_editor:handle_info(clear_status, Invalid#{status => ~"Saved"}),
    ?assertEqual(~"", maps:get(status, Cleared)).

tag_input_normalizes_and_notifies_parent(_Config) ->
    {Bindings, #{}} = blog_live_tag_input:mount(#{id => ~"tags", notify => ~"editor"}),
    {Added, #{}, [Reset]} = blog_live_tag_input:handle_event(
        ~"add_tag", #{~"tag" => ~" Erlang "}, Bindings
    ),
    ?assertEqual([~"erlang"], maps:get(tags, Added)),
    ?assertEqual(arizona_js:reset_form(~"#tag-form"), Reset),
    receive
        {arizona_view, ~"editor", {tags_changed, [~"erlang"]}} -> ok
    after 1000 -> ct:fail(parent_not_notified)
    end,
    {Removed, #{}, []} = blog_live_tag_input:handle_event(
        ~"remove_tag", #{~"tag" => ~"erlang"}, Added
    ),
    ?assertEqual([], maps:get(tags, Removed)).

tag_input_rejects_invalid_and_extra_tags(_Config) ->
    ?assertMatch({error, _}, blog_live_tag_input:normalize(~"two words")),
    Full = #{id => ~"tags", notify => ~"editor", tags => [~"a", ~"b", ~"c", ~"d", ~"e"]},
    {Bindings, #{}} = blog_live_tag_input:mount(Full),
    {Rejected, #{}, []} = blog_live_tag_input:handle_event(~"add_tag", #{~"tag" => ~"f"}, Bindings),
    ?assertEqual(~"A post can have at most 5 tags", maps:get(error, Rejected)),
    ?assertEqual(5, length(maps:get(tags, Rejected))).

%% Helpers

http_get(Path) ->
    http_get(Path, []).

http_get(Path, Headers) ->
    {ok, {{_, Status, _}, RespHeaders, Body}} =
        httpc:request(get, {url(Path), Headers}, [{autoredirect, false}], [{body_format, binary}]),
    {Status, RespHeaders, Body}.

url(Path) ->
    {ok, #{port := Port}} = application:get_env(nova, cowboy_configuration),
    "http://localhost:" ++ integer_to_list(Port) ++ Path.
