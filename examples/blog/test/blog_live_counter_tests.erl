-module(blog_live_counter_tests).
-include_lib("eunit/include/eunit.hrl").

events_change_the_count_test() ->
    {Bindings, #{}} = blog_live_counter:mount(#{}),
    {Inc, #{}, []} = blog_live_counter:handle_event(~"inc", #{}, Bindings),
    {Dec, #{}, []} = blog_live_counter:handle_event(~"dec", #{}, Inc),
    ?assertEqual([0, 1, 0], [maps:get(count, B) || B <- [Bindings, Inc, Dec]]).

renders_without_a_server_test() ->
    Html = iolist_to_binary(
        arizona_render:render_view_to_iolist(blog_live_counter, #{
            layouts => [{blog_live_layout, render}]
        })
    ),
    ?assertMatch({match, _}, re:run(Html, ~"<title>Counter - Nova Blog</title>")),
    ?assertMatch({match, _}, re:run(Html, ~"id=\"count\"><!--az:[^>]+-->0<!--/az-->")).
