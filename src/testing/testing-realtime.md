# Testing Real-Time

WebSocket handlers and live views need different testing approaches than HTTP endpoints. This chapter covers strategies for testing both.

## Testing WebSocket handlers

WebSocket handlers are Erlang modules with callbacks. Test them by calling the callbacks directly:

```erlang
-module(blog_ws_handler_tests).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    {ok, State} = blog_ws_handler:init(#{}),
    ?assertMatch(#{}, State).

echo_test() ->
    {reply, {text, Reply}, _State} =
        blog_ws_handler:websocket_handle({text, <<"hello">>}, #{}),
    ?assertEqual(<<"Echo: hello">>, Reply).

ignore_binary_frames_test() ->
    {ok, _State} =
        blog_ws_handler:websocket_handle({binary, <<1,2,3>>}, #{}),
    ok.
```

### Testing PubSub integration

For handlers that use PubSub, verify that messages are forwarded:

```erlang
-module(blog_feed_handler_tests).
-include_lib("eunit/include/eunit.hrl").

pubsub_message_forwarded_test() ->
    State = #{},
    Msg = {nova_pubsub, posts, self(), "post_created", #{id => 1, title => <<"Test">>}},
    {reply, {text, Json}, _State} =
        blog_feed_handler:websocket_info(Msg, State),
    Decoded = thoas:decode(Json),
    ?assertMatch({ok, #{<<"channel">> := <<"posts">>}}, Decoded).
```

## Integration testing WebSockets

For end-to-end WebSocket tests, use `gun` (an Erlang HTTP/WebSocket client):

```erlang
-module(blog_ws_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         test_ws_echo/1, test_ws_feed/1]).

all() -> [test_ws_echo, test_ws_feed].

init_per_suite(Config) ->
    application:ensure_all_started(gun),
    nova_test:start(blog, Config).

end_per_suite(Config) ->
    nova_test:stop(Config).

test_ws_echo(Config) ->
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive {gun_upgrade, ConnPid, StreamRef, _, _} -> ok end,

    gun:ws_send(ConnPid, StreamRef, {text, <<"hello">>}),
    receive
        {gun_ws, ConnPid, StreamRef, {text, Reply}} ->
            <<"Echo: hello">> = Reply
    after 1000 ->
        ct:fail("No WebSocket response")
    end,
    gun:close(ConnPid).

test_ws_feed(_Config) ->
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, "/feed"),
    receive {gun_upgrade, ConnPid, StreamRef, _, _} -> ok end,

    %% Trigger a broadcast
    nova_pubsub:broadcast(posts, "post_created", #{id => 99, title => <<"Test">>}),

    receive
        {gun_ws, ConnPid, StreamRef, {text, Json}} ->
            {ok, Decoded} = thoas:decode(Json),
            #{<<"event">> := <<"post_created">>} = Decoded
    after 2000 ->
        ct:fail("No feed message received")
    end,
    gun:close(ConnPid).
```

## Testing Arizona live views

Arizona live views use opaque state types (`arizona_view` and `arizona_stateful`), so unit testing callbacks directly requires constructing views with `arizona_view:new/3`. Test the mount and event callbacks:

```erlang
-module(blog_counter_live_tests).
-include_lib("eunit/include/eunit.hrl").

mount_test() ->
    View = blog_counter_live:mount(#{}, undefined),
    State = arizona_view:get_state(View),
    ?assertEqual(0, arizona_stateful:get_binding(count, State)).

increment_test() ->
    View = blog_counter_live:mount(#{}, undefined),
    {_Actions, View1} =
        blog_counter_live:handle_event(<<"increment">>, #{}, View),
    State = arizona_view:get_state(View1),
    ?assertEqual(1, arizona_stateful:get_binding(count, State)).

decrement_test() ->
    View = blog_counter_live:mount(#{}, undefined),
    {_Actions, View1} =
        blog_counter_live:handle_event(<<"decrement">>, #{}, View),
    State = arizona_view:get_state(View1),
    ?assertEqual(-1, arizona_stateful:get_binding(count, State)).
```

### Testing live view rendering

Verify that `render/1` produces expected content:

```erlang
render_shows_count_test() ->
    Html = blog_counter_live:render(#{count => 42}),
    %% Check that the rendered output contains the count
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Html), <<"42">>)).
```

### Testing PubSub in live views

```erlang
handle_info_new_comment_test() ->
    Comment = #{id => 1, body => <<"Nice!">>, author => #{username => <<"bob">>}},
    %% Build a view with empty comments
    View = arizona_view:new(blog_post_live, #{
        id => ~"test", comments => [], post => #{}, new_comment => <<>>,
        channel => test_channel
    }, none),
    Msg = {nova_pubsub, comments_1, self(), "new_comment", Comment},
    {_Actions, NewView} = blog_post_live:handle_info(Msg, View),
    State = arizona_view:get_state(NewView),
    ?assertEqual([Comment], arizona_stateful:get_binding(comments, State)).
```

## Test structure

```
test/
├── post_changeset_tests.erl              %% EUnit — changeset validation
├── blog_posts_controller_tests.erl       %% EUnit — controller unit tests
├── blog_auth_tests.erl                   %% EUnit — security functions
├── blog_ws_handler_tests.erl             %% EUnit — WebSocket handler unit tests
├── blog_counter_live_tests.erl           %% EUnit — live view unit tests
├── blog_api_SUITE.erl                    %% CT — API integration tests
└── blog_ws_SUITE.erl                     %% CT — WebSocket integration tests
```

---

With testing covered, let's prepare for production. Next: [Configuration](../production/configuration.md).
