-module(blog_realtime_ws_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("nova/include/nova_pubsub.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([
    echo_replies_to_text_frames/1,
    echo_keeps_state_per_connection/1,
    foreign_origin_is_refused/1,
    missing_origin_is_allowed/1,
    security_auth_data_reaches_handler/1,
    pubsub_delivers_an_envelope/1,
    pubsub_leave_stops_delivery/1,
    notifications_are_pushed_to_clients/1,
    notifications_replay_recent_on_connect/1,
    notify_log_keeps_the_last_ten/1,
    post_channels_are_per_post/1,
    broadcast_reaches_other_nodes/1
]).
-export([relay/2]).

all() ->
    [{group, websockets}, {group, pubsub}, {group, distributed}].

groups() ->
    [
        {websockets, [], [
            echo_replies_to_text_frames,
            echo_keeps_state_per_connection,
            foreign_origin_is_refused,
            missing_origin_is_allowed,
            security_auth_data_reaches_handler
        ]},
        {pubsub, [], [
            pubsub_delivers_an_envelope,
            pubsub_leave_stops_delivery,
            notifications_are_pushed_to_clients,
            notifications_replay_recent_on_connect,
            notify_log_keeps_the_last_ten,
            post_channels_are_per_post
        ]},
        {distributed, [], [broadcast_reaches_other_nodes]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(blog),
    {ok, _} = application:ensure_all_started(gun),
    ok = nova_router:add_routes(blog, #{
        prefix => "/ws-test",
        security => fun blog_whoami_ws:authenticate/1,
        routes => [{"/whoami", blog_whoami_ws, #{protocol => ws}}]
    }),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

%% WebSockets

echo_replies_to_text_frames(_Config) ->
    {ok, Client} = blog_ws_client:connect("/ws/echo", [origin()]),
    ok = blog_ws_client:send_text(Client, ~"hello"),
    ?assertEqual({text, ~"echo: hello"}, blog_ws_client:recv(Client)),
    blog_ws_client:close(Client).

echo_keeps_state_per_connection(_Config) ->
    {ok, First} = blog_ws_client:connect("/ws/echo", [origin()]),
    {ok, Second} = blog_ws_client:connect("/ws/echo", [origin()]),
    ok = blog_ws_client:send_text(First, ~"one"),
    {text, _} = blog_ws_client:recv(First),
    ok = blog_ws_client:send_text(First, ~"two"),
    {text, _} = blog_ws_client:recv(First),
    ok = blog_ws_client:send_text(First, ~"stats"),
    ?assertEqual({text, ~"received 2"}, blog_ws_client:recv(First)),
    ok = blog_ws_client:send_text(Second, ~"stats"),
    ?assertEqual({text, ~"received 0"}, blog_ws_client:recv(Second)),
    blog_ws_client:close(First),
    blog_ws_client:close(Second).

foreign_origin_is_refused(_Config) ->
    ?assertEqual(
        {error, 403},
        blog_ws_client:connect("/ws/echo", [{~"origin", ~"http://evil.example"}])
    ).

missing_origin_is_allowed(_Config) ->
    {ok, Client} = blog_ws_client:connect("/ws/echo"),
    blog_ws_client:close(Client).

security_auth_data_reaches_handler(_Config) ->
    ?assertEqual({error, 401}, blog_ws_client:connect("/ws-test/whoami")),
    {ok, Client} = blog_ws_client:connect("/ws-test/whoami", [{~"x-test-user", ~"42"}]),
    ok = blog_ws_client:send_text(Client, ~"whoami"),
    ?assertEqual({text, ~"42"}, blog_ws_client:recv(Client)),
    blog_ws_client:close(Client).

%% Pub/Sub

pubsub_delivers_an_envelope(_Config) ->
    ok = nova_pubsub:join(~"test:envelope"),
    ok = nova_pubsub:broadcast(~"test:envelope", ~"greeting", #{text => ~"hi"}),
    Self = self(),
    receive
        #nova_pubsub{
            channel = ~"test:envelope", sender = Self, topic = ~"greeting", payload = Payload
        } ->
            ?assertEqual(#{text => ~"hi"}, Payload)
    after 1000 -> ct:fail(no_envelope)
    end,
    ok = nova_pubsub:leave(~"test:envelope").

pubsub_leave_stops_delivery(_Config) ->
    ok = nova_pubsub:join(~"test:leave"),
    ok = nova_pubsub:leave(~"test:leave"),
    ?assertEqual(not_joined, nova_pubsub:leave(~"test:leave")),
    ok = nova_pubsub:broadcast(~"test:leave", ~"ignored", #{}),
    receive
        #nova_pubsub{channel = ~"test:leave"} -> ct:fail(delivered_after_leave)
    after 200 -> ok
    end.

notifications_are_pushed_to_clients(_Config) ->
    {ok, Client} = blog_ws_client:connect("/ws/notifications", [origin()]),
    ok = wait_for_members(blog_notify:channel(), 2),
    ok = blog_notify:broadcast(~"post_published", #{~"id" => 7, ~"title" => ~"Hello"}),
    ?assertEqual(
        #{~"event" => ~"post_published", ~"data" => #{~"id" => 7, ~"title" => ~"Hello"}},
        recv_event(Client, ~"post_published")
    ),
    ok = blog_ws_client:send_text(Client, ~"ping"),
    ?assertEqual({text, ~"pong"}, blog_ws_client:recv(Client)),
    blog_ws_client:close(Client).

notifications_replay_recent_on_connect(_Config) ->
    ok = blog_notify:broadcast(~"post_published", #{~"id" => 8, ~"title" => ~"Backlog"}),
    _ = blog_notify_log:recent(),
    {ok, Client} = blog_ws_client:connect("/ws/notifications", [origin()]),
    ?assertMatch(
        #{~"data" := #{~"title" := ~"Backlog"}},
        recv_event(Client, ~"post_published", ~"Backlog")
    ),
    blog_ws_client:close(Client).

notify_log_keeps_the_last_ten(_Config) ->
    [ok = blog_notify:broadcast(~"tick", #{~"n" => N}) || N <- lists:seq(1, 15)],
    Recent = blog_notify_log:recent(),
    ?assertEqual(10, length(Recent)),
    ?assertEqual({~"tick", #{~"n" => 15}}, lists:last(Recent)).

post_channels_are_per_post(_Config) ->
    ?assertEqual(~"post:42", blog_notify:post_channel(42)),
    ok = nova_pubsub:join(blog_notify:post_channel(42)),
    ok = blog_notify:broadcast_post(43, ~"comment_created", #{}),
    ok = blog_notify:broadcast_post(42, ~"comment_created", #{~"id" => 1}),
    receive
        #nova_pubsub{channel = Channel, payload = Payload} ->
            ?assertEqual({~"post:42", #{~"id" => 1}}, {Channel, Payload})
    after 1000 -> ct:fail(no_comment)
    end,
    ok = nova_pubsub:leave(blog_notify:post_channel(42)).

%% Distributed

broadcast_reaches_other_nodes(_Config) ->
    ok = ensure_distributed(),
    {ok, Peer, PeerNode} = peer:start_link(#{name => peer:random_name(), connection => standard_io}),
    pong = peer:call(Peer, net_adm, ping, [node()]),
    ok = peer:call(Peer, code, add_paths, [code:get_path()]),
    {ok, _} = peer:call(Peer, pg, start, [nova_scope]),
    Relay = spawn(PeerNode, ?MODULE, relay, [self(), ~"test:cluster"]),
    ok = wait_for_members(~"test:cluster", 1),
    ok = nova_pubsub:local_broadcast(~"test:cluster", ~"local", #{}),
    ok = nova_pubsub:broadcast(~"test:cluster", ~"everywhere", #{from => node()}),
    Here = node(),
    receive
        {relayed, PeerNode, #nova_pubsub{topic = Topic, sender = Sender}} ->
            ?assertEqual(~"everywhere", Topic),
            ?assertEqual(Here, node(Sender))
    after 2000 -> ct:fail(not_relayed)
    end,
    ?assert(is_pid(Relay)),
    peer:stop(Peer).

relay(TestPid, Channel) ->
    ok = nova_pubsub:join(Channel),
    relay_loop(TestPid).

relay_loop(TestPid) ->
    receive
        Message ->
            TestPid ! {relayed, node(), Message},
            relay_loop(TestPid)
    end.

%% Helpers

origin() ->
    {ok, #{port := Port}} = application:get_env(nova, cowboy_configuration),
    {~"origin", <<"http://localhost:", (integer_to_binary(Port))/binary>>}.

recv_event(Client, Event) ->
    case blog_ws_client:recv_json(Client) of
        #{~"event" := Event} = Message -> Message;
        #{~"event" := _Other} -> recv_event(Client, Event);
        Other -> ct:fail({unexpected, Other})
    end.

recv_event(Client, Event, Title) ->
    case recv_event(Client, Event) of
        #{~"data" := #{~"title" := Title}} = Message -> Message;
        _Older -> recv_event(Client, Event, Title)
    end.

wait_for_members(Channel, Count) ->
    wait_for_members(Channel, Count, 50).

wait_for_members(Channel, Count, 0) ->
    ct:fail({members, Channel, Count, nova_pubsub:get_members(Channel)});
wait_for_members(Channel, Count, Tries) ->
    case length(nova_pubsub:get_members(Channel)) >= Count of
        true ->
            ok;
        false ->
            timer:sleep(20),
            wait_for_members(Channel, Count, Tries - 1)
    end.

ensure_distributed() ->
    case node() of
        nonode@nohost ->
            {ok, _} = net_kernel:start(blog_ct, #{name_domain => shortnames}),
            ok;
        _ ->
            ok
    end.
