# Testing Real-Time

Real-time code is harder to test than a request and a response: messages arrive later, in other processes, and sometimes not at all. The blog's real-time tests follow three rules:

1. Test the callbacks as plain functions first. A WebSocket handler or a live view is a module, and most of its logic is in pure callbacks.
2. Then drive the real thing over a real socket, with a small client that turns messages into return values.
3. Never `timer:sleep/1` and hope. Wait for a specific message with a timeout, or poll for a condition with a limit.

The suites are `blog_realtime_ws_SUITE` (Nova WebSockets and pub/sub), `blog_realtime_arizona_SUITE` (the counter and the editor) and `blog_realtime_live_SUITE` (comments, the editor's save and publish, the dashboard).

## A WebSocket client for tests

[gun](https://github.com/ninenines/gun) is Cowboy's client, and the test profile pins `{gun, "2.5.0"}`, the last release that works with the `cowlib` Nova 0.18.1 locks. `test/util/blog_ws_client.erl` wraps it so a test never handles gun's messages itself:

```erlang
connect(Path, Headers) ->
    {ok, Conn} = gun:open("localhost", port(), #{protocols => [http], retry => 0}),
    {ok, http} = gun:await_up(Conn, ?TIMEOUT),
    Stream = gun:ws_upgrade(Conn, Path, Headers),
    receive
        {gun_upgrade, Conn, Stream, [~"websocket"], _RespHeaders} ->
            {ok, {Conn, Stream}};
        {gun_response, Conn, Stream, _Fin, Status, _RespHeaders} ->
            gun:close(Conn),
            {error, Status}
    after ?TIMEOUT ->
        gun:close(Conn),
        {error, timeout}
    end.

recv({Conn, Stream}) ->
    receive
        {gun_ws, Conn, Stream, Frame} -> Frame
    after ?TIMEOUT ->
        timeout
    end.

recv_json(Client) ->
    case recv(Client) of
        {text, Text} -> json:decode(Text);
        Other -> Other
    end.
```

Two details make it useful for negative tests. A refused upgrade comes back as `{error, Status}`, so "this must be rejected" is one `?assertEqual`. And `recv/1` returns `timeout` instead of crashing, so a test can also assert that nothing arrived. The suite starts gun next to the blog: `{ok, _} = application:ensure_all_started(gun)` in `init_per_suite/1`.

## WebSocket handlers

The echo handler from [WebSockets](../real-time/websockets.md) keeps state per connection, and the origin check refuses foreign pages:

```erlang
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
```

`origin()` builds `http://localhost:<port>` from the configured port, because the test config does not use 8080.

### A test-only route

To check that a security function's `auth_data` reaches the handler, the suite needs a route the blog does not have. `nova_router:add_routes/2` adds one to the running node in `init_per_suite/1`, pointing at a handler and a security function that live in `test/`:

```erlang
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(blog),
    {ok, _} = application:ensure_all_started(gun),
    ok = nova_router:add_routes(blog, #{
        prefix => "/ws-test",
        security => fun blog_whoami_ws:authenticate/1,
        routes => [{"/whoami", blog_whoami_ws, #{protocol => ws}}]
    }),
    Config.
```

```erlang
security_auth_data_reaches_handler(_Config) ->
    ?assertEqual({error, 401}, blog_ws_client:connect("/ws-test/whoami")),
    {ok, Client} = blog_ws_client:connect("/ws-test/whoami", [{~"x-test-user", ~"42"}]),
    ok = blog_ws_client:send_text(Client, ~"whoami"),
    ?assertEqual({text, ~"42"}, blog_ws_client:recv(Client)),
    blog_ws_client:close(Client).
```

The route exists only while the suite runs, and production code has no test hooks.

## Pub/Sub

A test process can join a channel like any other process, so pub/sub is tested by subscribing the test itself and matching the `#nova_pubsub{}` record from `nova/include/nova_pubsub.hrl`:

```erlang
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
```

Each test uses its own `test:...` channel and leaves it, because subscriptions belong to the test process and are not rolled back like database rows. A test that proves something did *not* happen waits a short, fixed time; keep those few and short.

### Waiting for a subscriber

The notifications socket joins its channel in `websocket_init/1`, which runs after the upgrade has been answered. A broadcast sent right after `connect/2` returns can arrive before the handler has joined, and is lost. Instead of sleeping, the test polls `nova_pubsub:get_members/1` with a limit:

```erlang
notifications_are_pushed_to_clients(_Config) ->
    {ok, Client} = blog_ws_client:connect("/ws/notifications", [origin()]),
    ok = wait_for_members(blog_notify:channel(), 2),
    ok = blog_notify:broadcast(~"post_published", #{~"id" => 7, ~"title" => ~"Hello"}),
    ?assertEqual(
        #{~"event" => ~"post_published", ~"data" => #{~"id" => 7, ~"title" => ~"Hello"}},
        recv_event(Client, ~"post_published")
    ),
    ok = blog_ws_client:send_text(Client, ~"ping"),
    ok = recv_pong(Client),
    blog_ws_client:close(Client).
```

```erlang
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
```

The count is 2 because the notification log (`blog_notify_log`) is already a member. The failure message includes who *is* subscribed, which is what you want to know when it fails.

### Skipping messages you did not ask for

The notification socket first replays recent events from the log, and other tests in the same run add to that log. So a test must not assume the next frame is the one it caused. `recv_event/2` skips other events until the one it wants:

```erlang
recv_event(Client, Event) ->
    case blog_ws_client:recv_json(Client) of
        #{~"event" := Event} = Message -> Message;
        #{~"event" := _Other} -> recv_event(Client, Event);
        Other -> ct:fail({unexpected, Other})
    end.
```

and `recv_pong/1` does the same for the `pong` frame. An earlier version of this suite took the first frame after `ping` and failed about one run in three, when a replayed event arrived first. Shared, node-wide state (the log, channels, ETS tables) is the usual cause of a real-time test that fails "sometimes".

### Across nodes

`nova_pubsub:broadcast/3` reaches subscribers on every connected node, and `local_broadcast/3` only the local ones. `broadcast_reaches_other_nodes` starts a second node with OTP's `peer` module and subscribes a relay process there:

```erlang
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
```

The peer only runs `pg` with Nova's scope (`nova_scope`), not the blog, which is all pub/sub needs. `ensure_distributed/0` turns the CT node into a distributed node with `net_kernel:start/2` if it is not one already. Distribution needs `epmd`, which is why the CI workflow starts it.

## Live views as functions

An Arizona live view's callbacks take bindings and return bindings, so they test like any function. `test/blog_live_counter_tests.erl` is plain EUnit:

```erlang
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
```

The third element of a callback's return is the list of effects, built with `arizona_js`. Compare effects with the same builders instead of their encoded form. From `blog_realtime_arizona_SUITE`:

```erlang
editor_save_reports_errors_and_focuses(_Config) ->
    {Bindings, #{}} = blog_live_editor:mount(#{current_user => #{id => 1}}),
    Payload = #{~"title" => ~"", ~"body" => ~"Text"},
    {Invalid, #{}, [Focus]} = blog_live_editor:handle_event(~"save", Payload, Bindings),
    ?assertEqual(#{title => ~"A title is required"}, maps:get(errors, Invalid)),
    ?assertEqual(arizona_js:focus(~"#post-title"), Focus),
    {Cleared, #{}, []} = blog_live_editor:handle_info(clear_status, Invalid#{status => ~"Saved"}),
    ?assertEqual(~"", maps:get(status, Cleared)).
```

The counter tests are EUnit because the counter touches nothing else. The editor's callbacks save posts, so the editor's tests live in Common Test with the database, but they still call the callbacks directly. A navigation or patch is tested the same way, by calling `handle_update/3` ([Live Navigation](../arizona/live-navigation.md#testing-navigation)).

### Messages from other processes

`handle_info/2` receives pub/sub envelopes. A test builds the record itself and calls the callback, which is how `blog_realtime_live_SUITE` proves that a comment broadcast twice is shown once, and that a reader's own broadcast is ignored:

```erlang
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
```

Duplicates happen for real when a reader reconnects, so this test is cheaper than reproducing the race.

## Live views over HTTP and WebSocket

The first page load is plain HTTP. Arizona routes bypass Nova's plugins and security functions and use their own middlewares ([Arizona Fundamentals](../arizona/fundamentals.md)), so test that a protected page is protected, over HTTP:

```erlang
editor_requires_login(_Config) ->
    {302, Headers, _} = http_get("/live/editor"),
    ?assertEqual("/login", proplists:get_value("location", Headers)).

editor_websocket_requires_login(_Config) ->
    ?assertEqual({error, 302}, blog_ws_client:connect("/live/ws?_az_path=/live/editor")).
```

The second test matters as much as the first. The live socket at `/live/ws` takes the page's path in `_az_path` and runs that route's middlewares on the upgrade, so a client that skips the page and opens the socket directly gets the same answer.

After the upgrade, the server sends one JSON frame describing the page's dynamic parts (`"a"`). An event is a JSON array `[ViewId, Event, Payload]`, and the answer lists operations (`"o"`) on dynamic targets:

```erlang
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
```

Only the count crosses the wire, and the second answer updates the same target. Matching the whole frame would tie the test to Arizona's wire format, which can change between versions; match the parts the feature is about.

For a logged-in view, the test writes a session into `nova_session_ets` and sends its cookie with the upgrade, as shown in [Integration Testing](integration-testing.md#logged-in-sessions-without-a-form). `comment_reaches_every_reader` opens two sockets on the same post, one logged in and one anonymous, sends a comment on the first and reads it from both:

```erlang
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
    ...
```

The comment is saved by the live process that serves Bob's socket, inside the test's sandbox transaction. That works because the sandbox is checked out in shared mode: every process on the node uses the test's connection, and the comment disappears when the test checks in.

## A real browser

Some behaviour only exists in a browser: the Arizona client applying patches, `az_navigate` keeping the document, a form being reset, focus moving. None of the suites can see that. While this Part was written, those behaviours were checked with a headless Chrome driven by [puppeteer-core](https://pptr.dev/), and a small version of that check ships with the app in `examples/blog/test/browser/`. It is optional: it needs Node.js and a Chrome or Chromium binary, and it is not part of `rebar3 ct`.

`check.mjs` runs four steps against a running blog, failing on any console error, page error or 4xx/5xx response:

```javascript
await step('register and log in', async () => {
  await author.goto(`${BASE}/register`);
  await author.type('input[name=email]', `${name}@example.com`);
  await author.type('input[name=username]', name);
  await author.type('input[name=password]', 'correct horse battery');
  await author.type('input[name=password_confirmation]', 'correct horse battery');
  await Promise.all([author.waitForNavigation(), author.click('form button[type=submit]')]);
  if (new URL(author.url()).pathname !== '/posts') throw new Error(`landed on ${author.url()}`);
});

await step('a comment reaches another reader', async () => {
  await reader.goto(author.url());
  await connected(reader);
  await author.type('#comment-body', 'First!');
  await author.click('#comment-form button[type=submit]');
  await textIs(reader, '#comment-list li:last-child .comment-body', 'First!');
  await author.waitForFunction(() => document.querySelector('#comment-body').value === '');
});
```

The other two click the counter and publish a post from the live editor. `reader` is a page in a second browser context, so it has no cookies and plays an anonymous reader. `connected` waits for the `az-connected` class Arizona's client puts on `<html>` once the socket is up. Every wait has a timeout.

The checks create a real user and post, so run them against a development database, never production. With the blog running (`rebar3 nova serve`):

```shell
$ cd test/browser
$ npm ci
$ CHROME=/usr/bin/google-chrome BASE=http://localhost:8080 node check.mjs
ok   counter counts
ok   register and log in
ok   write and publish a post
ok   a comment reaches another reader
browser check passed
```

`npm ci` installs the pinned `puppeteer-core` from `package-lock.json`. Unlike `puppeteer`, it does not download a browser; `CHROME` points at the one you have. The `README.md` in that directory has the details. To run it in CI, start the blog in the background after `rebar3 ct`, wait for `/heartbeat` and run the script; a browser test finds a different class of bug, at a higher cost per test, so keep it to a few journeys.

---

With testing covered, let's prepare for production. Next: [Configuration](../production/configuration.md).
