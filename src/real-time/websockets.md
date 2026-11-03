# WebSockets

Arizona gives you live pages without writing any WebSocket code. Sometimes you want the socket itself: a feed other clients (a mobile app, a script, a widget on a plain Nova page) can listen to, or a small protocol of your own. Nova has WebSocket handlers built in, through the `nova_websocket` behaviour. In this part we use them for a notification feed that every page of the blog can show.

## A first handler

A handler is a module with three callbacks. Start with an echo server, `src/ws/blog_echo_ws.erl`:

```erlang
-module(blog_echo_ws).
-behaviour(nova_websocket).

-export([init/1, websocket_handle/2, websocket_info/2]).

init(State) ->
    {ok, State#{received => 0}}.

websocket_handle({text, ~"stats"}, #{received := Received} = State) ->
    {reply, {text, <<"received ", (integer_to_binary(Received))/binary>>}, State};
websocket_handle({text, Text}, #{received := Received} = State) ->
    {reply, {text, <<"echo: ", Text/binary>>}, State#{received => Received + 1}};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Message, State) ->
    {ok, State}.
```

The callbacks:

- `init(State)` runs during the HTTP upgrade request. Return `{ok, State}` to accept. `State` is a map that already holds the upgrade request under `req` (and `auth_data`, see below).
- `websocket_init(State)` (optional) runs once the connection is a WebSocket, in the process that will handle it. Subscribe to things here.
- `websocket_handle(Frame, State)` gets frames from the client: `{text, Bin}`, `{binary, Bin}`, `ping`, `pong`.
- `websocket_info(Message, State)` gets any Erlang message sent to the connection process.
- `terminate(Reason, PartialReq, State)` (optional) runs when the connection ends.

`websocket_init`, `websocket_handle` and `websocket_info` return one of:

| Return | Meaning |
|---|---|
| `{ok, State}` | nothing to send |
| `{reply, Frame, State}` | send one frame, such as `{text, Bin}` |
| `{reply, [Frame], State}` | send several frames, in order |
| `{stop, State}` | close the connection |

Add `hibernate` as a last element (`{ok, State, hibernate}`) to hibernate the process between messages, which saves memory for many idle connections. Every connection is its own process, so the `received` counter above belongs to one client.

## Routing

A WebSocket route names the handler module (an atom, not a fun) and sets `protocol => ws`. The blog keeps its sockets in their own group in `blog_router.erl`, before the last group:

```erlang
#{
    prefix => "/ws",
    security => fun blog_ws_security:same_origin/1,
    routes => [
        {"/echo", blog_echo_ws, #{protocol => ws}},
        {"/notifications", blog_notifications_ws, #{protocol => ws}}
    ]
},
```

`rebar3 nova routes` shows them (Nova's WebSocket wrapper, `nova_ws_handler`, is what Cowboy actually calls):

```
     ├─  /ws
         ├─  _ /notifications (blog, nova_ws_handler:init/1)
         └─  _ /echo (blog, nova_ws_handler:init/1)
```

Restart the node and try it from the browser console on any blog page:

```javascript
let ws = new WebSocket(`ws://${location.host}/ws/echo`);
ws.onmessage = (e) => console.log(e.data);
ws.onopen = () => ws.send("Hello Nova!");
// echo: Hello Nova!
```

## Securing the upgrade

Browsers attach cookies to WebSocket upgrades but do not apply the same-origin policy to them. Any page on the internet can open `ws://your-blog/ws/...` from a visitor's browser, with the visitor's session cookie: this is *cross-site WebSocket hijacking*. Arizona checks the `Origin` header for its own socket; for Nova handlers it is up to you.

The route group's `security` function runs for the upgrade request like for any other route, so that is where the check goes. `src/ws/blog_ws_security.erl`:

```erlang
-module(blog_ws_security).
-export([same_origin/1]).

same_origin(Req) ->
    Host = cowboy_req:header(~"host", Req),
    case cowboy_req:header(~"origin", Req) of
        undefined -> true;
        <<"http://", Host/binary>> -> true;
        <<"https://", Host/binary>> -> true;
        _Other -> {false, 403, #{}}
    end.
```

A browser always sends `Origin` on a WebSocket upgrade, so a missing one means a non-browser client (a script, a test, another server), which carries no ambient browser credentials to abuse. Behind a proxy that rewrites `Host`, compare against your public origin from config instead.

Security functions work as described in [Routing](../foundations/routing.md). For a WebSocket route, `{true, AuthData}` puts `AuthData` into the handler's state as `auth_data`, so a handler can read who it is talking to in `init/1`. The test suite mounts this one on a test-only route:

```erlang
authenticate(Req) ->
    case cowboy_req:header(~"x-test-user", Req) of
        undefined -> {false, 401, #{}};
        Id -> {true, #{id => binary_to_integer(Id)}}
    end.

init(#{auth_data := User} = State) ->
    {ok, State#{user_id => maps:get(id, User)}}.

websocket_handle({text, ~"whoami"}, #{user_id := UserId} = State) ->
    {reply, {text, integer_to_binary(UserId)}, State};
```

A rejected upgrade gets the security function's status (`401` here) and never reaches `init/1`. In the real app the security function would look the user up from the session, as the Authentication chapter does for pages.

Plugins configured on the group run too, with their `pre_ws_request` and `post_ws_request` callbacks around each WebSocket callback.

```admonish note title="Idle connections"
Cowboy closes a WebSocket that has seen no frames for 60 seconds. Nova starts its handlers with Cowboy's defaults, so long-lived clients should send something periodically. The notification client in the next chapter sends `ping` every 30 seconds and the handler answers `pong`.
```

## Testing a WebSocket

Common Test can talk to the running node with [gun](https://github.com/ninenines/gun), Cowboy's companion client. The blog adds it to the test profile only:

```erlang
{test, [
    ...
    {deps, [
        {meck, "1.2.0"},
        {nova_test, "~> 0.2"},
        {gun, "2.5.0"}
    ]},
    ...
]}
```

gun 2.5.0 is the newest release that works with the `cowlib` 2.19 Nova 0.18.1 locks; later gun versions need a newer cowlib. A small helper, `test/util/blog_ws_client.erl`, opens a connection and turns gun's messages into return values:

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
```

and the tests in `test/blog_realtime_ws_SUITE.erl` read like a conversation:

```erlang
echo_replies_to_text_frames(_Config) ->
    {ok, Client} = blog_ws_client:connect("/ws/echo", [origin()]),
    ok = blog_ws_client:send_text(Client, ~"hello"),
    ?assertEqual({text, ~"echo: hello"}, blog_ws_client:recv(Client)),
    blog_ws_client:close(Client).

foreign_origin_is_refused(_Config) ->
    ?assertEqual(
        {error, 403},
        blog_ws_client:connect("/ws/echo", [{~"origin", ~"http://evil.example"}])
    ).
```

Remember `{ok, _} = application:ensure_all_started(gun)` in `init_per_suite/1`.

## Nova WebSockets or Arizona?

| Use | When |
|---|---|
| Arizona live views | The browser shows server-rendered UI that changes. You write no protocol and no client code. |
| `nova_websocket` | You need your own message format, a non-browser client, or a small feed on pages Arizona does not render. |

The blog uses both, side by side on one page: Arizona for the editor and the comment section, a Nova WebSocket for the notification feed in the header. The feed needs a way to hear about events from the rest of the app, which is [Pub/Sub](pubsub.md).
