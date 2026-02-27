# Sessions

Nova has a built-in session system backed by ETS (Erlang Term Storage). Session IDs are stored in a `session_id` cookie.

## How sessions work

Nova automatically creates a session for every visitor. On each request, the `nova_stream_h` stream handler checks for a `session_id` cookie:

- **Cookie exists** — the request proceeds normally. The session ID is read from the cookie when you call the session API.
- **No cookie** — Nova generates a new session ID, sets the `session_id` cookie on the response, and stores the ID in the request map.

This means you never need to manually generate session IDs or set the session cookie. By the time your controller runs, every request already has a session — you just read from and write to it.

## The session API

```erlang
nova_session:get(Req, Key)          -> {ok, Value} | {error, not_found}.
nova_session:set(Req, Key, Value)   -> ok | {error, session_id_not_set}.
nova_session:delete(Req)            -> {ok, Req1}.
nova_session:delete(Req, Key)       -> {ok, Req1}.
```

| Function | Description |
|----------|-------------|
| `get/2` | Retrieve a value by key. Returns `{error, not_found}` if the key or session doesn't exist. |
| `set/3` | Store a value in the current session. |
| `delete/1` | Delete the entire session and expire the cookie (sets `max_age => 0`). Returns an updated request — use this `Req1` if you need the cookie change in the response. |
| `delete/2` | Delete a single key from the session. |

## Configuration

The session manager is configured in `sys.config`:

```erlang
{nova, [
    {use_sessions, true},            %% Enable sessions (default: true)
    {session_manager, nova_session_ets}  %% Backend module (default)
]}
```

`nova_session_ets` stores session data in an ETS table and replicates changes across clustered nodes using `nova_pubsub`. Set `use_sessions` to `false` if your application doesn't need sessions (e.g. a pure JSON API).

## Cookie options

Nova sets the `session_id` cookie automatically with default options. For production, you may want to customise the cookie by setting it yourself in a [plugin](../foundations/plugins.md) or by configuring Cowboy's cookie defaults:

```erlang
cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req, #{
    path => <<"/">>,          %% Cookie is valid for all paths
    http_only => true,        %% Not accessible from JavaScript
    secure => true,           %% Only sent over HTTPS
    max_age => 86400          %% Expires after 24 hours (in seconds)
}).
```

```admonish warning
For production, always set `http_only` and `secure` to `true`.
```

## Custom session backends

If you want to store sessions in a database or Redis instead of ETS, implement the `nova_session` behaviour:

```erlang
-module(my_redis_session).
-behaviour(nova_session).

-export([start_link/0,
         get_value/2,
         set_value/3,
         delete_value/1,
         delete_value/2]).

start_link() ->
    ignore.

get_value(SessionId, Key) ->
    {ok, Value}.

set_value(SessionId, Key, Value) ->
    ok.

delete_value(SessionId) ->
    ok.

delete_value(SessionId, Key) ->
    ok.
```

Then configure it:

```erlang
{nova, [
    {session_manager, my_redis_session}
]}
```

## Distributed sessions

`nova_session_ets` replicates session changes across clustered nodes using `nova_pubsub` (built on OTP's `pg` module). When you call `nova_session:set/3` on one node, the change is broadcast to all other nodes in the cluster.

This means users can hit any node behind a load balancer and their session data is available — no sticky sessions required.

---

With sessions in place, let's build [authentication](authentication.md) on top of them.
