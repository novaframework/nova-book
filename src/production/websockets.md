# WebSockets

HTTP request-response works well for most operations, but sometimes you need real-time, bidirectional communication. Nova has built-in WebSocket support through the `nova_websocket` behaviour. We will use it to build a live comments handler for our blog.

## Creating a WebSocket handler

A WebSocket handler implements three callbacks: `init/1`, `websocket_handle/2`, and `websocket_info/2`.

Create `src/controllers/blog_ws_handler.erl`:

```erlang
-module(blog_ws_handler).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    {reply, {text, <<"Echo: ", Msg/binary>>}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
```

The callbacks:

- **`init/1`** — called when the WebSocket connection is established. Return `{ok, State}` to accept.
- **`websocket_handle/2`** — called when a message arrives from the client. Return `{reply, Frame, State}` to send a response, `{ok, State}` to do nothing, or `{stop, State}` to close.
- **`websocket_info/2`** — called when the handler process receives an Erlang message (not a WebSocket frame). Useful for receiving pub/sub notifications from other processes.

## Adding the route

WebSocket routes use the module name as an atom (not a fun reference) and set `protocol => ws`:

```erlang
{"/ws", blog_ws_handler, #{protocol => ws}}
```

Add it to your public routes:

```erlang
#{prefix => "",
  security => false,
  routes => [
             {"/login", fun blog_main_controller:login/1, #{methods => [get]}},
             {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
             {"/ws", blog_ws_handler, #{protocol => ws}}
            ]
}
```

## Testing the WebSocket

Start the node with `rebar3 nova serve` and test from a browser console:

```javascript
let ws = new WebSocket("ws://localhost:8080/ws");
ws.onmessage = (e) => console.log(e.data);
ws.onopen = () => ws.send("Hello Nova!");
// Should log: "Echo: Hello Nova!"
```

## A live comments handler

Let's build something more practical — a handler that broadcasts new comments to all connected clients using `nova_pubsub`.

Create `src/controllers/blog_comments_ws_handler.erl`:

```erlang
-module(blog_comments_ws_handler).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(State) ->
    nova_pubsub:join(comments),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    nova_pubsub:broadcast(comments, "new_comment", Msg),
    {ok, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({nova_pubsub, comments, _Sender, "new_comment", Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.
```

In `init/1` we join the `comments` channel. When a client sends a message, we broadcast it to all channel members. When a pub/sub message arrives via `websocket_info/2`, we forward it to the connected client. We will explore pub/sub in depth in the [Pub/Sub](pubsub.md) chapter.

## Custom handlers

Nova uses a handler registry that maps return tuple atoms to handler functions. The built-in handlers:

| Return atom | What it does |
|---|---|
| `json` | Encodes data as JSON |
| `ok` | Renders an ErlyDTL template |
| `status` | Returns a status code |
| `redirect` | Redirects to another URL |
| `sendfile` | Sends a file |
| `view` | Renders a specific view template |

You can register custom handlers:

```erlang
nova_handlers:register_handler(xml, fun my_xml_handler:handle/3).
```

Then return from controllers:

```erlang
my_action(_Req) ->
    {xml, <<"<user><name>Alice</name></user>">>}.
```

The handler function receives `(ReturnTuple, CallbackFun, Req)` where `ReturnTuple` is the full controller return value. It must return `{ok, Req2}`.

## Fallback controllers

If a controller returns an unrecognized value, Nova can delegate to a fallback controller:

```erlang
-module(my_controller).
-fallback_controller(my_fallback).

index(_Req) ->
    something_unexpected.
```

The fallback module needs `resolve/2`:

```erlang
-module(my_fallback).
-export([resolve/2]).

resolve(Req, InvalidReturn) ->
    logger:warning("Invalid return from controller: ~p", [InvalidReturn]),
    {status, 500, #{}, #{error => <<"internal server error">>}}.
```

---

With WebSockets in place, let's build a real-time comment feed using [Pub/Sub](pubsub.md).
