# Pub/Sub

In the [WebSockets](../building-apis/websockets.md) chapter we briefly used `nova_pubsub` to broadcast chat messages. Now let's dive deeper into Nova's pub/sub system and build real-time notifications for our notes application.

## How nova_pubsub works

Nova's pub/sub is built on OTP's `pg` module (process groups). It starts automatically with Nova — no configuration needed. Any Erlang process can join channels, and messages are delivered to all members.

```erlang
%% Join a channel
nova_pubsub:join(channel_name).

%% Leave a channel
nova_pubsub:leave(channel_name).

%% Broadcast to all members on all nodes
nova_pubsub:broadcast(channel_name, Topic, Payload).

%% Broadcast to members on the local node only
nova_pubsub:local_broadcast(channel_name, Topic, Payload).

%% Get all members of a channel
nova_pubsub:get_members(channel_name).

%% Get members on the local node
nova_pubsub:get_local_members(channel_name).
```

Channels are atoms. Topics can be lists or binaries. Payloads can be anything.

## Message format

When a process receives a pub/sub message, it arrives as:

```erlang
{nova_pubsub, Channel, SenderPid, Topic, Payload}
```

In a gen_server, handle this in `handle_info/2`. In a WebSocket handler, use `websocket_info/2`.

## Building real-time notifications

Let's notify all connected clients when notes are created, updated, or deleted.

### Notification WebSocket handler

Create `src/controllers/my_first_nova_notifications_handler.erl`:

```erlang
-module(my_first_nova_notifications_handler).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(State) ->
    nova_pubsub:join(notes),
    {ok, State}.

websocket_handle({text, <<"ping">>}, State) ->
    {reply, {text, <<"pong">>}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({nova_pubsub, notes, _Sender, Topic, Payload}, State) ->
    Msg = thoas:encode(#{
        event => list_to_binary(Topic),
        data => Payload
    }),
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.
```

On connect, the handler joins the `notes` channel. Pub/sub messages are encoded as JSON and forwarded to the client.

### Broadcasting from controllers

Update the notes API controller to broadcast on changes:

```erlang
create(#{params := #{<<"title">> := Title, <<"body">> := Body,
                     <<"author">> := Author}}) ->
    case my_first_nova_note_repo:create(Title, Body, Author) of
        {ok, Note} ->
            nova_pubsub:broadcast(notes, "note_created", Note),
            {json, 201, #{}, Note};
        {error, Reason} ->
            {status, 422, #{}, #{error => list_to_binary(io_lib:format("~p", [Reason]))}}
    end;
```

Add `nova_pubsub:broadcast/3` after each successful create, update, and delete.

### Adding the route

```erlang
{"/notifications", my_first_nova_notifications_handler, #{protocol => ws}}
```

### Client-side JavaScript

```javascript
const ws = new WebSocket("ws://localhost:8080/notifications");

ws.onmessage = (event) => {
    const msg = JSON.parse(event.data);
    console.log(`Event: ${msg.event}`, msg.data);

    switch (msg.event) {
        case "note_created":
            // Add the new note to the UI
            break;
        case "note_updated":
            // Update the note in the UI
            break;
        case "note_deleted":
            // Remove the note from the UI
            break;
    }
};
```

## Using pub/sub in gen_servers

Any Erlang process can join a channel. This is useful for background workers:

```erlang
-module(my_first_nova_note_indexer).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    nova_pubsub:join(notes),
    {ok, #{}}.

handle_info({nova_pubsub, notes, _Sender, "note_created", Note}, State) ->
    logger:info("Indexing new note: ~p", [maps:get(title, Note)]),
    {noreply, State};
handle_info({nova_pubsub, notes, _Sender, "note_deleted", #{id := Id}}, State) ->
    logger:info("Removing note ~p from index", [Id]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.
```

## Distributed pub/sub

`nova_pubsub` works across Erlang nodes. If you have multiple instances connected in a cluster, `broadcast/3` delivers to all members on all nodes.

For local-only messaging:

```erlang
nova_pubsub:local_broadcast(notes, "note_created", Note).
```

Useful for node-specific effects like clearing a local cache.

## Organizing channels and topics

```erlang
%% Different channels for different domains
nova_pubsub:join(notes).
nova_pubsub:join(users).
nova_pubsub:join(system).

%% Topics within channels for filtering
nova_pubsub:broadcast(notes, "created", Note).
nova_pubsub:broadcast(users, "logged_in", #{username => User}).
nova_pubsub:broadcast(system, "deploy", #{version => <<"1.2.0">>}).
```

Processes can join multiple channels and pattern match on channel and topic in their handlers.

---

Next, let's learn how to write [custom plugins](custom-plugins.md) for the request pipeline.
