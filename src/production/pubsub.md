# Pub/Sub and Real-Time Feed

In the [WebSockets](websockets.md) chapter we used `nova_pubsub` to broadcast comments. Now let's dive deeper into Nova's pub/sub system and build a real-time feed for our blog — live notifications when posts are published and comments are added.

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

## Building the real-time feed

### Notification WebSocket handler

Create `src/controllers/blog_feed_handler.erl`:

```erlang
-module(blog_feed_handler).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(State) ->
    nova_pubsub:join(posts),
    nova_pubsub:join(comments),
    {ok, State}.

websocket_handle({text, <<"ping">>}, State) ->
    {reply, {text, <<"pong">>}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({nova_pubsub, Channel, _Sender, Topic, Payload}, State) ->
    Msg = thoas:encode(#{
        channel => Channel,
        event => list_to_binary(Topic),
        data => Payload
    }),
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.
```

On connect, the handler joins both the `posts` and `comments` channels. Any pub/sub message is encoded as JSON and forwarded to the client.

### Broadcasting from controllers

Update the posts controller to broadcast on changes:

```erlang
create(#{json := Params}) ->
    CS = post:changeset(#{}, Params),
    case blog_repo:insert(CS) of
        {ok, Post} ->
            nova_pubsub:broadcast(posts, "post_created", post_to_json(Post)),
            {json, 201, #{}, post_to_json(Post)};
        {error, #kura_changeset{} = CS1} ->
            {json, 422, #{}, #{errors => changeset_errors_to_json(CS1)}}
    end;
```

Do the same for updates and deletes:

```erlang
%% After a successful update:
nova_pubsub:broadcast(posts, "post_updated", post_to_json(Updated)),

%% After a successful delete:
nova_pubsub:broadcast(posts, "post_deleted", #{id => binary_to_integer(Id)}),
```

And for comments:

```erlang
%% After creating a comment:
nova_pubsub:broadcast(comments, "comment_created", comment_to_json(Comment)),
```

### Adding the route

```erlang
{"/feed", blog_feed_handler, #{protocol => ws}}
```

### Client-side JavaScript

```javascript
const ws = new WebSocket("ws://localhost:8080/feed");

ws.onmessage = (event) => {
    const msg = JSON.parse(event.data);
    console.log(`[${msg.channel}] ${msg.event}:`, msg.data);

    switch (msg.event) {
        case "post_created":
            // Add the new post to the feed
            break;
        case "post_updated":
            // Update the post in the feed
            break;
        case "post_deleted":
            // Remove the post from the feed
            break;
        case "comment_created":
            // Append the new comment
            break;
    }
};

// Keep-alive
setInterval(() => ws.send("ping"), 30000);
```

## Per-post comment feeds

For a live comment section on a specific post, use dynamic channel names:

```erlang
-module(blog_post_comments_handler).
-behaviour(nova_websocket).

-export([init/1, websocket_handle/2, websocket_info/2]).

init(#{req := #{bindings := #{<<"post_id">> := PostId}}} = State) ->
    Channel = list_to_atom("post_comments_" ++ binary_to_list(PostId)),
    nova_pubsub:join(Channel),
    {ok, State#{channel => Channel}};
init(State) ->
    {ok, State}.

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({nova_pubsub, _Channel, _Sender, _Topic, Payload}, State) ->
    {reply, {text, thoas:encode(Payload)}, State};
websocket_info(_Info, State) ->
    {ok, State}.
```

Route:

```erlang
{"/posts/:post_id/comments/ws", blog_post_comments_handler, #{protocol => ws}}
```

When creating a comment, broadcast to the post-specific channel:

```erlang
Channel = list_to_atom("post_comments_" ++ integer_to_list(PostId)),
nova_pubsub:broadcast(Channel, "new_comment", comment_to_json(Comment)).
```

## Using pub/sub in gen_servers

Any Erlang process can join a channel. This is useful for background workers like search indexing:

```erlang
-module(blog_search_indexer).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    nova_pubsub:join(posts),
    {ok, #{}}.

handle_info({nova_pubsub, posts, _Sender, "post_created", Post}, State) ->
    logger:info("Indexing new post: ~p", [maps:get(title, Post)]),
    %% Add to search index
    {noreply, State};
handle_info({nova_pubsub, posts, _Sender, "post_deleted", #{id := Id}}, State) ->
    logger:info("Removing post ~p from index", [Id]),
    %% Remove from search index
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.
```

Add it to your supervisor to start automatically.

## Distributed pub/sub

`nova_pubsub` works across Erlang nodes. If you have multiple instances connected in a cluster, `broadcast/3` delivers to all members on all nodes.

For local-only messaging (e.g., clearing a local cache):

```erlang
nova_pubsub:local_broadcast(posts, "cache_invalidated", #{id => PostId}).
```

## Organizing channels and topics

```erlang
%% Different channels for different domains
nova_pubsub:join(posts).
nova_pubsub:join(comments).
nova_pubsub:join(users).
nova_pubsub:join(system).

%% Topics within channels for filtering
nova_pubsub:broadcast(posts, "created", Post).
nova_pubsub:broadcast(posts, "published", Post).
nova_pubsub:broadcast(comments, "created", Comment).
nova_pubsub:broadcast(users, "logged_in", #{username => User}).
nova_pubsub:broadcast(system, "deploy", #{version => <<"1.2.0">>}).
```

Processes can join multiple channels and pattern match on channel and topic in their handlers.

---

Next, let's look at [transactions, multi, and bulk operations](transactions-bulk.md) for atomic and efficient data operations.
