# Pub/Sub

A WebSocket handler can talk to its own client. To push "a new post was published" to every open notification feed, something has to deliver one event to many connection processes. Nova ships a small publish/subscribe layer for that, `nova_pubsub`. In this chapter we use it to build the blog's notification feed: a list of recent activity in the header of every live page, fed by a Nova WebSocket.

## nova_pubsub

`nova_pubsub` is a thin layer over OTP's `pg` process groups, in a scope Nova starts at boot (`nova_scope`). There is nothing to configure.

```erlang
nova_pubsub:join(Channel)                         %% the calling process joins
nova_pubsub:join(Channel, Pid)
nova_pubsub:leave(Channel)                        %% ok | not_joined
nova_pubsub:broadcast(Channel, Topic, Payload)    %% every member, every node
nova_pubsub:local_broadcast(Channel, Topic, Payload)  %% members on this node
nova_pubsub:get_members(Channel)
nova_pubsub:get_local_members(Channel)
```

- A **channel** is an atom or a binary. Use binaries for anything built at runtime, such as one channel per post; never create atoms from data.
- A **topic** is a binary (or string) naming the kind of event within the channel.
- The **payload** is any term.

Members receive a `#nova_pubsub{}` record. Include its definition and match on it:

```erlang
-include_lib("nova/include/nova_pubsub.hrl").
```

```erlang
-record(nova_pubsub, {
                      channel :: atom() | binary(),
                      sender :: pid(),
                      topic :: list() | binary(),
                      payload :: any()
                     }).
```

The test suite shows the round trip:

```erlang
pubsub_delivers_an_envelope(_Config) ->
    ok = nova_pubsub:join(~"test:envelope"),
    ok = nova_pubsub:broadcast(~"test:envelope", ~"greeting", #{text => ~"hi"}),
    Self = self(),
    receive
        #nova_pubsub{channel = ~"test:envelope", sender = Self, topic = ~"greeting", payload = Payload} ->
            ?assertEqual(#{text => ~"hi"}, Payload)
    after 1000 -> ct:fail(no_envelope)
    end,
    ok = nova_pubsub:leave(~"test:envelope").
```

A process leaves every channel automatically when it exits, so a connection process that dies never needs cleaning up.

## The blog's channels

Rather than scattering channel names through the code, the blog keeps them in one module, `src/ws/blog_notify.erl`:

```erlang
-module(blog_notify).
-export([channel/0, post_channel/1, broadcast/2, broadcast_post/3]).
-export([post_published/1, comment_created/3, comment/2]).

channel() ->
    ~"notifications".

post_channel(PostId) when is_integer(PostId) ->
    <<"post:", (integer_to_binary(PostId))/binary>>.

broadcast(Event, Payload) ->
    nova_pubsub:broadcast(channel(), Event, Payload).

broadcast_post(PostId, Event, Payload) ->
    nova_pubsub:broadcast(post_channel(PostId), Event, Payload).
```

The last three functions name the blog's events. They come up in the [Broadcasting from controllers](#broadcasting-from-controllers) section below.

Two kinds of channel:

- `~"notifications"` carries site-wide events for the feed: `post_published`, `comment_created`.
- `~"post:42"` carries events about one post, for whoever is looking at it: the [live comment section](live-feature.md) subscribes to its post's channel.

Payloads on the notification channel go straight to browsers as JSON, so they are maps with binary keys and JSON-friendly values, built deliberately rather than by dumping a database row.

## The notification feed

The feed handler joins the channel once the socket is up and forwards every event as a JSON frame. `src/ws/blog_notifications_ws.erl`:

```erlang
-module(blog_notifications_ws).
-behaviour(nova_websocket).

-include_lib("nova/include/nova_pubsub.hrl").

-export([init/1, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(State) ->
    {ok, State}.

websocket_init(State) ->
    ok = nova_pubsub:join(blog_notify:channel()),
    {reply, [frame(Event, Payload) || {Event, Payload} <- blog_notify_log:recent()], State}.

websocket_handle({text, ~"ping"}, State) ->
    {reply, {text, ~"pong"}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(#nova_pubsub{topic = Event, payload = Payload}, State) ->
    {reply, frame(Event, Payload), State};
websocket_info(_Message, State) ->
    {ok, State}.

frame(Event, Payload) ->
    {text, json:encode(#{~"event" => Event, ~"data" => Payload})}.
```

`join` happens in `websocket_init/1`, which runs in the connection process after the upgrade. The handler then sends the recent events, so a reader who just arrived sees what happened a minute ago, not an empty list. A list of frames is fine as a reply; Nova sends them in order. An event published in the moment between `join` and `recent()` is delivered twice, once live and once in the backlog, so the feed is at-least-once. Joining before reading the backlog means an event can arrive twice but never not at all, which is the right side to err on for a feed: a repeated line in the header is harmless, and a client that cares can skip an event it has already shown.

### Pub/sub in a gen_server

The backlog comes from a process that is subscribed for the life of the node and remembers the last ten events. Any process can be a member, and a `gen_server` handles the envelope in `handle_info/2`. `src/ws/blog_notify_log.erl`:

```erlang
-module(blog_notify_log).
-behaviour(gen_server).

-include_lib("nova/include/nova_pubsub.hrl").

-export([start_link/0, recent/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(KEEP, 10).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

recent() ->
    gen_server:call(?MODULE, recent).

init([]) ->
    ok = nova_pubsub:join(blog_notify:channel()),
    {ok, []}.

handle_call(recent, _From, Recent) ->
    {reply, lists:reverse(Recent), Recent}.

handle_cast(_Message, Recent) ->
    {noreply, Recent}.

handle_info(#nova_pubsub{topic = Event, payload = Payload}, Recent) ->
    {noreply, lists:sublist([{Event, Payload} | Recent], ?KEEP)};
handle_info(_Message, Recent) ->
    {noreply, Recent}.
```

It joins in `init/1`, so if it crashes and restarts it joins again. Start it from the blog's supervisor in `src/blog_sup.erl`:

```erlang
init([]) ->
    Children = [
        #{id => blog_notify_log, start => {blog_notify_log, start_link, []}}
    ],
    {ok, {#{strategy => one_for_all}, Children}}.
```

### The browser side

The client is a few lines of plain JavaScript, served by Nova's static route from `priv/assets/js/notifications.js`. It fills the `<ul id="notifications">` the Arizona layout renders, so it runs on every live page:

```javascript
const list = document.getElementById('notifications');

function describe({ event, data }) {
  switch (event) {
    case 'post_published': return `New post: ${data.title}`;
    case 'comment_created': return `${data.author} commented on ${data.post_title}`;
    default: return event;
  }
}

function connect(attempt = 0) {
  const scheme = location.protocol === 'https:' ? 'wss' : 'ws';
  const socket = new WebSocket(`${scheme}://${location.host}/ws/notifications`);
  const keepAlive = setInterval(() => socket.send('ping'), 30000);
  socket.onopen = () => { attempt = 0; list.replaceChildren(); };
  socket.onmessage = (message) => {
    if (message.data === 'pong') return;
    const item = document.createElement('li');
    item.textContent = describe(JSON.parse(message.data));
    list.prepend(item);
    while (list.children.length > 5) list.lastElementChild.remove();
  };
  socket.onclose = () => {
    clearInterval(keepAlive);
    setTimeout(() => connect(attempt + 1), Math.min(1000 * 2 ** attempt, 30000));
  };
}

if (list) connect();
```

It uses `textContent`, never `innerHTML`, because titles and names are user input. It reconnects with backoff and clears the list on reconnect, since the server replays the backlog. Live navigation between Arizona pages keeps the document, so the feed stays connected while the reader moves around.

The layout loads it with `{script, [{type, ~"module"}, {src, ~"/assets/js/notifications.js"}], []}`.

Try it: open <http://localhost:8080/live/counter>, then in the `rebar3 shell`:

```erlang
1> blog_notify:broadcast(~"post_published", #{~"id" => 1, ~"title" => ~"Hello"}).
ok
```

"New post: Hello" appears in the page's Latest list.

## Broadcasting from controllers

Events start where data changes. The comments API from [Associations & Preloading](../data-layer/associations.md) is one such place. Once the comment is stored, `blog_posts_controller:create_comment/1` announces it:

```erlang
create_comment(#{bindings := #{~"id" := Id}, json := Params, auth_data := Actor}) ->
    case blog_posts:get_post(Id) of
        {ok, Post} ->
            case blog_posts:add_comment(Post, Actor, Params) of
                {ok, Comment} ->
                    ok = blog_notify:comment_created(Post, Actor, Comment),
                    {json, 201, #{}, blog_api_json:comment(Comment)};
                {error, #kura_changeset{} = CS} ->
                    unprocessable(CS)
            end;
        {error, not_found} ->
            not_found()
    end.
```

The event functions in `blog_notify` decide what each channel gets:

```erlang
post_published(#{id := Id, title := Title}) ->
    broadcast(~"post_published", #{~"id" => Id, ~"title" => Title}).

comment_created(#{id := PostId, title := PostTitle}, #{username := Author} = User, Comment) ->
    ok = broadcast_post(PostId, ~"comment_created", comment(Comment, User)),
    broadcast(~"comment_created", #{
        ~"post_id" => PostId,
        ~"post_title" => PostTitle,
        ~"author" => Author
    }).

comment(#{id := Id, body := Body, inserted_at := InsertedAt}, #{username := Author}) ->
    #{id => Id, body => Body, author => Author, inserted_at => blog_api_json:datetime(InsertedAt)}.
```

One comment produces two events:

- The post's own channel gets the whole comment, in the shape the [live comment section](live-feature.md) renders. That is an Erlang map that never leaves the server.
- The site-wide feed gets a summary for browsers: who commented on what. It has no comment text, because the feed is not the place to read comments.

Both are built from named fields. Never broadcast a row as it came from the database: a user row holds `hashed_password`, and any field in a payload is one careless `json:encode` away from a browser.

Rules that keep broadcasts honest:

- **Broadcast after the write succeeds**, in the code that knows it succeeded. Inside a `blog_repo:transaction/1` or a `kura_multi`, wait until it has committed: a listener told about a row that was rolled back will show a comment that does not exist.
- **Keep it in one module.** Event names and payloads are an interface between parts of the app. With `blog_notify` the API controller and the live comment form announce a comment the same way, and a test can check the shape in one place.
- **A broadcast is fire-and-forget.** `nova_pubsub:broadcast/3` returns `ok` whether there are a hundred listeners or none, and a slow listener does not slow the controller.

The suite checks that the API broadcasts:

```erlang
api_comments_are_broadcast(Config) ->
    #{id := PostId} = ?config(post, Config),
    Token = blog_data_fixtures:token(?config(bob, Config)),
    ok = nova_pubsub:join(blog_notify:post_channel(PostId)),
    Path = "/api/posts/" ++ integer_to_list(PostId) ++ "/comments",
    {201, #{~"id" := CommentId}} =
        blog_data_fixtures:request_as(Token, post, Path, #{~"body" => ~"Via the API"}),
    receive
        #nova_pubsub{topic = ~"comment_created", payload = #{id := Id, author := Author}} ->
            ?assertEqual({CommentId, ~"bob"}, {Id, Author})
    after 2000 -> ct:fail(no_broadcast)
    end.
```

The test process joins the channel like any other listener, so the test needs no mocks.

Try it with the feed open in a browser and a published post in the database (the [editor](live-feature.md#saving-and-publishing) publishes one). Writes to the API need a bearer token ([API tokens](../auth-sessions/authentication.md#api-tokens)); get one for your user, then comment on post 1:

```shell
TOKEN=$(curl -s -X POST localhost:8080/api/tokens -H 'content-type: application/json' \
  -d '{"email": "alice@example.com", "password": "correct horse battery"}' | sed 's/.*"token":"\([^"]*\)".*/\1/')
curl -s -X POST localhost:8080/api/posts/1/comments \
  -H "authorization: Bearer $TOKEN" -H 'content-type: application/json' \
  -d '{"body": "From the API"}'
```

```
{"id":1,"body":"From the API","user_id":2,"inserted_at":"2026-11-03T08:47:38Z"}
```

The Latest list shows "alice commented on Hello live" (with your user and post): the author is whoever the token belongs to.

## Arizona's pub/sub

Arizona has its own pub/sub, `arizona_pubsub`, also built on `pg` (scope `arizona_pubsub`). The `?subscribe(Topic)` and `?unsubscribe(Topic)` macros in a live view subscribe the live process; `arizona_pubsub:broadcast(Topic, Message)` sends `Message` as is, with no envelope. All views in one live process share the membership.

The two do the same job. The blog publishes its application events through `nova_pubsub`, and the live views join those channels with `nova_pubsub:join/1`: a live view is a process like any other, so the `#nova_pubsub{}` envelope reaches its `handle_info/2`. One bus means a controller, a WebSocket handler, a gen_server and a live view all hear the same event.

## Across nodes

`pg` groups span every connected node of the cluster. When the blog runs on two nodes joined with distributed Erlang, `broadcast/3` on one reaches feeds connected to the other, with no extra setup; `local_broadcast/3` stays on the calling node. The suite proves it with a `peer` node:

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
    ...
```

The relay on the peer receives `everywhere` but not `local`.

Know what you are getting:

- **No persistence.** A member that is not connected when an event is sent never sees it. That is why the feed's backlog lives in `blog_notify_log`, and why anything that matters is in the database first and announced second.
- **At most once.** Messages between nodes are lost if the connection drops. Group membership also takes a moment to spread after a node joins.
- **Ordering** holds between one sender and one receiver, not across senders.
- **Fan-out cost** is one message per member. Fine for thousands of feeds; for very large audiences, broadcast to one process per node and fan out locally with `local_broadcast/3`.

For a blog's live UI those are the right trade-offs. If you need durable, acknowledged delivery, use a job queue such as `shigoto` or a message broker instead.

---

Next, [Building a Live Feature](live-feature.md) puts Arizona and pub/sub together in a live comment section.
