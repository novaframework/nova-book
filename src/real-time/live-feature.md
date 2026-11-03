# Building a Live Feature

This chapter puts the last two parts together. A published post gets a page whose comment section updates on its own. Comments come from readers on the page, from other readers' pages and from the JSON API, and all three show up for everyone. The editor from [Live Views](../arizona/live-views.md) learns to save and publish real posts, and publishing sends the author to the new post's page.

The pieces:

- `blog_live_post`, a live view at `/live/posts/:id`
- a route middleware that loads the post and refuses drafts
- an Arizona *stream* for the comments
- the post's pub/sub channel from [Pub/Sub](pubsub.md), `~"post:42"`
- `blog_notify`, which builds every event

## The route

The page needs the post id from the path, the reader (who may be anonymous), and the post itself:

```erlang
{live, ~"/live/posts/:id", blog_live_post, #{
    layouts => [{blog_live_layout, render}],
    middlewares => [
        {blog_live_middleware, path_bindings},
        {blog_live_middleware, current_user},
        {blog_live_middleware, published_post},
        {arizona_middleware, fetch_flash}
    ]
}},
```

`fetch_flash` is Arizona's own middleware. It takes no arguments, so it can be named directly. It is explained in [Live Navigation](../arizona/live-navigation.md). `published_post/2` is new in `src/live/blog_live_middleware.erl` (add it, and `own_post/2` from later in this chapter, to the module's `-export`):

```erlang
published_post(Req, #{~"id" := Id} = Bindings) ->
    case blog_posts:get_post(Id, [author]) of
        {ok, #{status := published} = Post} -> {cont, Req, Bindings#{post => Post}};
        _ -> not_found(Req)
    end.

not_found(Req) ->
    {halt, arizona_req:put_resp_status(Req, 404)}.
```

`blog_posts:get_post/2` casts the id, so `/live/posts/abc` is a plain not-found and never a crash. A draft answers `404` too, the same as a post that does not exist, so the page does not reveal that a draft is there. A halt that is not a redirect refuses the WebSocket upgrade in the same way, so a draft cannot be watched over the socket either.

```shell
curl -s -o /dev/null -w "%{http_code}\n" localhost:8080/live/posts/999
```

```
404
```

## Mounting: subscribe, then read

`src/live/blog_live_post.erl`:

```erlang
-module(blog_live_post).
-include_lib("arizona/include/arizona_stateful.hrl").
-include_lib("nova/include/nova_pubsub.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).
-export([path/1, comment_key/1]).

mount(#{post := Post} = Bindings) ->
    #{id := PostId, title := Title, body := Body, author := #{username := Author}} = Post,
    ?connected andalso nova_pubsub:join(blog_notify:post_channel(PostId)),
    {
        #{
            id => ~"post",
            title => Title,
            post => #{id => PostId, title => Title},
            author => Author,
            paragraphs => blog_live_editor:paragraphs(Body),
            current_user => maps:get(current_user, Bindings),
            comments => arizona_stream:new(fun ?MODULE:comment_key/1, load_comments(PostId)),
            comment_error => undefined,
            flash => maps:get(flash, Bindings, #{})
        },
        #{}
    }.
```

The order matters. The connected mount joins the post's channel *before* it reads the comments. A comment posted between the two steps is then both in the list and in the mailbox, and the view handles the duplicate (below). Reading first would leave a gap in which a comment is in neither. The HTTP render does not join: its process is gone as soon as the page is sent.

The view keeps only what it renders. `post` holds the id and title it needs for comments, not the whole row, and the author row is reduced to a username.

The comments are loaded through the post's preload and put in comment order:

```erlang
load_comments(PostId) ->
    {ok, #{comments := Comments}} = blog_posts:get_post(PostId, [{comments, [author]}]),
    Sorted = lists:sort([{Id, Comment} || #{id := Id} = Comment <- Comments]),
    [blog_notify:comment(Comment, Author) || {_, #{author := Author} = Comment} <- Sorted].
```

Each one becomes the same small map a broadcast carries, built by `blog_notify:comment/2` from [Pub/Sub](pubsub.md#broadcasting-from-controllers): `#{id, body, author, inserted_at}`. Loaded comments and comments that arrive later have one shape, so they render with one template.

## Streams

The comments are in an `arizona_stream`, not a plain list. A stream is a keyed, ordered collection that remembers what changed: `insert/2`, `update/3`, `delete/2` and `move/3` queue an operation, and the next diff sends only those items. Adding the 200th comment sends one `<li>`, not 200.

`arizona_stream:new(KeyFun, Items)` takes the function that gives each item its key. `fun ?MODULE:comment_key/1` names an exported function rather than building a closure:

```erlang
comment_key(#{id := Id}) ->
    Id.
```

Keys must be unique and of one type. Arizona addresses an item in the DOM by its key's text, so `1` and `~"1"` would collide.

In the template, `?each` over a stream gives the function the item *and* its key, and the item's root element carries the key as `az_key`:

```erlang
render(Bindings) ->
    ?html(
        {article, [{id, ?get(id)}, {class, ~"post"}], [
            ?stateless(blog_live_components, flash, #{flash => ?get(flash)}),
            {h1, [], [maps:get(title, ?get(post))]},
            {p, [{class, ~"byline"}], [~"by ", ?get(author)]},
            ?each(fun(Paragraph) -> {p, [], [Paragraph]} end, ?get(paragraphs)),
            {section, [{id, ~"comments"}], [
                {h2, [], [~"Comments"]},
                {ol, [{id, ~"comment-list"}], [
                    ?each(
                        fun(#{author := Author, body := Body, inserted_at := At}, Key) ->
                            {li, [{az_key, Key}, {class, ~"comment"}], [
                                {p, [{class, ~"comment-meta"}], [
                                    {strong, [], [Author]},
                                    ~" ",
                                    {time, [{datetime, At}], [At]}
                                ]},
                                {p, [{class, ~"comment-body"}], [Body]}
                            ]}
                        end,
                        ?get(comments)
                    )
                ]},
                ?stateless(fun comment_form/1, #{
                    signed_in => ?get(current_user) =/= undefined,
                    error => ?get(comment_error)
                })
            ]}
        ]}
    ).
```

Comment bodies are user input and are rendered as text, so `<b>` in a comment shows as `<b>`. Keep a long-lived stream as a top-level binding, as here: the live process clears its queue after each diff, which it cannot do for a stream nested inside another value.

```admonish tip title="Bounded streams"
`arizona_stream:new(KeyFun, Items, #{limit => 50, on_limit => drop})` keeps only the newest 50 items visible and evicts the oldest on each append, like `tail -f`. Use it for activity feeds and logs that grow without end.
```

## The comment form

`comment_form/1` is a stateless helper in the same module, embedded with `?stateless(fun comment_form/1, Props)`. Anonymous readers get a link instead of a form:

```erlang
comment_form(#{signed_in := false}) ->
    ?html({p, [{id, ~"comment-login"}], [{a, [{href, ~"/login"}], [~"Log in"]}, ~" to comment."]});
comment_form(Bindings) ->
    ?html(
        {form, [{id, ~"comment-form"}, {az_submit, arizona_js:push_event(~"add_comment")}], [
            {label, [{for, ~"comment-body"}], [~"Add a comment"]},
            {textarea, [{id, ~"comment-body"}, {name, ~"body"}, {rows, ~"3"}], []},
            ?stateless(blog_live_components, field_error, #{error => ?get(error)}),
            {button, [{type, ~"submit"}], [~"Post comment"]}
        ]}
    ).
```

The event handler:

```erlang
handle_event(~"add_comment", #{~"body" := Body}, #{current_user := #{id := UserId}} = Bindings) ->
    #{post := Post, comments := Comments} = Bindings,
    {ok, Author} = blog_repo:get(blog_user, UserId),
    case blog_posts:add_comment(Post, Author, #{~"body" => Body}) of
        {ok, Comment} ->
            ok = blog_notify:comment_created(Post, Author, Comment),
            Item = blog_notify:comment(Comment, Author),
            {
                Bindings#{
                    comments => arizona_stream:insert(Comments, Item), comment_error => undefined
                },
                #{},
                [arizona_js:reset_form(~"#comment-form")]
            };
        {error, Changeset} ->
            {Bindings#{comment_error => first_error(Changeset)}, #{}, [
                arizona_js:focus(~"#comment-body")
            ]}
    end;
handle_event(~"add_comment", _Payload, Bindings) ->
    {Bindings, #{}, []}.
```

```admonish important title="Hiding the form is not access control"
Anyone can send any event to a view they have open: the payload and the event name are just JSON on a WebSocket. The first clause only matches when the view's own `current_user` is set, which came from the server-side session in a middleware. The second clause ignores everyone else. The author is taken from the session, never from the payload, and only the `body` field is passed to the changeset.
```

Details worth copying:

- `blog_posts:add_comment/3` and its changeset do the validation, so an empty comment, or one over 5000 characters, gets the same message as through the API. `first_error/1` takes the first message from `blog_api_json:errors/1`.
- The form is cleared with a `reset_form` effect only when the comment is saved. A rejected comment keeps its text and the cursor goes back to it. (`az_form_reset` would clear it before the server had answered, see [Events & Interactivity](../arizona/events.md#forms).)
- The writer's own page inserts the comment straight away, then broadcasts.

## Hearing other readers

Every connected page of this post is a member of `~"post:42"`. `blog_notify:comment_created/3` sends the comment there, and to the site-wide feed. Each live view gets the envelope in `handle_info/2`:

```erlang
handle_info(#nova_pubsub{sender = Sender}, Bindings) when Sender =:= self() ->
    {Bindings, #{}, []};
handle_info(#nova_pubsub{topic = ~"comment_created", payload = #{id := Id} = Item}, Bindings) ->
    #{comments := Comments} = Bindings,
    {Bindings#{comments => arizona_stream:update(Comments, Id, Item)}, #{}, []};
handle_info(_Message, Bindings) ->
    {Bindings, #{}, []}.
```

- The writer's page has already inserted its own comment, and it is the `sender` of the broadcast, so it skips it.
- Everyone else applies it with `arizona_stream:update/3`, which inserts a key it does not have and replaces one it does. `insert/2` would crash on a duplicate key, and a duplicate can happen: the join-then-read in `mount/1` may already have loaded the comment. Applying the same event twice gives the same list.
- Unknown messages are ignored, so adding a new event to the channel later does not crash pages that are already open.

Comments posted through the JSON API take the same path: the controller calls `blog_notify:comment_created/3` too, and every open page shows the comment. The same broadcast gives the notification feed "bob commented on Hello live", on every live page, through the Nova WebSocket from [Pub/Sub](pubsub.md).

## Saving and publishing

The editor's `save/2`, which [Events & Interactivity](../arizona/events.md) called after validation, now writes to the database:

```erlang
save(Intent, #{post := Post, current_user := User, post_title := Title, body := Body} = Bindings) ->
    case store(Post, User, #{~"title" => Title, ~"body" => Body}) of
        {ok, Saved} when Intent =:= ~"publish" ->
            publish(Saved, Bindings);
        {ok, Saved} ->
            ?send_after(?STATUS_TIMEOUT, clear_status),
            {Bindings#{post => Saved, errors => #{}, status => ~"Draft saved"}, #{}, [
                arizona_js:set_title(Title)
            ]};
        {error, stale} ->
            {Bindings#{status => ~"Someone else changed this post. Reload to see it."}, #{}, []};
        {error, Changeset} ->
            Errors = changeset_errors(Changeset),
            {Bindings#{errors => Errors}, #{}, [arizona_js:focus(first_invalid(Errors))]}
    end.

store(undefined, User, Params) -> blog_posts:create_post(User, Params);
store(Post, _User, Params) -> blog_posts:update_post(Post, Params).
```

- The first save creates the post. Its row goes into the `post` binding, so the next save updates the same post.
- `blog_posts:update_post/2` uses the optimistic lock from [Data Safety](../data-layer/data-safety.md#optimistic-locking). The row in the binding carries the `lock_version` it was read with. If the post was changed elsewhere since then, the update returns `{error, stale}` and the author is told, rather than overwriting the other change.
- The editor checks only the cheap things itself (required fields, title length while typing). The changeset is the final word, and its messages land under the same fields:

```erlang
changeset_errors(Changeset) ->
    Errors = blog_api_json:errors(Changeset),
    #{Field => Message || Field := [Message | _] <- maps:with([title, body], Errors)}.
```

A two-letter title passes the editor's check and fails the schema's `{min, 3}`, so the author sees "should be at least 3 character(s)" under the title.

Publishing stores the post, sets its status and tags in one `kura_multi` (from [Transactions & Bulk Operations](../data-layer/transactions-bulk.md)), tells the feed, and moves the author to the post:

```erlang
publish(Post, #{tags := Tags} = Bindings) ->
    {ok, Published} = blog_posts:publish(Post, Tags),
    ok = blog_notify:post_published(Published),
    Flash = #{~"info" => ~"Your post is live"},
    {Bindings#{post => Published}, #{}, [
        arizona_js:navigate(blog_live_post:path(Published), #{flash => Flash})
    ]}.
```

with

```erlang
path(#{id := Id}) ->
    <<"/live/posts/", (integer_to_binary(Id))/binary>>.
```

`navigate/2` is a live navigation: the post page mounts in the same document, and the flash map travels with it to the page's `flash` binding. "Your post is live" shows above the title. Every other reader's feed shows "New post: ..." at the same moment.

### Editing your own posts

Drafts are listed on the dashboard from [Live Navigation](../arizona/live-navigation.md) and open in the editor at `/live/editor/:id`:

```erlang
{live, ~"/live/editor/:id", blog_live_editor, #{
    layouts => [{blog_live_layout, render}],
    middlewares => [
        {blog_live_middleware, require_user},
        {blog_live_middleware, path_bindings},
        {blog_live_middleware, own_post}
    ]
}},
```

```erlang
own_post(Req, #{~"id" := Id, current_user := #{id := UserId}} = Bindings) ->
    case blog_posts:get_post(Id, [tags]) of
        {ok, #{user_id := UserId} = Post} -> {cont, Req, Bindings#{post => Post}};
        _ -> not_found(Req)
    end.
```

`UserId` is already bound by the function head, so the `case` pattern only matches a post whose `user_id` is the reader's. Someone else's post answers `404`, like a missing one. The middlewares run in order: `own_post/2` relies on `current_user` from `require_user/2` and `~"id"` from `path_bindings/2`.

The editor's `mount/1` already handles a `post` binding. Its title, body and tags are the starting values, and the fields render them through `initial_title` and `initial_body`.

## Trying it

Log in as one user (see [Authentication](../auth-sessions/authentication.md)) and write a post at <http://localhost:8080/live/editor>. Press **Publish**. The editor turns into the post's page with "Your post is live" at the top.

Open the same URL in a private window, where you are not logged in. That page shows "Log in to comment." Post a comment from the first window. It appears in both, and the private window's Latest list says "alice commented on ...". Post another through the API with `curl` as in [Pub/Sub](pubsub.md#broadcasting-from-controllers), and it appears in both again. Reload either page and the comments are still there, because they came from the database.

## Testing it

The new suite, `test/blog_realtime_live_SUITE.erl`, runs against the database with the fixtures from Part II. `init_per_testcase/2` checks out a sandbox in shared mode, so live processes and request handlers use the test's transaction too. It creates two users and a published post:

```erlang
init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    Alice = blog_data_fixtures:user(~"alice"),
    Bob = blog_data_fixtures:user(~"bob"),
    Draft = blog_data_fixtures:post(Alice, #{~"title" => ~"Hello live", ~"body" => ~"One\n\nTwo"}),
    {ok, Post} = blog_posts:publish(Draft, []),
    [{alice, Alice}, {bob, Bob}, {post, Post} | Config].
```

The main test has two WebSocket clients on the same post page, one logged in and one anonymous, and the test process listening to the feed:

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
    receive
        #nova_pubsub{topic = ~"comment_created", payload = Payload} ->
            ?assertEqual(
                #{~"post_id" => PostId, ~"post_title" => ~"Hello live", ~"author" => ~"bob"},
                Payload
            )
    after 2000 -> ct:fail(no_notification)
    end,
    {ok, #{comments := [#{body := ~"Hi from Bob"}]}} = blog_posts:get_post(PostId, [comments]),
    blog_ws_client:close(Bob),
    blog_ws_client:close(Reader).
```

Bob's reply carries the new comment and the `#comment-form` reset. The anonymous reader's frame is the pushed update. The feed got its summary, and the comment is in the database.

The duplicate handling is a plain function test:

```erlang
    {Once, #{}, []} = blog_live_post:handle_info(Envelope, Bindings),
    {Twice, #{}, []} = blog_live_post:handle_info(Envelope, Once),
    ?assertEqual([Comment], arizona_stream:to_list(maps:get(comments, Twice))),
    Own = Envelope#nova_pubsub{sender = self(), payload = Comment#{id => 8}},
    ?assertEqual({Twice, #{}, []}, blog_live_post:handle_info(Own, Twice)).
```

Other cases cover drafts and missing posts answering `404`, an anonymous `add_comment` event changing nothing, a rejected comment keeping focus, the editor's create, update, stale and publish paths, and opening someone else's draft. The comment section was also checked in a headless Chrome with two pages open side by side, one logged in and one anonymous.

```shell
rebar3 ct --suite blog_realtime_live_SUITE
```

```
All 15 tests passed.
```

---

The blog now has live pages, a live comment section and a site-wide feed. [Testing Real-Time](../testing/testing-realtime.md) goes deeper into testing them.
