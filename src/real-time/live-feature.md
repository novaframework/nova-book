# Building a Live Feature

Let's bring everything together — Arizona live views, Nova PubSub, and Kura — to build a real-time comment section for our blog. When anyone posts a comment, all viewers see it instantly.

## The live comment section

```erlang
-module(blog_post_live).
-compile({parse_transform, arizona_parse_transform}).
-behaviour(arizona_view).

-export([mount/2, render/1, handle_event/3, handle_info/2]).

mount(#{<<"id">> := PostId}, _Req) ->
    Id = binary_to_integer(PostId),
    {ok, Post} = blog_repo:get(post, Id),
    Post1 = blog_repo:preload(post, Post, [{comments, [author]}]),

    %% Subscribe to real-time comment updates
    Channel = list_to_atom("comments_" ++ integer_to_list(Id)),
    nova_pubsub:join(Channel),

    arizona_view:new(?MODULE, #{
        id => list_to_binary("post_live_" ++ integer_to_list(Id)),
        post => Post1,
        comments => maps:get(comments, Post1, []),
        new_comment => <<>>,
        channel => Channel
    }, none).

render(Bindings) ->
    Post = arizona_template:get_binding(post, Bindings),
    Comments = arizona_template:get_binding(comments, Bindings),
    arizona_template:from_html(~"""
    <article>
        <h1>{maps:get(title, Post)}</h1>
        <div class="body">{maps:get(body, Post)}</div>
    </article>

    <section class="comments">
        <h2>Comments ({integer_to_list(length(Comments))})</h2>

        {arizona_template:render_list(Comments, fun(C) ->
            render_comment(C)
        end)}

        <form az-submit="post_comment">
            <textarea name="body" placeholder="Write a comment..."
                      az-change="update_comment">{arizona_template:get_binding(new_comment, Bindings)}</textarea>
            <button type="submit">Post Comment</button>
        </form>
    </section>
    """).

handle_event(<<"update_comment">>, #{<<"body">> := Body}, View) ->
    State = arizona_view:get_state(View),
    NewState = arizona_stateful:put_binding(new_comment, Body, State),
    {[], arizona_view:update_state(NewState, View)};

handle_event(<<"post_comment">>, #{<<"body">> := Body}, View) ->
    State = arizona_view:get_state(View),
    Post = arizona_stateful:get_binding(post, State),
    Channel = arizona_stateful:get_binding(channel, State),
    PostId = maps:get(id, Post),
    CS = comment:changeset(#{}, #{<<"body">> => Body,
                                   <<"post_id">> => PostId,
                                   <<"user_id">> => 1}),
    case blog_repo:insert(CS) of
        {ok, Comment} ->
            Comment1 = blog_repo:preload(comment, Comment, [author]),
            %% Broadcast to all viewers
            nova_pubsub:broadcast(Channel, "new_comment", Comment1),
            NewState = arizona_stateful:put_binding(new_comment, <<>>, State),
            {[], arizona_view:update_state(NewState, View)};
        {error, _} ->
            {[], View}
    end.

%% Receive broadcasts from PubSub
handle_info({nova_pubsub, _Channel, _Sender, "new_comment", Comment}, View) ->
    State = arizona_view:get_state(View),
    Comments = arizona_stateful:get_binding(comments, State),
    NewState = arizona_stateful:put_binding(comments, Comments ++ [Comment], State),
    {[], arizona_view:update_state(NewState, View)}.

%% Helpers

render_comment(Comment) ->
    arizona_template:from_html(~"""
    <div class="comment">
        <strong>{maps:get(username, maps:get(author, Comment))}</strong>
        <p>{maps:get(body, Comment)}</p>
    </div>
    """).
```

## How it works

1. When a user visits `/posts/42`, Arizona mounts `blog_post_live` with the post ID
2. The mount function loads the post with comments and subscribes to PubSub
3. Arizona renders the HTML and sends it to the browser
4. When someone submits a comment:
   - The comment is saved to the database via Kura
   - The comment is broadcast via Nova PubSub
   - All subscribed live views receive the broadcast in `handle_info/2`
   - Each live view updates its state with the new comment
   - Arizona diffs the HTML and pushes only the new comment to each client

## Broadcasting from controllers

You can also broadcast from traditional Nova controllers. If comments are also created via the JSON API:

```erlang
%% In blog_comments_controller.erl
create(#{json := Params}) ->
    CS = comment:changeset(#{}, Params),
    case blog_repo:insert(CS) of
        {ok, Comment} ->
            PostId = maps:get(post_id, Comment),
            Comment1 = blog_repo:preload(comment, Comment, [author]),
            Channel = list_to_atom("comments_" ++ integer_to_list(PostId)),
            nova_pubsub:broadcast(Channel, "new_comment", Comment1),
            {json, 201, #{}, comment_to_json(Comment1)};
        {error, CS1} ->
            {json, 422, #{}, #{errors => changeset_errors_to_json(CS1)}}
    end.

comment_to_json(#{id := Id, body := Body, post_id := PostId, inserted_at := At}) ->
    #{id => Id, body => Body, post_id => PostId, inserted_at => At}.
```

Both live views and WebSocket handlers receive the broadcast — any process that called `nova_pubsub:join(Channel)` gets the message.

## Optimistic updates

For a snappier feel, update the UI immediately and reconcile later:

```erlang
handle_event(<<"post_comment">>, #{<<"body">> := Body}, View) ->
    State = arizona_view:get_state(View),
    Post = arizona_stateful:get_binding(post, State),
    Comments = arizona_stateful:get_binding(comments, State),
    PostId = maps:get(id, Post),
    %% Optimistic: show the comment immediately
    TempComment = #{body => Body, author => #{username => <<"you">>},
                    id => temp, post_id => PostId},
    S1 = arizona_stateful:put_binding(comments, Comments ++ [TempComment], State),
    S2 = arizona_stateful:put_binding(new_comment, <<>>, S1),

    %% Persist in background
    CS = comment:changeset(#{}, #{<<"body">> => Body,
                                   <<"post_id">> => PostId,
                                   <<"user_id">> => 1}),
    blog_repo:insert(CS),
    {[], arizona_view:update_state(S2, View)}.
```

---

With our live feature complete, let's add email notifications. Next: [Sending Email](../email/sending-email.md).
