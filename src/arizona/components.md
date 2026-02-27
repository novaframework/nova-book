# Components

Live views render entire pages. Components extract reusable pieces — a comment form, a notification badge, a tag selector. Arizona has two types: **stateful** components (with their own state and event handlers) and **stateless** components (pure render functions).

## Stateless components

A stateless component is a function that takes assigns and returns rendered HTML. It has no state and no event handling — just rendering.

```erlang
-module(blog_components).
-export([post_card/1, tag_badge/1, user_avatar/1]).

post_card(#{post := Post}) ->
    arizona:render("
        <article class=\"post-card\">
            <h2>~{maps:get(title, Post)}</h2>
            <p class=\"meta\">by ~{maps:get(username, maps:get(author, Post))}</p>
            <p>~{binary:part(maps:get(body, Post), 0, min(200, byte_size(maps:get(body, Post))))}...</p>
        </article>
    ").

tag_badge(#{tag := Tag}) ->
    arizona:render("
        <span class=\"tag\">~{maps:get(name, Tag)}</span>
    ").

user_avatar(#{user := User}) ->
    arizona:render("
        <div class=\"avatar\">
            <span>~{binary:part(maps:get(username, User), 0, 1)}</span>
        </div>
    ").
```

Use stateless components in a live view:

```erlang
render(#{posts := Posts} = _State) ->
    arizona:render("
        <div class=\"post-list\">
            ~{[blog_components:post_card(#{post => P}) || P <- Posts]}
        </div>
    ").
```

## Stateful components

A stateful component has its own state, handles its own events, and re-renders independently of its parent. Each stateful component must have a unique `id`.

```erlang
-module(blog_comment_form).
-behaviour(arizona_live_component).

-export([mount/1, render/1, handle_event/3]).

mount(#{post_id := PostId}) ->
    {ok, #{post_id => PostId, body => <<>>, error => undefined}}.

render(State) ->
    arizona:render("
        <div id=\"comment-form\">
            ~{case maps:get(error, State) of
                undefined -> <<>>;
                Err -> <<\"<p class='error'>\", Err/binary, \"</p>\">>
            end}
            <form az-submit=\"submit_comment\">
                <textarea name=\"body\" az-change=\"validate\"
                    placeholder=\"Write a comment...\">~{maps:get(body, State)}</textarea>
                <button type=\"submit\">Post Comment</button>
            </form>
        </div>
    ").

handle_event(<<"validate">>, #{<<"body">> := Body}, State) ->
    Error = case byte_size(Body) of
        0 -> <<"Comment cannot be empty">>;
        _ -> undefined
    end,
    {noreply, State#{body => Body, error => Error}};
handle_event(<<"submit_comment">>, #{<<"body">> := Body},
             #{post_id := PostId} = State) ->
    CS = comment:changeset(#{}, #{<<"body">> => Body, <<"post_id">> => PostId}),
    case blog_repo:insert(CS) of
        {ok, _Comment} ->
            {noreply, State#{body => <<>>, error => undefined}};
        {error, _} ->
            {noreply, State#{error => <<"Failed to post comment">>}}
    end.
```

Embed a stateful component in a live view:

```erlang
render(#{post := Post} = _State) ->
    arizona:render("
        <article>
            <h1>~{maps:get(title, Post)}</h1>
            ~{arizona:component(blog_comment_form, #{id => \"comment-form\",
                                                      post_id => maps:get(id, Post)})}
        </article>
    ").
```

The `id` is required — Arizona uses it to track the component instance across re-renders.

## Slots

Slots let components accept nested content from their parent, enabling flexible composition:

```erlang
-module(blog_card).
-export([render/1]).

render(#{title := Title, inner_content := InnerContent}) ->
    arizona:render("
        <div class=\"card\">
            <div class=\"card-header\">
                <h3>~{Title}</h3>
            </div>
            <div class=\"card-body\">
                ~{InnerContent}
            </div>
        </div>
    ").
```

## When to use which

| Use case | Component type |
|---|---|
| Display-only UI pieces (badges, cards, avatars) | Stateless |
| Interactive forms, toggles, dropdowns | Stateful |
| UI that needs its own event handling | Stateful |
| Layout wrappers, formatting helpers | Stateless |

Stateless components re-render when their parent re-renders. Stateful components re-render independently — only when their own state changes.

---

Next, let's handle user interactions in depth with [Events & Interactivity](events.md).
