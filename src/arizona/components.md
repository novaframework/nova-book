# Components

Live views render entire pages. Components extract reusable pieces — a comment form, a notification badge, a tag selector. Arizona has two types: **stateful** components (with their own state and event handlers) and **stateless** components (pure render functions).

## Stateless components

A stateless component is a function that takes bindings and returns a template. It has no state and no event handling — just rendering.

```erlang
-module(blog_components).
-compile({parse_transform, arizona_parse_transform}).
-export([post_card/1, tag_badge/1, user_avatar/1]).

post_card(Bindings) ->
    Post = maps:get(post, Bindings),
    arizona_template:from_html(~"""
    <article class="post-card">
        <h2>{maps:get(title, Post)}</h2>
        <p class="meta">by {maps:get(username, maps:get(author, Post))}</p>
        <p>{binary:part(maps:get(body, Post), 0, min(200, byte_size(maps:get(body, Post))))}...</p>
    </article>
    """).

tag_badge(Bindings) ->
    Tag = maps:get(tag, Bindings),
    arizona_template:from_html(~"""
    <span class="tag">{maps:get(name, Tag)}</span>
    """).

user_avatar(Bindings) ->
    User = maps:get(user, Bindings),
    arizona_template:from_html(~"""
    <div class="avatar">
        <span>{binary:part(maps:get(username, User), 0, 1)}</span>
    </div>
    """).
```

Use stateless components in a live view with `arizona_template:render_stateless/3`:

```erlang
render(Bindings) ->
    arizona_template:from_html(~"""
    <div class="post-list">
        {arizona_template:render_list(
            arizona_template:get_binding(posts, Bindings),
            fun(P) ->
                arizona_template:render_stateless(blog_components, post_card, #{post => P})
            end)}
    </div>
    """).
```

## Stateful components

A stateful component has its own state, handles its own events, and re-renders independently of its parent. Each stateful component must have a unique `id` binding.

```erlang
-module(blog_comment_form).
-compile({parse_transform, arizona_parse_transform}).
-behaviour(arizona_stateful).

-export([mount/1, render/1, handle_event/3]).

mount(Bindings) ->
    arizona_stateful:new(?MODULE, #{
        id => maps:get(id, Bindings),
        post_id => maps:get(post_id, Bindings),
        body => <<>>,
        error => undefined
    }).

render(Bindings) ->
    arizona_template:from_html(~"""
    <div id="comment-form">
        {case arizona_template:get_binding(error, Bindings) of
            undefined -> ~"";
            Err -> arizona_template:from_html(~"<p class='error'>{Err}</p>")
        end}
        <form az-submit="submit_comment">
            <textarea name="body" az-change="validate"
                placeholder="Write a comment...">{arizona_template:get_binding(body, Bindings)}</textarea>
            <button type="submit">Post Comment</button>
        </form>
    </div>
    """).

handle_event(<<"validate">>, #{<<"body">> := Body}, State) ->
    Error = case byte_size(Body) of
        0 -> <<"Comment cannot be empty">>;
        _ -> undefined
    end,
    S1 = arizona_stateful:put_binding(body, Body, State),
    S2 = arizona_stateful:put_binding(error, Error, S1),
    {[], S2};
handle_event(<<"submit_comment">>, #{<<"body">> := Body}, State) ->
    PostId = arizona_stateful:get_binding(post_id, State),
    CS = comment:changeset(#{}, #{<<"body">> => Body, <<"post_id">> => PostId}),
    case blog_repo:insert(CS) of
        {ok, _Comment} ->
            S1 = arizona_stateful:put_binding(body, <<>>, State),
            S2 = arizona_stateful:put_binding(error, undefined, S1),
            {[], S2};
        {error, _} ->
            S1 = arizona_stateful:put_binding(error, <<"Failed to post comment">>, State),
            {[], S1}
    end.
```

Embed a stateful component in a live view with `arizona_template:render_stateful/2`:

```erlang
render(Bindings) ->
    Post = arizona_template:get_binding(post, Bindings),
    arizona_template:from_html(~"""
    <article>
        <h1>{maps:get(title, Post)}</h1>
        {arizona_template:render_stateful(blog_comment_form, #{
            id => ~"comment-form",
            post_id => maps:get(id, Post)
        })}
    </article>
    """).
```

The `id` is required — Arizona uses it to track the component instance across re-renders.

## Slots

Slots let components accept nested content from their parent, enabling flexible composition:

```erlang
-module(blog_card).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_html(~"""
    <div class="card">
        <div class="card-header">
            <h3>{maps:get(title, Bindings)}</h3>
        </div>
        <div class="card-body">
            {arizona_template:render_slot(maps:get(inner_content, Bindings))}
        </div>
    </div>
    """).
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
