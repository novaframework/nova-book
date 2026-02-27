# Live Views

A live view is a server-side process that renders HTML and responds to user events. It's the core building block of Arizona.

## Creating a live view

A live view implements the `arizona_view` behaviour with two required callbacks: `mount/2` and `render/1`.

```erlang
-module(blog_counter_live).
-compile({parse_transform, arizona_parse_transform}).
-behaviour(arizona_view).

-export([mount/2, render/1, handle_event/3]).

mount(_Params, _Req) ->
    arizona_view:new(?MODULE, #{
        id => ~"counter",
        count => 0
    }, none).

render(Bindings) ->
    arizona_template:from_html(~"""
    <div>
        <h1>Count: {integer_to_list(arizona_template:get_binding(count, Bindings))}</h1>
        <button az-click="increment">+1</button>
        <button az-click="decrement">-1</button>
    </div>
    """).

handle_event(<<"increment">>, _Params, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    NewState = arizona_stateful:put_binding(count, Count + 1, State),
    {[], arizona_view:update_state(NewState, View)};
handle_event(<<"decrement">>, _Params, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    NewState = arizona_stateful:put_binding(count, Count - 1, State),
    {[], arizona_view:update_state(NewState, View)}.
```

### mount/2

Called when the live view is first loaded. Receives the mount argument and an Arizona request, and returns a new view created with `arizona_view:new/3`. The third argument is a layout module (`none` for no layout).

### render/1

Called whenever state changes. Receives the current bindings as a map and returns an Arizona template. Arizona diffs the output against the previous render and only sends changes to the client. Use `arizona_template:get_binding/2` to access bindings — this enables Arizona's dependency tracking for differential updates.

### handle_event/3

Called when the user triggers an event (click, form submit, key press). Receives the event name, event parameters, and current view. Returns `{Actions, UpdatedView}` where Actions is a list of action tuples (empty list for no actions).

State is managed through the `arizona_stateful` API:
- `arizona_view:get_state(View)` — get the stateful state from the view
- `arizona_stateful:get_binding(Key, State)` — read a binding
- `arizona_stateful:put_binding(Key, Value, State)` — update a binding
- `arizona_view:update_state(State, View)` — put the updated state back into the view

## Event bindings

Arizona uses `az-` attributes to bind DOM events to server-side handlers:

| Attribute | Triggers on |
|---|---|
| `az-click` | Click |
| `az-submit` | Form submission |
| `az-change` | Input change |
| `az-keydown` | Key press |
| `az-keyup` | Key release |
| `az-focus` | Element focus |
| `az-blur` | Element blur |

```html
<button az-click="delete" az-value-id="42">Delete</button>
```

The `az-value-*` attributes send additional data with the event. In this case, `handle_event` receives `#{<<"id">> => <<"42">>}` as the params.

## Routing a live view

Add the live view to your Nova router:

```erlang
#{prefix => "",
  security => false,
  routes => [
      {"/counter", blog_counter_live, #{protocol => live_view}}
  ]}
```

The `protocol => live_view` option tells Nova to handle this route with Arizona's live view protocol.

## A blog-relevant example: live post editor

```erlang
-module(blog_post_editor_live).
-compile({parse_transform, arizona_parse_transform}).
-behaviour(arizona_view).

-export([mount/2, render/1, handle_event/3]).

mount(#{<<"id">> := PostId}, _Req) ->
    {ok, Post} = blog_repo:get(post, binary_to_integer(PostId)),
    arizona_view:new(?MODULE, #{
        id => ~"post_editor",
        post => Post,
        editing => false,
        saved => false
    }, none).

render(Bindings) ->
    case arizona_template:get_binding(editing, Bindings) of
        false ->
            Post = arizona_template:get_binding(post, Bindings),
            arizona_template:from_html(~"""
            <article>
                <h1>{maps:get(title, Post)}</h1>
                <div>{maps:get(body, Post)}</div>
                <button az-click="edit">Edit</button>
            </article>
            """);
        true ->
            Post = arizona_template:get_binding(post, Bindings),
            arizona_template:from_html(~"""
            <form az-submit="save">
                <input type="text" name="title"
                       value="{maps:get(title, Post)}" />
                <textarea name="body">{maps:get(body, Post)}</textarea>
                <button type="submit">Save</button>
                <button type="button" az-click="cancel">Cancel</button>
            </form>
            """)
    end.

handle_event(<<"edit">>, _Params, View) ->
    State = arizona_view:get_state(View),
    NewState = arizona_stateful:put_binding(editing, true, State),
    {[], arizona_view:update_state(NewState, View)};
handle_event(<<"cancel">>, _Params, View) ->
    State = arizona_view:get_state(View),
    NewState = arizona_stateful:put_binding(editing, false, State),
    {[], arizona_view:update_state(NewState, View)};
handle_event(<<"save">>, Params, View) ->
    State = arizona_view:get_state(View),
    Post = arizona_stateful:get_binding(post, State),
    CS = post:changeset(Post, Params),
    case blog_repo:update(CS) of
        {ok, Updated} ->
            S1 = arizona_stateful:put_binding(post, Updated, State),
            S2 = arizona_stateful:put_binding(editing, false, S1),
            S3 = arizona_stateful:put_binding(saved, true, S2),
            {[], arizona_view:update_state(S3, View)};
        {error, _CS} ->
            {[], View}
    end.
```

The form submits over the WebSocket — no HTTP round trip, no page reload. The state updates and Arizona re-renders just the changed parts.

---

Next, let's build reusable UI pieces with [Components](components.md).
