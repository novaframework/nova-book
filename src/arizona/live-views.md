# Live Views

A live view is a server-side process that renders HTML and responds to user events. It's the core building block of Arizona.

## Creating a live view

A live view implements two required callbacks: `mount/1` and `render/1`.

```erlang
-module(blog_counter_live).
-behaviour(arizona_live_view).

-export([mount/1, render/1, handle_event/3]).

mount(_Params) ->
    {ok, #{count => 0}}.

render(State) ->
    arizona:render("
        <div>
            <h1>Count: ~{integer_to_list(maps:get(count, State))}</h1>
            <button az-click=\"increment\">+1</button>
            <button az-click=\"decrement\">-1</button>
        </div>
    ").

handle_event(<<"increment">>, _Params, State) ->
    NewState = State#{count => maps:get(count, State) + 1},
    {noreply, NewState};
handle_event(<<"decrement">>, _Params, State) ->
    NewState = State#{count => maps:get(count, State) - 1},
    {noreply, NewState}.
```

### mount/1

Called when the live view is first loaded. Receives connection parameters and returns `{ok, InitialState}`. The state is a map — it holds all data the view needs.

### render/1

Called whenever state changes. Returns the HTML template. Arizona diffs the output against the previous render and only sends changes to the client.

### handle_event/3

Called when the user triggers an event (click, form submit, key press). Receives the event name, event parameters, and current state. Returns `{noreply, NewState}` to update the state (which triggers a re-render).

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
-behaviour(arizona_live_view).

-export([mount/1, render/1, handle_event/3]).

mount(#{<<"id">> := PostId}) ->
    {ok, Post} = blog_repo:get(post, binary_to_integer(PostId)),
    {ok, #{post => Post, editing => false, saved => false}}.

render(#{editing := false, post := Post} = _State) ->
    arizona:render("
        <article>
            <h1>~{maps:get(title, Post)}</h1>
            <div>~{maps:get(body, Post)}</div>
            <button az-click=\"edit\">Edit</button>
        </article>
    ");
render(#{editing := true, post := Post} = _State) ->
    arizona:render("
        <form az-submit=\"save\">
            <input type=\"text\" name=\"title\"
                   value=\"~{maps:get(title, Post)}\" />
            <textarea name=\"body\">~{maps:get(body, Post)}</textarea>
            <button type=\"submit\">Save</button>
            <button type=\"button\" az-click=\"cancel\">Cancel</button>
        </form>
    ").

handle_event(<<"edit">>, _Params, State) ->
    {noreply, State#{editing => true}};
handle_event(<<"cancel">>, _Params, State) ->
    {noreply, State#{editing => false}};
handle_event(<<"save">>, Params, #{post := Post} = State) ->
    CS = post:changeset(Post, Params),
    case blog_repo:update(CS) of
        {ok, Updated} ->
            {noreply, State#{post => Updated, editing => false, saved => true}};
        {error, _CS} ->
            {noreply, State}
    end.
```

The form submits over the WebSocket — no HTTP round trip, no page reload. The state updates and Arizona re-renders just the changed parts.

---

Next, let's build reusable UI pieces with [Components](components.md).
