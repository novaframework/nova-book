# Events & Interactivity

Arizona's event system connects user interactions in the browser to server-side Erlang functions. Every click, form submission, and key press travels over the WebSocket to your `handle_event/3` callback.

## Event handling

The `handle_event/3` callback receives three arguments:

```erlang
handle_event(EventName, Params, View) ->
    {Actions, UpdatedView}.
```

State is managed through the `arizona_stateful` and `arizona_view` APIs — get the state from the view, update bindings, and put it back.

### Return values

The return is always `{Actions, View}` where `Actions` is a list:

| Action | Effect |
|---|---|
| `[]` | No actions — just update state and re-render |
| `[{redirect, Path}]` | Navigate to a new page |
| `[{dispatch, Event, Payload}]` | Dispatch an event to another component |

## Form handling

Forms are the most common interactive pattern:

```erlang
render(Bindings) ->
    CS = arizona_template:get_binding(changeset, Bindings),
    Errors = arizona_template:get_binding(errors, Bindings),
    arizona_template:from_html(~"""
    <form az-submit="save" az-change="validate">
        <input type="text" name="title"
               value="{maps:get(title, kura_changeset:apply_changes(CS))}" />
        {render_error(Errors, title)}

        <textarea name="body">{maps:get(body, kura_changeset:apply_changes(CS))}</textarea>
        {render_error(Errors, body)}

        <button type="submit">Save</button>
    </form>
    """).

handle_event(<<"validate">>, Params, View) ->
    State = arizona_view:get_state(View),
    CS = post:changeset(#{}, Params),
    Errors = changeset_errors_to_json(CS),
    S1 = arizona_stateful:put_binding(changeset, CS, State),
    S2 = arizona_stateful:put_binding(errors, Errors, S1),
    {[], arizona_view:update_state(S2, View)};

handle_event(<<"save">>, Params, View) ->
    CS = post:changeset(#{}, Params),
    case blog_repo:insert(CS) of
        {ok, Post} ->
            {[{redirect, "/posts/" ++ integer_to_list(maps:get(id, Post))}], View};
        {error, CS1} ->
            State = arizona_view:get_state(View),
            S1 = arizona_stateful:put_binding(changeset, CS1, State),
            S2 = arizona_stateful:put_binding(errors, changeset_errors_to_json(CS1), S1),
            {[], arizona_view:update_state(S2, View)}
    end.
```

The `render_error/2` helper formats a field's error for display:

```erlang
render_error(Errors, Field) ->
    case maps:get(atom_to_binary(Field), Errors, []) of
        [] -> <<>>;
        [Msg | _] -> arizona_template:from_html(~"<span class=\"error\">{Msg}</span>")
    end.
```

The `az-change` attribute triggers validation on every input change — giving users instant feedback without a form submission.

## Passing values with events

Use `az-value-*` attributes to send data with events:

```html
<button az-click="delete" az-value-id="42" az-value-type="post">Delete</button>
```

In `handle_event`:

```erlang
handle_event(<<"delete">>, #{<<"id">> := Id, <<"type">> := Type}, View) ->
    ...
```

## Key events

```html
<input type="text" az-keydown="search" az-debounce="300" />
```

The `az-debounce` attribute delays the event by the specified milliseconds — useful for search-as-you-type to avoid flooding the server.

```erlang
handle_event(<<"search">>, #{<<"value">> := Query}, View) ->
    Q = kura_query:from(post),
    Q1 = kura_query:where(Q, {title, ilike, <<"%", Query/binary, "%">>}),
    {ok, Results} = blog_repo:all(Q1),
    State = arizona_view:get_state(View),
    NewState = arizona_stateful:put_binding(results, Results, State),
    {[], arizona_view:update_state(NewState, View)}.
```

## Client-side JavaScript hooks

Arizona exposes a JavaScript API for pushing events from custom JS code:

```javascript
// Push an event to the live view
arizona.pushEvent("my_event", {key: "value"});

// Push to a specific component by ID
arizona.pushEventTo("#comment-form", "submit", {body: "Hello"});

// Call an event and get a reply
const result = await arizona.callEvent("get_data", {id: 42});

// Call on a specific component
const result = await arizona.callEventFrom("#search", "search", {q: "nova"});
```

On the server:

```erlang
handle_event(<<"get_data">>, #{<<"id">> := Id}, View) ->
    {ok, Post} = blog_repo:get(post, binary_to_integer(Id)),
    {[{dispatch, <<"get_data_reply">>, #{title => maps:get(title, Post)}}], View}.
```

## Actions

Actions let you trigger side effects alongside state updates:

```erlang
handle_event(<<"publish">>, _Params, View) ->
    State = arizona_view:get_state(View),
    Post = arizona_stateful:get_binding(post, State),
    CS = post:changeset(Post, #{<<"status">> => <<"published">>}),
    {ok, Updated} = blog_repo:update(CS),
    NewState = arizona_stateful:put_binding(post, Updated, State),
    Actions = [
        {dispatch, <<"post_published">>, #{id => maps:get(id, Updated)}},
        {redirect, "/posts/" ++ integer_to_list(maps:get(id, Updated))}
    ],
    {Actions, arizona_view:update_state(NewState, View)}.
```

---

Next: [Live Navigation](live-navigation.md) — navigating between live views without full page reloads.
