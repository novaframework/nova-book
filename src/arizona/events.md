# Events & Interactivity

Arizona's event system connects user interactions in the browser to server-side Erlang functions. Every click, form submission, and key press travels over the WebSocket to your `handle_event/3` callback.

## Event handling

The `handle_event/3` callback receives three arguments:

```erlang
handle_event(EventName, Params, State) ->
    {noreply, NewState} |           %% Update state, re-render
    {reply, ReplyMap, NewState} |   %% Send data back to JS caller
    {noreply, NewState, Actions}.   %% Update state + dispatch actions
```

### Return values

| Return | Effect |
|---|---|
| `{noreply, State}` | Update state and re-render |
| `{reply, Map, State}` | Send a reply to a JS `callEvent` caller |
| `{noreply, State, [{redirect, Path}]}` | Navigate to a new page |
| `{noreply, State, [{dispatch, Event, Payload}]}` | Dispatch an event to another component |

## Form handling

Forms are the most common interactive pattern:

```erlang
render(#{changeset := CS, errors := Errors} = _State) ->
    arizona:render("
        <form az-submit=\"save\" az-change=\"validate\">
            <input type=\"text\" name=\"title\"
                   value=\"~{maps:get(title, kura_changeset:apply_changes(CS))}\" />
            ~{render_error(Errors, title)}

            <textarea name=\"body\">~{maps:get(body, kura_changeset:apply_changes(CS))}</textarea>
            ~{render_error(Errors, body)}

            <button type=\"submit\">Save</button>
        </form>
    ").

handle_event(<<"validate">>, Params, State) ->
    CS = post:changeset(#{}, Params),
    Errors = changeset_errors_to_json(CS),
    {noreply, State#{changeset => CS, errors => Errors}};

handle_event(<<"save">>, Params, State) ->
    CS = post:changeset(#{}, Params),
    case blog_repo:insert(CS) of
        {ok, Post} ->
            {noreply, State, [{redirect, "/posts/" ++ integer_to_list(maps:get(id, Post))}]};
        {error, CS1} ->
            {noreply, State#{changeset => CS1, errors => changeset_errors_to_json(CS1)}}
    end.
```

The `render_error/2` helper formats a field's error for display:

```erlang
render_error(Errors, Field) ->
    case maps:get(atom_to_binary(Field), Errors, []) of
        [] -> <<>>;
        [Msg | _] -> arizona:render("<span class=\"error\">~{Msg}</span>")
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
handle_event(<<"delete">>, #{<<"id">> := Id, <<"type">> := Type}, State) ->
    ...
```

## Key events

```html
<input type="text" az-keydown="search" az-debounce="300" />
```

The `az-debounce` attribute delays the event by the specified milliseconds — useful for search-as-you-type to avoid flooding the server.

```erlang
handle_event(<<"search">>, #{<<"value">> := Query}, State) ->
    Q = kura_query:from(post),
    Q1 = kura_query:where(Q, {title, ilike, <<"%", Query/binary, "%">>}),
    {ok, Results} = blog_repo:all(Q1),
    {noreply, State#{results => Results}}.
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
%% For callEvent — must return {reply, ...}
handle_event(<<"get_data">>, #{<<"id">> := Id}, State) ->
    {ok, Post} = blog_repo:get(post, binary_to_integer(Id)),
    {reply, #{title => maps:get(title, Post)}, State}.
```

## Actions

Actions let you trigger side effects alongside state updates:

```erlang
handle_event(<<"publish">>, _Params, #{post := Post} = State) ->
    CS = post:changeset(Post, #{<<"status">> => <<"published">>}),
    {ok, Updated} = blog_repo:update(CS),
    Actions = [
        {dispatch, <<"post_published">>, #{id => maps:get(id, Updated)}},
        {redirect, "/posts/" ++ integer_to_list(maps:get(id, Updated))}
    ],
    {noreply, State#{post => Updated}, Actions}.
```

---

Next: [Live Navigation](live-navigation.md) — navigating between live views without full page reloads.
