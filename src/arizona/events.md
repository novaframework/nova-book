# Events & Interactivity

Everything a reader does on a live page reaches the server as an *event*: a name, a payload and the view it belongs to. This chapter looks at how events are declared, what their payloads contain, how forms and keys work, and the client-side *effects* that run without a round trip.

## Declaring events

An event is an `az_<domevent>` attribute whose value is a command, usually `arizona_js:push_event/1,2`:

```erlang
{button, [{id, ~"inc"}, {az_click, arizona_js:push_event(~"inc")}], [~"+"]}
```

Any DOM event name works: `az_click`, `az_submit`, `az_input`, `az_change`, `az_keydown`, `az_focusout`, and so on. The parse transform records which `az_*` names a page uses and the client listens for exactly those. The value can also be a list of commands, run in order.

The event goes to the closest view around the element: the page, or a stateful component inside it. That view's `handle_event/3` gets the event name as a binary.

## Payloads

`push_event/1` collects a payload from the element that carries the attribute:

| Trigger element | Payload |
|---|---|
| a `<form>` | all its fields, keyed by `name` |
| an `<input>`, `<select>` or `<textarea>` | `#{~"value" => ...}` (plus `~"checked"` for checkboxes) |
| anything else | `#{}` |

So an `az_input` on the editor's form sends both fields on every keystroke, and an `az_keydown` on an input sends its current value. A plain button inside a form does *not* send the form; only submitting does.

`push_event/2` adds explicit data, merged over the collected payload. The tag input's remove buttons say which tag:

```erlang
{az_click, arizona_js:push_event(~"remove_tag", #{~"tag" => Tag})}
```

```erlang
handle_event(~"remove_tag", #{~"tag" := Tag}, Bindings) ->
```

Payloads arrive as JSON-decoded maps with binary keys. Everything in them came from the browser and can be changed by the user, including the `tag` above and any hidden id you put in a payload. Validate it as you would a request body, and never turn it into an atom.

## Forms

A live form is a `<form>` with `az_submit`. Arizona prevents the browser's own submit and sends the fields as the payload. The payload also carries the `name` and `value` of the button that submitted the form, just as a normal form post would. The editor has two submit buttons, `intent=save` and `intent=publish`, and one handler for both:

```erlang
handle_event(~"save", #{~"title" := Title, ~"body" := Body} = Params, Bindings) ->
    case validate(Title, Body) of
        Errors when map_size(Errors) =:= 0 ->
            Intent = maps:get(~"intent", Params, ~"save"),
            save(Intent, Bindings#{post_title => Title, body => Body});
        Errors ->
            {Bindings#{errors => Errors}, #{}, [arizona_js:focus(first_invalid(Errors))]}
    end.
```

with

```erlang
first_invalid(#{title := _}) -> ~"#post-title";
first_invalid(#{body := _}) -> ~"#post-body";
first_invalid(#{}) -> ~"#post-title".
```

On failure the errors render under their fields (through the `field_error` component) and the cursor jumps to the first bad field. Pressing Enter in the title field submits with the first button, so `intent` is `save`; the `maps:get/3` default covers a submit with no button at all. `save/2` writes the post to the database. On success it shows "Draft saved", gives the browser tab the post's title and arms the timer from [Live Views](live-views.md):

```erlang
        {ok, Saved} ->
            ?send_after(?STATUS_TIMEOUT, clear_status),
            {Bindings#{post => Saved, errors => #{}, status => ~"Draft saved"}, #{}, [
                arizona_js:set_title(Title)
            ]};
```

The rest of `save/2`, including publishing, is in [Building a Live Feature](../real-time/live-feature.md).

A few form details:

- `az_form_reset` (a bare attribute on the form) clears the fields as soon as the event is sent. It does not wait for the server, so a submit the server rejects loses what was typed. To clear a form only when the server accepts it, return `arizona_js:reset_form(Selector)` as an effect instead. The tag input and the comment form both do that.
- Checkbox groups, `<select multiple>` and fields whose name ends in `[]` always arrive as lists, even with one value. Use `name="tags[]"` for a group whose size comes from data.
- Live events travel over the Origin-checked WebSocket, so live forms need no CSRF token. A form that posts to a Nova controller still needs `nova_csrf_plugin` and its token.
- Arizona gives every form `method="post"`, so with JavaScript disabled the browser posts to the page's URL. A `live` route only accepts `GET`, so that answers `405` rather than doing anything surprising.

### Live validation and debouncing

`az_input` fires on every keystroke. Add `az_debounce` (milliseconds) to send only when typing pauses, or `az_throttle` to send at most once per interval:

```erlang
{form,
    [
        {id, ~"post-form"},
        {az_input, arizona_js:push_event(~"preview")},
        {az_debounce, ~"150"},
        {az_submit, arizona_js:push_event(~"save")}
    ],
    ...
```

Both attributes are read from the element that carries the event attribute, here the form. The `preview` handler from [Live Views](live-views.md) re-validates on each event, which is how the "title too long" message appears while you type.

## Key events

`arizona_js:on_key(Key, Command)` wraps a command so it only runs for matching keys. `Key` is a lowercase key name (`enter`, `escape`, `arrowdown`), a list of them, or a regex as a binary. The tag input clears its field on Escape:

```erlang
{az_keydown, arizona_js:on_key(escape, arizona_js:reset_form(~"#tag-form"))}
```

`reset_form/1` is a client-side command, so this never reaches the server. To send an event on a key, wrap `push_event` instead: `arizona_js:on_key(enter, arizona_js:push_event(~"search"))`. Its payload is the input's value.

## Effects

The `arizona_js` builders work in two places:

- **In an attribute**, a command is part of the markup and runs when the event fires. Purely client-side commands run with no round trip. The editor's preview toggle:

  ```erlang
  {button, [{id, ~"toggle-preview"}, {az_click, arizona_js:toggle(~"#preview")}], [
      ~"Show or hide preview"
  ]},
  ```

- **Returned from a callback** (`handle_event/3`, `handle_info/2`, `handle_update/3`), commands are sent over the WebSocket and run after the patch is applied, like the `focus` and `set_title` from the save handler, or the tag input's `reset_form` after a tag is accepted.

The commonly useful ones:

| Command | Does |
|---|---|
| `push_event(Name)`, `push_event(Name, Payload)` | send an event (attributes only) |
| `toggle(Sel)`, `show(Sel)`, `hide(Sel)` | flip the `hidden` attribute |
| `add_class/2`, `remove_class/2`, `toggle_class/2` | CSS classes |
| `set_attr/3`, `remove_attr/2`, `toggle_attr/2` | attributes |
| `focus(Sel)`, `blur(Sel)`, `select(Sel)`, `scroll_to(Sel)` | first matching element |
| `reset_form(Sel)` | reset matching forms |
| `show_modal(Sel)`, `close_modal(Sel)` | open or close a `<dialog>` |
| `set_title(Title)` | document title |
| `dispatch_event(Name, Detail)` | a `CustomEvent` on `document` for your own scripts |
| `navigate(Path)`, `patch(Path)` | live navigation, see [Live Navigation](live-navigation.md) |
| `copy_to_clipboard(Sel)` | needs a user gesture, so attributes only |

Selectors match all elements for the "broadcast" commands (visibility, classes, attributes, `reset_form`) and the first element for focus, scroll and dialogs.

```admonish tip title="Render state, command one-shots"
If something on screen follows from state, put it in the template: `{class, [~"post ", ?get(status)]}`. The diff keeps it right, including on the first HTML render. Effects are for things the DOM cannot derive from state: moving focus, the title, scrolling, a transition. That is also why there is no `set_text` command: content belongs to the template.
```

### Effects that need cookies: `fetch`

A WebSocket frame cannot set a cookie. When an event must change a cookie without reloading the page, `arizona_js:fetch(Url, Opts)` sends an HTTP request to a controller route and applies the effects it answers with. The blog does not need it: logging in and out are ordinary Nova form posts that load a new page, which also gives the WebSocket the new session.

## Client-only state and hooks

Two more tools, which the blog does not use:

- `?local(Key, Init)` is a slot the server renders once and the browser owns afterwards, changed with `arizona_js:set(Key, Value)`. Good for UI-only state like an open menu.
- `az_hook` attaches a JavaScript object with `mounted`/`updated`/`destroyed` callbacks to an element, registered through the client's `hooks` export, for integrating a client-side widget.

## Testing events

Unit tests call `handle_event/3` and compare the effects with the same builders:

```erlang
?assertEqual(arizona_js:focus(~"#post-title"), Focus),
```

Over the WebSocket, an event is a JSON array `[ViewId, Event, Payload]`:

```erlang
Preview = #{~"title" => ~"Hello <b>", ~"body" => ~"First words\n\nSecond"},
ok = blog_ws_client:send_json(Client, [~"editor", ~"preview", Preview]),
#{~"o" := Ops} = blog_ws_client:recv_json(Client),
```

Client-side commands (toggle, reset on Escape, focus) only exist in a browser. The book's examples were also checked with a headless Chrome clicking through the pages.

---

Next, [Live Navigation](live-navigation.md) moves between live pages without reloading.
