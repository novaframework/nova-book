# Components

The editor from the [previous chapter](live-views.md) is already made of pieces: an error message under each field, a word count, a tag input. Arizona has two kinds of component for that:

- **Stateless** components are functions from props to a template. They have no state and no events of their own.
- **Stateful** components implement `arizona_stateful`, exactly like a page, and are embedded in a parent's template. They keep their own bindings and handle their own events, inside the parent's live process.

## Stateless components

A stateless component is any exported 1-arity function returning `?html(...)`, in a module that includes `arizona_stateless.hrl`. Its argument is the props map, conventionally named `Bindings` so `?get` works. The blog keeps its small pieces in `src/live/blog_live_components.erl`:

```erlang
-module(blog_live_components).
-include_lib("arizona/include/arizona_stateless.hrl").
-export([tag_badge/1, field_error/1, word_stats/1]).

tag_badge(Bindings) ->
    ?html({span, [{class, ~"tag"}], [~"#", ?get(tag)]}).

field_error(Bindings) ->
    case ?get(error) of
        undefined -> ?html({span, [{class, ~"field-error"}, hidden], []});
        Message -> ?html({span, [{class, ~"field-error"}, {role, ~"alert"}], [Message]})
    end.

word_stats(Bindings) ->
    ?html(
        {p, [{class, ~"word-stats"}], [
            ?get(words), ~" words, about ", ?get(minutes), ~" min read"
        ]}
    ).
```

[Live Navigation](live-navigation.md#navigating-from-the-server) adds a fourth, `flash/1`, to the same module.

Embed one with `?stateless(Module, Function, Props)`:

```erlang
?stateless(blog_live_components, field_error, #{
    error => maps:get(title, ?get(errors), undefined)
}),
```

The props map is evaluated in the parent, so the parent's slot depends on `errors`; when `errors` changes, the component re-renders with its new props, and when it does not, nothing is sent. `field_error/1` returns one of two templates depending on its props. That is allowed: the diff replaces the element when the shape changes. A helper in the same module can be embedded with `?stateless(fun my_helper/1, Props)`.

Stateless components are also the only kind a layout can contain, because a layout is rendered once and never diffed.

## Stateful components

The tag input needs state of its own: the tags added so far and the last validation error. It handles its own events, and tells the editor when the list changes. `src/live/blog_live_tag_input.erl`:

```erlang
-module(blog_live_tag_input).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).
-export([normalize/1]).

-define(MAX_TAGS, 5).

mount(Props) ->
    {
        #{
            id => maps:get(id, Props),
            notify => maps:get(notify, Props),
            tags => maps:get(tags, Props, []),
            error => undefined
        },
        #{}
    }.
```

`mount/1` receives the props the parent passed. Like a page, the component must keep an `id` and put it on its root element.

```erlang
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}, {class, ~"tag-input"}], [
            {ul, [{class, ~"tags"}], [
                ?each(
                    fun(Tag) ->
                        {li, [], [
                            ?stateless(blog_live_components, tag_badge, #{tag => Tag}),
                            {button,
                                [
                                    {type, ~"button"},
                                    {'aria-label', [~"Remove ", Tag]},
                                    {az_click,
                                        arizona_js:push_event(~"remove_tag", #{~"tag" => Tag})}
                                ],
                                [~"x"]}
                        ]}
                    end,
                    ?get(tags)
                )
            ]},
            {form, [{id, ~"tag-form"}, {az_submit, arizona_js:push_event(~"add_tag")}], [
                {input,
                    [
                        {id, ~"tag-name"},
                        {name, ~"tag"},
                        {placeholder, ~"Add a tag and press Enter"},
                        {autocomplete, ~"off"},
                        {az_keydown, arizona_js:on_key(escape, arizona_js:reset_form(~"#tag-form"))}
                    ],
                    []}
            ]},
            ?stateless(blog_live_components, field_error, #{error => ?get(error)})
        ]}
    ).
```

The tag field is its own little form: pressing Enter in a text input submits its form, so `az_submit` gives us "add on Enter" for free. Escape clears the field without a round trip (more on key events in the [next chapter](events.md)).

An event goes to the closest view around the element that fired it, so clicks and submits inside the tag input reach `blog_live_tag_input:handle_event/3`, not the editor's:

```erlang
handle_event(~"add_tag", #{~"tag" := Input}, Bindings) ->
    #{tags := Tags} = Bindings,
    case normalize(Input) of
        {ok, _Tag} when length(Tags) >= ?MAX_TAGS ->
            {Bindings#{error => ~"A post can have at most 5 tags"}, #{}, []};
        {ok, Tag} ->
            NewTags = lists:uniq(Tags ++ [Tag]),
            notify(Bindings, NewTags),
            {Bindings#{tags => NewTags, error => undefined}, #{}, [
                arizona_js:reset_form(~"#tag-form")
            ]};
        {error, Message} ->
            {Bindings#{error => Message}, #{}, []}
    end;
handle_event(~"remove_tag", #{~"tag" := Tag}, Bindings) ->
    NewTags = lists:delete(Tag, maps:get(tags, Bindings)),
    notify(Bindings, NewTags),
    {Bindings#{tags => NewTags, error => undefined}, #{}, []}.

normalize(Input) ->
    Tag = string:lowercase(string:trim(Input)),
    case re:run(Tag, ~"^[a-z0-9-]{1,24}\\z") of
        {match, _} -> {ok, Tag};
        nomatch -> {error, ~"Tags are 1-24 characters: a-z, 0-9 and -"}
    end.

notify(#{notify := ParentId}, Tags) ->
    arizona_live:send(ParentId, {tags_changed, Tags}).
```

An accepted tag returns a `reset_form` effect, which empties the field for the next tag. A rejected one returns no effect, so the field keeps what was typed and the author can fix it.

Tags become part of URLs and database rows later, so the component normalises them and refuses anything outside a small alphabet.

### Embedding it

The editor embeds the component with `?stateful(Module, Props)`:

```erlang
?stateful(blog_live_tag_input, #{id => ~"tags", notify => ?get(id), tags => ?get(tags)}),
```

`id` is required, must be unique on the page, and must stay stable: it is how the diff engine routes patches and events to this instance. Change the id and the component is unmounted and mounted again.

### Talking to the parent

Parent and child run in the same live process. A child talks to its parent by sending a message to the parent's view id; `arizona_live:send/2` routes it to that view's `handle_info/2`. The editor passes its own id as the `notify` prop, and handles the message:

```erlang
handle_info({tags_changed, Tags}, Bindings) ->
    {Bindings#{tags => Tags}, #{}, []};
```

Passing the id in, instead of hard-coding `~"editor"` in the component, keeps the component reusable.

### Props from the parent: `handle_update/3`

When the parent re-renders the `?stateful` slot (here, because `tags` changed), the child receives the new props. Without a `handle_update/3` callback they are merged into the child's bindings, which is what the tag input wants. Export `handle_update(Props, Bindings, Effects)` to transform them instead, for example to keep a local draft when the parent sends a stale value. It must return the `Effects` accumulator it was given, extended with its own effects.

## Choosing between them

| | Stateless | Stateful |
|---|---|---|
| Is | a function `Props -> template` | a module with `mount/render/handle_event/...` |
| State | none, re-rendered from props | its own bindings |
| Events | handled by the enclosing view | handled by itself |
| Needs an `id` | no | yes, unique and stable |
| Allowed in a layout | yes | no |

Start stateless. Reach for a stateful component when a piece of UI has state the parent should not have to manage, like the half-typed tag and its validation error here.

## Lists

`?each(Fun, List)` renders a list. For a short list that is rebuilt whole, like the tags or the preview paragraphs, that is all you need. For long or growing collections, such as a comment thread, Arizona has *streams*: keyed collections where an insert or delete sends only the affected item. The [live comment section](../real-time/live-feature.md) uses one.

## Testing components

Stateful components are tested like pages. The test process plays the parent, so the `notify` message lands in its own mailbox:

```erlang
tag_input_normalizes_and_notifies_parent(_Config) ->
    {Bindings, #{}} = blog_live_tag_input:mount(#{id => ~"tags", notify => ~"editor"}),
    {Added, #{}, [Reset]} = blog_live_tag_input:handle_event(
        ~"add_tag", #{~"tag" => ~" Erlang "}, Bindings
    ),
    ?assertEqual([~"erlang"], maps:get(tags, Added)),
    ?assertEqual(arizona_js:reset_form(~"#tag-form"), Reset),
    receive
        {arizona_view, ~"editor", {tags_changed, [~"erlang"]}} -> ok
    after 1000 -> ct:fail(parent_not_notified)
    end,
    ...
```

---

Next, [Events & Interactivity](events.md) looks at payloads, forms, keys and client-side effects.
