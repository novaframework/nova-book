# Live Views

A live view is a module implementing the `arizona_stateful` behaviour and mounted by a `live` route. The counter showed the shape. In this chapter we build something the blog needs: a post editor that previews the post as the author types, validates it, and only lets logged-in authors in.

## The lifecycle

| Callback | When it runs | Returns |
|---|---|---|
| `mount(Bindings)` | Once for the HTTP render, and again when the WebSocket connects | `{Bindings, Resets}` |
| `render(Bindings)` | After every mount and every change | a template |
| `handle_event(Event, Payload, Bindings)` | A client event (`az_click`, `az_submit`, ...) | `{Bindings, Resets, Effects}` |
| `handle_info(Message, Bindings)` | A message in the live process's mailbox | `{Bindings, Resets, Effects}` |
| `handle_update(Props, Bindings, Effects)` | New props from a parent, or a `patch` navigation | `{Bindings, Resets, Effects}` |
| `unmount(Bindings)` | The view is removed or the process exits | anything |

Only `mount/1` and `render/1` are required.

`mount/1` runs **twice** for a page: first inside the HTTP request that produces the initial HTML, then inside the live process when the browser connects. The two runs do not share state. The macro `?connected` tells them apart: it is `false` during the HTTP render and `true` in the live process. Guard anything that only makes sense with a connected client, such as subscribing to messages or starting timers:

```erlang
mount(Bindings) ->
    ?connected andalso ?subscribe(some_topic),
    {Bindings, #{}}.
```

`mount/1` has no effects slot because the HTTP render has no client to run them on. For a "run this once connected" effect, send yourself a message with `?connected andalso ?send(Msg)` and return the effect from `handle_info/2`. The [Live Navigation](live-navigation.md) chapter uses that to set the page title.

## Where bindings come from

`mount/1` receives the route's initial bindings. They come from three places, merged in this order:

1. The route's `bindings` option: `{live, Path, Handler, #{bindings => #{section => ~"drafts"}}}`.
2. Route `middlewares`, which turn the request into bindings (below).
3. `on_mount` hooks, a list of `{Module, Function}` taking and returning the bindings map. They also run before every navigation to the route.

Whatever `mount/1` returns *is* the view's state. Nothing is merged back in, so copy across what you need and drop the rest.

## Who is logged in?

As the [previous chapter](fundamentals.md) warned, Arizona answers its routes before Nova's router, so Nova security functions do not protect live pages. Arizona's equivalent is the route's `middlewares` list. A middleware is a 2-arity function taking the request and the bindings gathered so far, returning `{cont, Request, Bindings}` to carry on or `{halt, Request}` to stop.

Middlewares run on the HTTP render **and** on the WebSocket upgrade (and on every live navigation), so a page and its socket are checked the same way. The upgrade is a request from the same browser, so it carries the same cookies, including Nova's `session_id`.

The blog keeps its logged-in user in Nova's session (the [Authentication](../auth-sessions/authentication.md) chapter writes it there with `nova_auth`, which stores an "actor" map with at least an `id`). `arizona_req:raw/1` returns the underlying Cowboy request, which is exactly what `nova_session` and `nova_auth_actor` read the cookie from. Create `src/live/blog_live_middleware.erl`:

```erlang
-module(blog_live_middleware).
-export([path_bindings/2, current_user/2, require_user/2]).

path_bindings(Req, Bindings) ->
    Extract = arizona_middleware:extract([path_bindings]),
    Extract(Req, Bindings).

current_user(Req, Bindings) ->
    {cont, Req, Bindings#{current_user => session_user(Req)}}.

require_user(Req, Bindings) ->
    case session_user(Req) of
        undefined ->
            {halt, arizona_req:redirect(Req, ~"/login")};
        User ->
            {cont, Req, Bindings#{current_user => User}}
    end.

session_user(Req) ->
    case nova_auth_actor:fetch(arizona_req:raw(Req)) of
        {ok, User} -> User;
        {error, not_found} -> undefined
    end.
```

- `current_user/2` adds a `current_user` binding, `undefined` for anonymous readers.
- `require_user/2` redirects anonymous visitors to `/login` and halts. On the WebSocket upgrade a halt refuses the connection.
- `path_bindings/2` is explained below.
- [Building a Live Feature](../real-time/live-feature.md) adds two more, `published_post/2` and `own_post/2`, which load posts.

Name the step as a `{Module, Function}` pair on the route. Here is the editor's route in the `arizona_nova` config:

```erlang
{live, ~"/live/editor", blog_live_editor, #{
    layouts => [{blog_live_layout, render}],
    middlewares => [{blog_live_middleware, require_user}]
}},
```

```shell
curl -i localhost:8080/live/editor
```

```
HTTP/1.1 302 Found
...
location: /login
```

The tests seed a Nova session by hand and send its cookie, which is how you can try the editor before the authentication chapter exists:

```erlang
login(User) ->
    {ok, SessionId} = nova_session:generate_session_id(),
    ok = nova_session_ets:set_value(SessionId, nova_auth_actor:session_key(), term_to_binary(User)),
    SessionId.
```

```erlang
editor_websocket_requires_login(_Config) ->
    ?assertEqual({error, 302}, blog_ws_client:connect("/live/ws?_az_path=/live/editor")).
```

```admonish note title="Why path_bindings needs a wrapper"
Arizona's built-in `arizona_middleware:extract([path_bindings, params, ...])` copies request data into bindings. But `extract/1` is a *builder*: it returns the middleware fun, and a sys.config file can only hold terms, not calls. `arizona_nova` reads its routes from config, so wrap any parameterised step in a named 2-arity function like `path_bindings/2` and reference it as `{blog_live_middleware, path_bindings}`. Writing `{arizona_middleware, extract}` compiles but fails with `undef` at request time. Path bindings arrive with binary keys: `/live/posts/:id` gives `~"id"`.
```

```admonish note title="Logging in and out"
A WebSocket is not re-authenticated when cookies change. Logging in or out happens in Nova controllers with a normal POST and redirect, so the browser loads a fresh page and opens a fresh socket. The user lookup above goes to the server-side session on every mount and navigation, so a session deleted at logout is noticed on the next one. Arizona also has its own encrypted cookie session (`arizona_middleware:fetch_session`), but the blog keeps one session, Nova's.
```

## The post editor

The editor lives at `/live/editor`. It has a form for the title and body, a preview that updates as you type, live validation, and a tag input (a component, built in the [next chapter](components.md)). Saving is covered in [Events & Interactivity](events.md). Here is `src/live/blog_live_editor.erl`, starting with `mount/1`:

```erlang
-module(blog_live_editor).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).
-export([validate/2, paragraphs/1, word_count/1]).

-define(MAX_TITLE, 120).
-define(STATUS_TIMEOUT, 3000).

mount(Bindings) ->
    Post = maps:get(post, Bindings, undefined),
    Title = field(title, Post),
    Body = field(body, Post),
    {
        #{
            id => ~"editor",
            title => ~"Write a post",
            current_user => maps:get(current_user, Bindings),
            post => Post,
            initial_title => Title,
            initial_body => Body,
            post_title => Title,
            body => Body,
            tags => tag_names(Post),
            errors => #{},
            status => ~""
        },
        #{}
    }.

field(_Field, undefined) -> ~"";
field(Field, Post) -> maps:get(Field, Post).

tag_names(#{tags := Tags}) -> [Name || #{name := Name} <- Tags];
tag_names(_Post) -> [].
```

`current_user` comes from `require_user/2`. `title` is read by the layout for the `<title>` element, so the post's own title is `post_title`. `post` stays `undefined` on `/live/editor`; [Building a Live Feature](../real-time/live-feature.md) adds a `/live/editor/:id` route whose middleware loads an existing post, and saving it to the database.

### Rendering

```erlang
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}, {class, ~"editor"}], [
            {h1, [], [~"Write a post"]},
            {form,
                [
                    {id, ~"post-form"},
                    {az_input, arizona_js:push_event(~"preview")},
                    {az_debounce, ~"150"},
                    {az_submit, arizona_js:push_event(~"save")}
                ],
                [
                    {label, [{for, ~"post-title"}], [~"Title"]},
                    {input,
                        [
                            {id, ~"post-title"},
                            {name, ~"title"},
                            {value, ?get(initial_title)},
                            {autocomplete, ~"off"}
                        ],
                        []},
                    ?stateless(blog_live_components, field_error, #{
                        error => maps:get(title, ?get(errors), undefined)
                    }),
                    {label, [{for, ~"post-body"}], [~"Body"]},
                    {textarea, [{id, ~"post-body"}, {name, ~"body"}, {rows, ~"10"}], [
                        ?get(initial_body)
                    ]},
                    ?stateless(blog_live_components, field_error, #{
                        error => maps:get(body, ?get(errors), undefined)
                    }),
                    {button, [{type, ~"submit"}, {name, ~"intent"}, {value, ~"save"}], [
                        ~"Save draft"
                    ]},
                    {button, [{type, ~"submit"}, {name, ~"intent"}, {value, ~"publish"}], [
                        ~"Publish"
                    ]}
                ]},
            ?stateful(blog_live_tag_input, #{id => ~"tags", notify => ?get(id), tags => ?get(tags)}),
            {p, [{id, ~"save-status"}, {role, ~"status"}], [?get(status)]},
            {button, [{id, ~"toggle-preview"}, {az_click, arizona_js:toggle(~"#preview")}], [
                ~"Show or hide preview"
            ]},
            {article, [{id, ~"preview"}], [
                {h2, [], [?get(post_title)]},
                ?each(fun(Paragraph) -> {p, [], [Paragraph]} end, paragraphs(?get(body))),
                ?stateless(blog_live_components, word_stats, #{
                    words => word_count(?get(body)),
                    minutes => reading_minutes(word_count(?get(body)))
                })
            ]}
        ]}
    ).
```

A few things to notice:

- The form's `az_input` fires on every keystroke in any field inside it. Because the attribute sits on the `<form>`, the event's payload is the whole form: `#{~"title" => ..., ~"body" => ...}`. `az_debounce` waits until typing pauses for 150 ms before sending.
- The fields start from `initial_title` and `initial_body`, which are set once in `mount/1` and never change, so the diff never touches the fields again. After that the browser owns what is being typed and the server only reads it from events. Binding a field to `post_title`, which changes on every keystroke, would have the server rewriting the field under the cursor.
- `?each(Fun, List)` renders one element per list item. `paragraphs/1` and `word_count/1` are plain functions of `?get(body)`, and that is fine: the slot depends on `body`, so it is recomputed when `body` changes.
- `?stateless` and `?stateful` embed components, covered next chapter.
- The two submit buttons share the form. [Events & Interactivity](events.md) shows how the handler tells them apart.

`render/1` must return the `?html(...)` directly. The parse transform adds the view marker and checks the root id only when the template is the body of `render/1`, so do not delegate to a helper (`render(B) -> page(B).`). Helpers that return smaller templates for parts of the page are fine.

### Handling the preview event

```erlang
handle_event(~"preview", #{~"title" := Title, ~"body" := Body}, Bindings) ->
    Errors = preview_errors(Title, Body),
    {Bindings#{post_title => Title, body => Body, errors => Errors}, #{}, []};
```

with the helpers:

```erlang
validate(Title, Body) ->
    TitleErrors =
        case string:trim(Title) of
            ~"" ->
                #{title => ~"A title is required"};
            Trimmed when byte_size(Trimmed) > ?MAX_TITLE ->
                #{title => ~"Keep the title under 120 characters"};
            _ ->
                #{}
        end,
    case string:trim(Body) of
        ~"" -> TitleErrors#{body => ~"Write something first"};
        _ -> TitleErrors
    end.

preview_errors(~"", _Body) -> #{};
preview_errors(Title, Body) -> maps:with([title], validate(Title, Body)).

paragraphs(Body) ->
    [string:trim(P) || P <- binary:split(Body, [~"\r\n\r\n", ~"\n\n"], [global, trim_all])].

word_count(Body) ->
    length(string:lexemes(Body, [$\s, $\n, $\t, [$\r, $\n]])).

reading_minutes(Words) ->
    max(1, (Words + 199) div 200).
```

While typing, only "title too long" is shown: nagging about empty fields before the author has finished is unfriendly, so "required" errors wait for the save.

Payload keys are binaries, straight from the browser. Match the ones you need and treat the values as untrusted input, as you would a form POST. Rendering them is safe because Arizona escapes text; storing them goes through a changeset, as in [Changesets & Validation](../data-layer/changesets.md).

What goes over the wire for one preview event is just the changed slots. From the WebSocket test, sending `["editor","preview",{"title":"Hello <b>","body":"First words\n\nSecond"}]`:

```erlang
[[0,<<"editor:DI7P3U-6">>,<<"Hello <b>">>],
 [10,<<"editor:DI7P3U-5">>,
  [[5,0,
    #{<<"d">> => [<<"First words">>],
      <<"f">> => <<"1CBUKXR">>,
      <<"s">> =>
          [<<"<p az=\"1CBUKXR-0\"><!--az:1CBUKXR-0-->">>,
           <<"<!--/az--></p>">>]}],
   [5,0,#{<<"d">> => [<<"Second">>],<<"f">> => <<"1CBUKXR">>}]]],
 [0,<<"editor:DI7P3U-5-1-1G5REGA-0">>,<<"3">>]]
```

The title is a text op (`0`). The browser sets it as text, so `<b>` shows up literally. The paragraphs are a list patch (`10`) with two inserts (`5`); the second reuses the statics of the first by fingerprint (`f`). The word count is another text op. The form, the labels and the tag input are not touched.

## Mailbox messages: `handle_info/2`

The live view is a process, so anything can send it a message, and `handle_info/2` handles it like an event. Arizona has helpers for messages to views:

- `?send(Msg)` sends `Msg` to the current view; `arizona_live:send(ViewId, Msg)` to another view in the same live process.
- `?send_after(Time, Msg)` does the same after `Time` milliseconds and returns the timer reference. Arizona cancels a view's pending timers when the view goes away or the page navigates.

After a successful save the editor shows "Draft saved" and clears it three seconds later:

```erlang
handle_info({tags_changed, Tags}, Bindings) ->
    {Bindings#{tags => Tags}, #{}, []};
handle_info(clear_status, Bindings) ->
    {Bindings#{status => ~""}, #{}, []}.
```

The save handler arms the timer with `?send_after(?STATUS_TIMEOUT, clear_status)`. The `tags_changed` clause is a message from the tag input component, which we will meet in the [next chapter](components.md).

A message that arrives before the view has mounted is dropped, so a process that needs to reach a view should do it through pub/sub (subscribe in `mount/1`, guarded by `?connected`). That is how the [live comment section](../real-time/live-feature.md) hears about new comments.

## Resets and effects

The second element of each return tuple, `Resets`, is a map merged into the bindings *after* the render. The new value is stored but not rendered, so it suits bookkeeping (a "this event was handled" flag) rather than anything on screen. The blog always returns `#{}`.

The third element, `Effects`, is a list of `arizona_js` commands the browser runs after applying the patch: focus a field, set the document title, scroll, dispatch a DOM event. [Events & Interactivity](events.md) covers them.

## When a view crashes

A crash in a callback takes down the live process, not the node. The socket closes with code 4500 and the client reconnects with backoff and mounts the view afresh, restoring what was typed in form fields. State held only in the view's bindings is gone, which is the usual Erlang trade: keep anything that matters in the database or in a supervised process, and let the view rebuild from it in `mount/1`.

`unmount/1` runs when a view goes away for any reason. Pub/sub memberships and monitors die with the process, so you rarely need it.

## Testing

Each callback is a plain function, so most of a view's behaviour is tested by calling it:

```erlang
editor_save_reports_errors_and_focuses(_Config) ->
    {Bindings, #{}} = blog_live_editor:mount(#{current_user => #{id => 1}}),
    Payload = #{~"title" => ~"", ~"body" => ~"Text"},
    {Invalid, #{}, [Focus]} = blog_live_editor:handle_event(~"save", Payload, Bindings),
    ?assertEqual(#{title => ~"A title is required"}, maps:get(errors, Invalid)),
    ?assertEqual(arizona_js:focus(~"#post-title"), Focus),
    ...
```

The suite also renders the page over HTTP with a session cookie and sends a preview event over a real WebSocket. See `test/blog_realtime_arizona_SUITE.erl`.

---

Next, [Components](components.md) splits the editor into reusable pieces.
