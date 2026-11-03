# Live Navigation

Every live page talks to the server over one WebSocket. Moving from one live page to another does not need a new document, a new socket or a fresh download of the layout. Arizona has two kinds of in-page navigation:

- **navigate** replaces the page's view with the view of another route, mounted fresh.
- **patch** keeps the current view and hands it the new URL's bindings, for pages that are one view with several states: tabs, filters, pages of a list.

Both update the address bar and the browser history, and both fall back to a normal page load when they cannot work live.

## Navigating between pages

An `<a>` with the bare `az_navigate` attribute navigates live. The layout's header uses it for the blog's live pages:

```erlang
{nav, [], [
    {a, [{href, ~"/live/counter"}, az_navigate], [~"Counter"]},
    {a, [{href, ~"/live/editor"}, az_navigate], [~"Write"]},
    {a, [{href, ~"/live/dashboard/drafts"}, az_navigate], [~"Your posts"]}
]}
```

A click sends the path over the WebSocket. The server resolves the route, runs its middlewares, mounts the new view in the live process (a connected mount, `?connected` is `true`) and sends it back. The client swaps the old view for the new one and pushes the URL onto the history. The `href` is an ordinary link, so without JavaScript it simply loads the page.

What stays and what goes:

- **The document stays.** Scripts keep running and other sockets stay open. The notification feed from [Pub/Sub](../real-time/pubsub.md) keeps its connection while the reader moves between pages.
- **The old view goes.** Its process state, timers and pub/sub memberships end with it. Back and Forward navigate again, so the page you return to is mounted afresh: the counter is back at `0`.
- **The layout is not rendered again.** That includes `<title>`, so a page reached by navigation still has the previous page's title.

Paths that are not live routes, such as Nova's own pages, are loaded the ordinary way, so an `az_navigate` link to one does no harm. Leave `az_navigate` off links to pages outside Arizona anyway; the plain link says what will happen.

### Setting the title

To give a navigated page its title, set it from the page once it is connected. `mount/1` cannot return effects, so the page sends itself a message and returns the effect from `handle_info/2`. The dashboard does it like this:

```erlang
mount(#{current_user := #{id := UserId} = User} = Params) ->
    Section = section(maps:get(~"section", Params, ~"drafts")),
    Bindings = #{
        id => ~"dashboard",
        title => heading(Section),
        current_user => User,
        section => Section,
        posts => posts(UserId, Section)
    },
    ?connected andalso ?send(update_title),
    {Bindings, #{}}.
```

```erlang
handle_info(update_title, Bindings) ->
    {Bindings, #{}, [document_title(maps:get(section, Bindings))]}.

document_title(Section) ->
    arizona_js:set_title(<<(heading(Section))/binary, " - Nova Blog">>).
```

The `title` binding still serves the HTTP render, where the layout reads it. On a full page load the effect sets the same title again, which is harmless.

```admonish warning title="?send/1 reads a variable called Bindings"
`?send(Msg)` expands to `arizona_live:send(map_get(id, Bindings), Msg)`, and `?send_after/2` works the same way. In `mount/1` the argument usually has no `id` yet, so build the new bindings first under the name `Bindings` and send afterwards, as above. Sending before that crashes the mount with `{badkey, id}`, and the client falls back to a full page load.
```

## Navigating from the server

A handler can navigate by returning `arizona_js:navigate/1,2` as an effect. When the editor publishes a post it sends the author to the post's page:

```erlang
publish(Post, #{tags := Tags} = Bindings) ->
    {ok, Published} = blog_posts:publish(Post, Tags),
    ok = blog_notify:post_published(Published),
    Flash = #{~"info" => ~"Your post is live"},
    {Bindings#{post => Published}, #{}, [
        arizona_js:navigate(blog_live_post:path(Published), #{flash => Flash})
    ]}.
```

`navigate/2` takes options:

- `flash`, a map shown on the page navigated to.
- `replace => true`, which replaces the current history entry instead of adding one.
- `full => true`, which does a real page load.

The flash does not use a cookie: the server removes it from the effect and hands it to the next page's mount in the same process. The page reads it through Arizona's `fetch_flash` middleware, which puts a `flash` binding (a map, `#{}` when empty) into the bindings:

```erlang
{live, ~"/live/posts/:id", blog_live_post, #{
    layouts => [{blog_live_layout, render}],
    middlewares => [
        {blog_live_middleware, path_bindings},
        {blog_live_middleware, current_user},
        {blog_live_middleware, published_post},
        {arizona_middleware, fetch_flash}
    ]
}},
```

and shows it with a stateless component, `flash/1` in `blog_live_components` (exported next to the others):

```erlang
flash(Bindings) ->
    case maps:get(~"info", ?get(flash), undefined) of
        undefined -> ?html({p, [{class, ~"flash"}, hidden], []});
        Message -> ?html({p, [{class, ~"flash"}, {role, ~"status"}], [Message]})
    end.
```

A flash set by a live handler cannot survive a *full* page load, because a WebSocket frame cannot set a cookie. Arizona drops it and logs a warning in that case.

## Middlewares run on every navigation

A navigation is a request for a page, so the target route's middlewares run exactly as they do over HTTP. The dashboard is behind `require_user/2`. When a logged-out reader clicks "Your posts", the redirect to `/login` becomes a navigate effect that replaces the history entry. `/login` is a Nova page, so it is loaded normally. A middleware that halts without a redirect, like `published_post/2` with its `404`, makes the client load the path normally, so the browser gets the real error response.

The socket's requests are built from the cookies the browser sent when the WebSocket connected. That is why logging in and out are normal page loads (see [Live Views](live-views.md#who-is-logged-in)): they give the next socket the new session cookie.

## Patching: one view, several URLs

The dashboard lists the reader's posts in two tabs, drafts and published. The route captures the tab:

```erlang
{live, ~"/live/dashboard/:section", blog_live_dashboard, #{
    layouts => [{blog_live_layout, render}],
    middlewares => [
        {blog_live_middleware, require_user},
        {blog_live_middleware, path_bindings}
    ]
}},
```

The tabs are links with the bare `az_patch` attribute:

```erlang
render(Bindings) ->
    Section = ?get(section),
    ?html(
        {main, [{id, ?get(id)}, {class, ~"dashboard"}], [
            {h1, [], [heading(Section)]},
            {nav, [{'aria-label', ~"Post status"}], [
                {a,
                    [
                        {href, ~"/live/dashboard/drafts"},
                        az_patch,
                        {'aria-current', current(Section, draft)}
                    ],
                    [~"Drafts"]},
                {a,
                    [
                        {href, ~"/live/dashboard/published"},
                        az_patch,
                        {'aria-current', current(Section, published)}
                    ],
                    [~"Published"]}
            ]},
            {ul, [{id, ~"post-list"}], [
                ?each(
                    fun(#{id := Id, title := Title}) ->
                        {li, [], [{a, [{href, post_href(Section, Id)}, az_navigate], [Title]}]}
                    end,
                    ?get(posts)
                )
            ]}
        ]}
    ).
```

A patch goes to a URL whose route has the *same* handler as the current page. The server runs the route's middlewares, as for a navigate, but instead of mounting a new view it calls the current view's `handle_update/3` with the resulting bindings:

```erlang
handle_update(Params, #{current_user := #{id := UserId}} = Bindings, Effects) ->
    Section = section(maps:get(~"section", Params, ~"drafts")),
    Updated = Bindings#{section => Section, posts => posts(UserId, Section)},
    {Updated, #{}, [document_title(Section) | Effects]}.
```

`Params` holds what the middlewares produced, here `current_user` and the `~"section"` path binding. The view keeps its state and its DOM, the diff updates the heading, the tab marker and the list, and the title effect rides along with the patch. `handle_update/3` must return the `Effects` it was given, plus its own. Back and Forward between two patched URLs call `handle_update/3` again.

The section comes from the URL, so it is untrusted input. `section/1` maps it to one of two atoms with fixed clauses, and anything else counts as drafts:

```erlang
section(~"published") -> published;
section(_Other) -> draft.
```

The query uses that atom and the session's user id, never the raw path segment:

```erlang
posts(UserId, Status) ->
    Mine = kura_query:where(kura_query:from(blog_post), {user_id, UserId}),
    Query = kura_query:where(Mine, {status, atom_to_binary(Status)}),
    {ok, Posts} = blog_repo:all(kura_query:order_by(Query, [{updated_at, desc}, {id, desc}])),
    Posts.
```

From a handler, `arizona_js:patch(Path)` patches the same way. A patch to a route with a different handler cannot keep the view, so it becomes a navigate.

| Use | When |
|---|---|
| `az_patch`, `arizona_js:patch/1` | Same page, different state in the URL: tabs, filters, sort order, pagination |
| `az_navigate`, `arizona_js:navigate/1,2` | A different live page |
| a plain link | A page Arizona does not serve |

Put state the reader might bookmark or share in the URL and patch to it. State that is only on screen for a moment, like an open menu, belongs in bindings.

## Testing navigation

`handle_update/3` is a function like the others, so the patch is tested by calling it:

```erlang
dashboard_lists_posts_by_section(Config) ->
    Alice = ?config(alice, Config),
    Draft = blog_data_fixtures:post(Alice, #{~"title" => ~"Draft one"}),
    _ = blog_data_fixtures:post(?config(bob, Config), #{~"title" => ~"Not mine"}),
    Mount = #{current_user => actor(Alice), ~"section" => ~"published"},
    {Published, #{}} = blog_live_dashboard:mount(Mount),
    ?assertEqual([~"Hello live"], titles(Published)),
    {Drafts, #{}, [Title]} =
        blog_live_dashboard:handle_update(#{~"section" => ~"drafts"}, Published, []),
    ?assertEqual([maps:get(title, Draft)], titles(Drafts)),
    ?assertEqual(arizona_js:set_title(~"Your drafts - Nova Blog"), Title),
    ?assertEqual(draft, blog_live_dashboard:section(~"anything-else")).
```

Whether the document survives a navigation, and the view survives a patch, can only be seen in a browser. The headless-browser check stores a JavaScript property on `window` and on the `#dashboard` element, clicks the Published tab, and finds both still there. After an `az_navigate` click the `window` property is still there and the view element is a new one.

---

Next, Part VII goes below Arizona to Nova's own [WebSockets](../real-time/websockets.md).
