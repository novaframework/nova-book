# Live Navigation

Traditional page navigation triggers a full HTTP request-response cycle. Arizona supports **live navigation** — moving between live views over the existing WebSocket connection, preserving the connection state and avoiding full page reloads.

## Live redirects

A live redirect navigates to a new live view, replacing the current one:

```erlang
handle_event(<<"go_to_post">>, #{<<"id">> := Id}, View) ->
    {[{redirect, "/posts/" ++ binary_to_list(Id)}], View}.
```

In templates, use `az-live-redirect`:

```html
<a href="/posts/42" az-live-redirect>View Post</a>
```

When the user clicks this link:
1. The browser updates the URL (pushState)
2. The WebSocket sends a navigation event
3. The server mounts the new live view
4. Arizona sends the new HTML over the WebSocket
5. The client patches the DOM

No HTTP request. No page flash.

## Live patches

A live patch updates the URL and re-triggers `mount/2` on the **same** live view. Useful for filtering, pagination, and search:

```html
<a href="/posts?page=2" az-live-patch>Page 2</a>
<a href="/posts?status=published" az-live-patch>Published</a>
```

```erlang
mount(Params, _Req) ->
    Page = binary_to_integer(maps:get(<<"page">>, Params, <<"1">>)),
    Status = maps:get(<<"status">>, Params, <<"all">>),
    Q = build_query(Status, Page),
    {ok, Posts} = blog_repo:all(Q),
    arizona_view:new(?MODULE, #{
        id => ~"post_list",
        posts => Posts,
        page => Page,
        status => Status
    }, none).
```

The same live view, different URL, different state. The WebSocket connection stays alive.

## URL-driven state

Live patches make the URL the source of truth for view state. This means:
- Back/forward buttons work
- URLs are shareable and bookmarkable
- Browser history is preserved

```erlang
handle_event(<<"filter">>, #{<<"status">> := Status}, View) ->
    Path = "/posts?status=" ++ binary_to_list(Status),
    {[{patch, Path}], View}.
```

The `{patch, Path}` action updates the URL and re-mounts with the new params.

## Regular navigation

For navigating to non-live-view pages (traditional Nova controllers), use regular links:

```html
<a href="/about">About</a>
```

This triggers a normal HTTP navigation — a full page load. Use live navigation only between live views.

---

With Arizona covered, let's look at the underlying [WebSocket](../real-time/websockets.md) infrastructure that powers both raw WebSocket handlers and Arizona's live connections.
