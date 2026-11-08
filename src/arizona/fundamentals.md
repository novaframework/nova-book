# Arizona Fundamentals

So far the blog renders HTML on the server and sends complete pages to the browser. That covers most of a blog, but some parts want to react while the reader is on the page: a post editor that previews as you type, a comment section that shows new comments without a reload, a feed of what just happened. [Arizona](https://github.com/arizona-framework/arizona) is a real-time web framework for Erlang that does this the server-rendered way: the state lives in an Erlang process, the server renders HTML, and the browser receives small patches over a WebSocket.

This part of the book adds Arizona to the blog. We start with a counter, the "hello world" of live views, and use it to see how the pieces fit. The following chapters build the live post editor, components, events and navigation.

```admonish warning title="Arizona needs OTP 29"
This is why the book asks for OTP 29 from the start: Nova, Kura and Shigoto run on OTP 28, but Arizona 0.4, `arizona_cowboy` and `arizona_nova` all declare `minimum_otp_vsn` 29. Check with `erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'` before you add them.
```

## How Arizona works

1. The browser requests a page. Arizona mounts the page's handler, renders it to HTML and sends a normal HTTP response. The page works and is indexable before any JavaScript runs.
2. A small client script opens a WebSocket. The server starts one `arizona_live` process (a `gen_server`) for the connection and mounts the handler again, this time "connected".
3. A click or form submit becomes an event frame. The live process calls the handler's `handle_event/3`, which returns new bindings.
4. Arizona re-renders only the parts of the template that read changed bindings and sends those values to the browser, which patches the DOM.

Templates are Erlang terms, not strings. A parse transform splits each template at compile time into the parts that never change (sent once) and the dynamic parts, and records which bindings each dynamic part reads. When `count` changes, only the slot that reads `count` is recomputed and sent.

## Adding Arizona to the blog

Arizona itself ships no HTTP server. An adapter package provides one, and `arizona_nova` is the adapter that runs Arizona *inside* a Nova application, on Nova's own Cowboy listener. Add it to `rebar.config`:

```erlang
{deps, [
    {nova, "~> 0.18"},
    ...
    {arizona_nova, "~> 0.1"}
]}.
```

`arizona_nova` 0.1.0 depends on `arizona` 0.4 and `arizona_cowboy` 0.1, so those two come along. Then list `arizona_nova` in `src/blog.app.src` so it starts before the blog:

```erlang
{applications, [
    kernel,
    stdlib,
    nova,
    kura,
    kura_postgres,
    nova_auth,
    arizona_nova
]},
```

### Sharing Nova's listener

Nova owns the listener and its middleware chain: router, plugins, security, controller. Arizona needs to see each request first and answer the ones that match an Arizona route. That is what `arizona_cowboy_middleware` does: a request for an Arizona path is served by Arizona, and everything else continues down Nova's chain untouched.

Put it in front of Nova's chain with the `middleware_handlers` key of `cowboy_configuration`. In `config/dev_sys.config.src`:

```erlang
{cowboy_configuration, #{
    port => 8080,
    stream_handlers => [nova_stream_h, blog_session_h, cowboy_compress_h, cowboy_stream_h],
    middleware_handlers => [
        arizona_cowboy_middleware,
        nova_router,
        nova_plugin_handler,
        nova_security_handler,
        nova_handler,
        nova_plugin_handler
    ]
}},
```

The last five entries are Nova's default chain. `stream_handlers` is the list from [Sessions](../auth-sessions/sessions.md#hardening-the-cookie); keep it, or the session cookie loses its flags.

```admonish warning title="Spell the list out in sys.config"
The `arizona_nova` README suggests `middleware_handlers => arizona_nova:middlewares()`. That is a function call, and a sys.config file is read with `file:consult/1`, which only accepts terms: the node refuses to boot with `bad term`. `arizona_nova:middlewares()` returns exactly the list above, so write the list out.
```

### Declaring Arizona routes

Arizona routes are not part of `blog_router.erl`. They live in the `arizona_nova` application environment, next to the Nova config:

```erlang
{arizona_nova, [
    {routes, [
        {live, ~"/live/counter", blog_live_counter, #{layouts => [{blog_live_layout, render}]}},
        {ws, ~"/live/ws", #{}},
        {asset, ~"/assets/arizona", {priv_dir, arizona, "static/assets/js"}}
    ]}
]},
```

Three kinds of route:

- `{live, Path, Handler, Opts}` - a live page. `Handler` is the module that mounts and renders it, `layouts` wraps it in an HTML shell.
- `{ws, Path, Opts}` - the WebSocket every live page connects to. One is enough for the whole app.
- `{asset, Path, Source}` - static files. Arizona's client JavaScript ships inside the `arizona` package (`priv/static/assets/js`), so the blog serves it from its own origin. Nothing is loaded from a CDN.

Paths are binaries. A path an Arizona route claims is answered by Arizona, including its 404s, so give Arizona its own prefixes. The blog uses `/live/...` for pages and `/assets/arizona` for the client, which leaves Nova's `/assets/[...]` static route serving `favicon.ico` as before. `rebar3 nova routes` lists only Nova's routes, so keep the Arizona list in mind when you add Nova routes.

When `arizona_nova` starts it mounts these routes on Nova's listener (`nova_listener`, the bootstrap application's). When it stops, before Nova, it drains the live sessions so their browsers reconnect to the next node.

The same `arizona_nova` block goes in `config/test_sys.config` and the production config. A route list that grows is the price of keeping routes in config; by the end of this part the blog's has seven entries (eight in development, with the MCP route below).

## The layout

A layout is the HTML document around a page. It is a *stateless* module: a `render/1` that returns a template. Create `src/live/blog_live_layout.erl`:

```erlang
-module(blog_live_layout).
-include_lib("arizona/include/arizona_stateless.hrl").
-export([render/1]).

render(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [{lang, ~"en"}], [
            {head, [], [
                {meta, [{charset, ~"utf-8"}], []},
                {link, [{rel, ~"icon"}, {href, ~"/assets/favicon.ico"}], []},
                {meta, [{name, ~"viewport"}, {content, ~"width=device-width, initial-scale=1"}],
                    []},
                {title, [], [?get(title, ~"Blog"), ~" - Nova Blog"]}
            ]},
            {body, [], [
                {header, [], [
                    {a, [{href, ~"/"}], [~"Nova Blog"]},
                    {nav, [], [
                        {a, [{href, ~"/live/counter"}, az_navigate], [~"Counter"]},
                        {a, [{href, ~"/live/editor"}, az_navigate], [~"Write"]},
                        {a, [{href, ~"/live/dashboard/drafts"}, az_navigate], [~"Your posts"]}
                    ]}
                ]},
                ?inner_content,
                {aside, [{'aria-label', ~"Latest activity"}], [
                    {h2, [], [~"Latest"]},
                    {ul, [{id, ~"notifications"}], []}
                ]},
                {script, [{type, ~"module"}, {src, ~"/assets/js/notifications.js"}], []},
                {script, [{type, ~"module"}], [
                    ~"""
                    import { connect } from '/assets/arizona/arizona.min.js';
                    connect('/live/ws');
                    """
                ]}
            ]}
        ]}
    ]).
```

`?inner_content` is where the page goes. The layout reads the page's bindings, so `?get(title, ~"Blog")` picks up the `title` binding a page sets in `mount/1`. The last script loads the client and connects it to the `ws` route. The `nav` links and the "Latest" list are used in later chapters ([Live Navigation](live-navigation.md) and [Pub/Sub](../real-time/pubsub.md)); they do no harm yet.

A layout renders once, for the first HTTP response. It is never diffed, so it cannot hold live state (a `?stateful` component in a layout is a render error).

## The first live view

Create `src/live/blog_live_counter.erl`:

```erlang
-module(blog_live_counter).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(_Bindings) ->
    {#{id => ~"counter", title => ~"Counter", count => 0}, #{}}.

render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Counter"]},
            {button, [{id, ~"dec"}, {az_click, arizona_js:push_event(~"dec")}], [~"-"]},
            {span, [{id, ~"count"}], [?get(count)]},
            {button, [{id, ~"inc"}, {az_click, arizona_js:push_event(~"inc")}], [~"+"]}
        ]}
    ).

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []};
handle_event(~"dec", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) - 1}, #{}, []}.
```

`arizona_stateful.hrl` sets the `arizona_stateful` behaviour and enables the parse transform. The three callbacks:

- `mount/1` gets the route's initial bindings and returns `{Bindings, Resets}`: the view's state, and a map of values to re-apply after every event (empty here; see [Live Views](live-views.md)).
- `render/1` returns the template for the current bindings.
- `handle_event/3` gets the event name, its payload and the bindings, and returns `{Bindings, Resets, Effects}`. Effects are client-side commands such as "focus this field"; covered in [Events & Interactivity](events.md).

Every route page needs an `id` binding and must put it on its root element as `{id, ?get(id)}`. The id is how events find their view and how patches find their element. Leave it out and the module does not compile:

```
bad_view.erl:9:6: arizona_stateful render/1 root element must have an id attribute
%    9|     ?html({main, [], [?get(count)]}).
%     |      ^
```

Restart the node and open <http://localhost:8080/live/counter>. Click the buttons and the number changes, with no page reload.

## Template syntax

A template is built from three kinds of term:

- `{Tag, Attributes, Children}` - an element. Tags are atoms; quote the ones that are reserved words, such as `'div'`. Void elements like `input` take `[]` as children.
- A binary such as `~"Counter"` - text. Text and attribute values are HTML-escaped, so a binding holding `<script>` renders as text.
- `?get(Key)` - a dynamic value read from the bindings. Integers and other simple values are converted for you.

Attributes are `{Name, Value}` pairs or a bare atom for a boolean attribute (`hidden`, `az_navigate`). Attribute names with underscores, like `az_click`, are written with dashes in the HTML. A value can be a list, which is concatenated: `{class, [~"tag ", ?get(color)]}`.

Use `?get(Key, Default)` when a binding may be missing; plain `?get(Key)` raises `missing_binding`. `?html([...])` with a list renders several top-level nodes, which layouts use; a page must have a single root.

`?raw(Value)` splices a trusted HTML fragment without escaping. Never use it for anything a user typed.

## What goes over the wire

The first response is ordinary HTML. Here is the counter, from `curl -i localhost:8080/live/counter`, with line breaks added:

```
HTTP/1.1 200 OK
content-length: 890
content-type: text/html; charset=utf-8
...

<!DOCTYPE html>
<html lang="en">
...
<main az="69UO12-0" az-view id="counter">
<h1>Counter</h1>
<button id="dec" az-click="[0,&quot;dec&quot;]">-</button>
<span az="69UO12-1" id="count">
<!--az:69UO12-1-->0<!--/az-->
</span>
<button id="inc" az-click="[0,&quot;inc&quot;]">+</button>
</main>
...
<script type="module">import { connect } from '/assets/arizona/arizona.min.js';
connect('/live/ws');</script>
</body>
</html>
```

The parse transform marked the root as a view (`az-view`) and gave the one dynamic slot an address (`az="69UO12-1"`, with comment markers around the value). `az-click` holds the encoded command: op `0` is "push event", with the event name.

The client connects to `/live/ws?_az_path=/live/counter`. A click sends a JSON frame naming the target view, the event and a payload:

```json
["counter","inc",{}]
```

and the server answers with the ops to apply. This one is "set the text of slot `69UO12-1` in view `counter` to `1`":

```json
{"o":[[0,"counter:69UO12-1","1"]]}
```

The `h1` and the buttons never cross the wire again. Those frames are what the counter's WebSocket test in `test/blog_realtime_arizona_SUITE.erl` sends and expects.

## CSRF and the Origin check

Every `live` route (and the WebSocket upgrade, which runs the target page's middlewares) gets an Origin check by default: a request whose `Origin` header is neither this host nor in the `csrf_origins` allowlist is answered `403`. A page on another site cannot open a WebSocket into a reader's session:

```erlang
foreign_origin_websocket_is_refused(_Config) ->
    ?assertEqual(
        {error, 403},
        blog_ws_client:connect("/live/ws?_az_path=/live/counter", [
            {~"origin", ~"http://evil.example"}
        ])
    ).
```

Requests without an `Origin` header (curl, top-level page loads) are allowed. If the blog sits behind a proxy that rewrites `Host`, add the public origin to the `arizona` app env: `{arizona, [{csrf_origins, [~"https://blog.example.com"]}]}`. Because events travel over this checked WebSocket, live forms need no CSRF token. Plain Nova forms still use `nova_csrf_plugin` (see [Plugins](../foundations/plugins.md)).

```admonish important title="Arizona routes bypass Nova's plugins and security"
A request Arizona answers never reaches `nova_router`, so route-group `plugins`, `security` functions and global Nova plugins do not run for it. Arizona has its own per-route `middlewares` for that job. The [Live Views](live-views.md) chapter uses them to find the logged-in user.
```

## Development workflow

`rebar3 nova serve` recompiles changed modules as before. A connected live view keeps running the code and state it mounted with, so reload the browser tab after changing a view. Arizona has its own hot reloader that remounts open views on fresh code, but it needs an extra dev-only file watcher package (`arizona_watcher_fs`) and a `reloader` config; the blog does without it.

```admonish tip title="A dev MCP endpoint for coding agents"
Arizona ships `arizona_dev_mcp`, a Model Context Protocol server that lets an MCP client inspect the running node: render a component with given bindings, show a live view's current bindings (`view_state`), read recent logs, and evaluate Erlang. The blog mounts it in `dev_sys.config.src` only:

    {mcp, ~"/mcp", arizona_dev_mcp, #{allow_remote_access => false}},

It answers loopback clients only and refuses foreign origins, but `eval` is remote code execution, so never put it in a test or production config. Its `list_routes` tool reads the `arizona` server config, which `arizona_nova` does not use, so it reports no routes.
```

## Testing a live view

Live views are ordinary modules, so the first level of testing is calling the callbacks:

```erlang
counter_events_update_bindings(_Config) ->
    {Bindings, #{}} = blog_live_counter:mount(#{}),
    ?assertMatch(#{id := ~"counter", count := 0}, Bindings),
    {Inc, #{}, []} = blog_live_counter:handle_event(~"inc", #{}, Bindings),
    ?assertMatch(#{count := 1}, Inc),
    {Dec, #{}, []} = blog_live_counter:handle_event(~"dec", #{}, Inc),
    ?assertMatch(#{count := 0}, Dec).
```

`arizona_render:render_view_to_iolist/2` renders a page (with layouts) without a server, and the suite also fetches the page over HTTP and drives it over a real WebSocket with the [gun](https://github.com/ninenines/gun) client (a test-only dependency). [Testing Real-Time](../testing/testing-realtime.md) walks through those tests.

---

Next, [Live Views](live-views.md) builds the post editor and looks at the view lifecycle in detail.
