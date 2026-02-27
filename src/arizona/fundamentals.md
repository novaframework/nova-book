# Arizona Fundamentals

So far our blog renders HTML on the server and sends complete pages to the browser. For real-time interactivity — updating a comment section without a page reload, live form validation, instant notifications — we need something more. [Arizona](https://github.com/arizona-framework/arizona_core) is a real-time web framework for Erlang, inspired by Phoenix LiveView, that brings server-rendered interactivity to Nova applications.

```admonish info
Arizona requires OTP 28+. Check your Erlang version with `erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell`.
```

## What Arizona does

Arizona keeps a persistent WebSocket connection between the browser and the server. When state changes on the server, Arizona:

1. Computes which parts of the HTML changed (differential rendering)
2. Sends only the changed parts over the WebSocket
3. The client patches the DOM using morphdom

No full page reloads. No client-side framework. The state lives on the server in an Erlang process.

## How it works under the hood

Arizona uses **compile-time template optimization** via Erlang parse transforms. When you write a template:

```erlang
render(State) ->
    arizona:render("
        <h1>Hello, ~{maps:get(name, State)}!</h1>
        <p>You have ~{integer_to_list(maps:get(count, State))} messages.</p>
    ").
```

At compile time, Arizona:
1. Parses the template into static and dynamic segments
2. Tracks which variables each segment depends on
3. Generates code that only re-renders segments whose dependencies changed

This means when `count` changes but `name` doesn't, only the second `<p>` is re-rendered and sent to the client.

## Adding Arizona to your project

Add the dependency to `rebar.config`:

```erlang
{deps, [
    nova,
    {kura, "~> 1.0"},
    {arizona_core, {git, "https://github.com/arizona-framework/arizona_core.git", {branch, "main"}}}
]}.
```

Add `arizona_core` to your application dependencies in `src/blog.app.src`:

```erlang
{applications,
 [kernel,
  stdlib,
  nova,
  kura,
  arizona_core
 ]},
```

## Template syntax

Arizona supports three template syntaxes. The HTML string syntax is the most common:

```erlang
%% Embedded Erlang expressions with ~{...}
arizona:render("
    <div class=\"post\">
        <h2>~{maps:get(title, State)}</h2>
        <p>~{maps:get(body, State)}</p>
    </div>
")
```

Expressions inside `~{...}` are evaluated at render time and tracked for differential updates.

## The connection lifecycle

1. Browser requests a page — Nova renders the initial HTML and sends a full page
2. Arizona's JavaScript client opens a WebSocket connection
3. The server spawns an `arizona_live` GenServer for this connection
4. User interactions trigger events sent over the WebSocket
5. The server processes events, updates state, and pushes DOM diffs back
6. The client patches the DOM

Each connected user has their own server-side process holding their state — true server-rendered interactivity.

---

Next, let's build our first live view in [Live Views](live-views.md).
