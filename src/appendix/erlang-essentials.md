# Erlang Essentials

This appendix is not a full Erlang tutorial. It is a quick reference for the parts of the language this book uses, written for OTP 29, with links to proper learning material.

## Learning resources

- **[Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/)** - the best free book for learning Erlang from scratch, from syntax to OTP. It predates the newer syntax shown below, but everything in it still applies.
- **[Adopting Erlang](https://adoptingerlang.org/)** - a practical guide to development setup, building and running Erlang in production.
- **[Erlang/OTP documentation](https://www.erlang.org/doc/)** - the official reference.

## Installing Erlang and rebar3

We recommend [mise](https://mise.jdx.dev/) for managing tool versions:

```shell
curl https://mise.run | sh
mise use -g erlang@29 rebar@3
rebar3 version
```

`mise use -g` installs the tools and makes them the default for your user; drop `-g` inside a project to pin versions in that project's `mise.toml` instead. The book needs OTP 29: Nova, Kura and Shigoto run on 28, but Arizona (Part VI) requires 29.

mise builds Erlang from source, which takes a few minutes and needs a C compiler plus the OpenSSL and ncurses development headers. On Debian or Ubuntu:

```shell
sudo apt install build-essential autoconf libssl-dev libncurses-dev
```

`rebar3 version` confirms both tools and the OTP release they run on:

```
rebar 3.27.0 on Erlang/OTP 29 Erts 17.1.1
```

On Linux, also install `inotify-tools` so that `rebar3 nova serve` can reload code when files change (see [Your First Nova Application](../foundations/create-app.md#running-the-application)).

## Quick reference

To try the snippets below, start a shell with `rebar3 shell` in any rebar3 project (the blog will do) and type expressions at the prompt, ending each with a full stop. For code that belongs in a module, put it in a file under `src/` and run `rebar3 shell` again.

### Atoms

Atoms are named constants. They start with a lowercase letter, or are quoted with single quotes:

```erlang
[ok, error, true, false, undefined, 'Content-Type', 'my-atom']
```

Atoms are never garbage collected, so never create them from user input (`binary_to_atom/1` on a request parameter is a classic denial of service). Keep user data as binaries.

### Binaries and strings

Erlang has two kinds of strings. **Binaries** are what Nova uses everywhere: request paths, headers, JSON keys, template output. Write them with the `~"..."` sigil:

```erlang
Name = ~"Ada",
Greeting = <<"Hello, ", Name/binary, "!">>.
```

`Greeting` is `~"Hello, Ada!"`. The `<<...>>` syntax builds binaries from parts; `~"hello"` and `<<"hello">>` are the same value.

Double quotes without the sigil give a **list of characters**: `"hello"` is `[104,101,108,108,111]`. You will meet them in configuration and some older APIs (`{redirect, "/about"}` accepts either).

Binary literals in source files are UTF-8, so `byte_size(~"åäö")` is 6 while `string:length(~"åäö")` is 3. The `string` module works on both kinds.

Triple-quoted sigils hold multi-line text and embedded quotes; the indentation of the closing `"""` is removed from every line:

```erlang
~"""
A "quoted" word
on two lines
"""
```

### Tuples

Fixed-size containers, usually tagged with an atom in the first position. Controllers return them:

```erlang
[{ok, 42}, {error, not_found}, {json, #{posts => []}}]
```

### Maps

Key-value data. Nova uses maps for requests, options, route groups and configuration:

```erlang
Post = #{title => ~"Hello", views => 0},
#{title := Title} = Post,
Viewed = Post#{views := 1},
Tagged = Viewed#{tags => [~"erlang"]},
Default = maps:get(author, Post, ~"anonymous").
```

- `=>` creates or updates a key; `:=` updates a key that must already exist (it raises `{badkey, Key}` otherwise) and is also how you match.
- `maps:get/3` returns a default for a missing key.

Map comprehensions build maps from maps or lists:

```erlang
#{K => V * 2 || K := V <- #{a => 1, b => 2}}
```

That evaluates to `#{a => 2, b => 4}`.

### Pattern matching

Erlang's most important feature. `=` matches rather than assigns, function clauses are chosen by matching their arguments, and `case` matches a value against patterns:

```erlang
handle(#{method := ~"GET"} = Req) -> {get, Req};
handle(#{method := ~"POST"} = Req) -> {post, Req}.

lookup(Id) ->
    case find_post(Id) of
        {ok, Post} -> {json, Post};
        {error, not_found} -> {status, 404}
    end.
```

If no clause matches, the process crashes with `function_clause` (or `case_clause`). That is usually what you want: handle the cases you expect and let the rest fail loudly.

Guards add conditions to a clause:

```erlang
page_size(#{~"limit" := Limit}) when is_binary(Limit) ->
    min(binary_to_integer(Limit), 100);
page_size(_Query) ->
    20.
```

### Lists, comprehensions and recursion

```erlang
[Head | Tail] = [1, 2, 3],
Doubled = [X * 2 || X <- [1, 2, 3]],
Even = [X || X <- [1, 2, 3, 4], X rem 2 =:= 0],
Titles = [Title || #{title := Title} <- [#{title => ~"A"}, #{title => ~"B"}]].
```

`Head` is `1` and `Tail` is `[2, 3]`. The last comprehension uses a pattern as a filter: elements that do not match are skipped, giving `[~"A", ~"B"]`.

When a comprehension is not enough, write a recursive function. It reads better than a fold and the compiler optimises it well:

```erlang
sum([]) -> 0;
sum([N | Rest]) -> N + sum(Rest).
```

### The maybe expression

`maybe` chains steps that can each fail. `?=` matches like `=`, but when the match fails the whole `maybe` expression returns the non-matching value instead of crashing:

```erlang
publish(Params) ->
    maybe
        {ok, Title} ?= required(~"title", Params),
        {ok, Body} ?= required(~"body", Params),
        {ok, #{title => Title, body => Body}}
    end.

required(Key, Params) ->
    case maps:get(Key, Params, ~"") of
        ~"" -> {error, {missing, Key}};
        Value -> {ok, Value}
    end.
```

`publish(#{~"title" => ~"T"})` returns `{error, {missing, ~"body"}}`. The blog's controllers use `maybe` to validate input and hand errors to a [fallback controller](../foundations/controllers.md#fallback-controllers).

### JSON

OTP ships a `json` module:

```erlang
Encoded = iolist_to_binary(json:encode(#{title => ~"Hello", tags => [~"erlang"]})),
Decoded = json:decode(~"{\"title\":\"Hello\",\"draft\":false}").
```

`Encoded` is `~"{\"title\":\"Hello\",\"tags\":[\"erlang\"]}"` (do not rely on key order) and `Decoded` is `#{~"title" => ~"Hello", ~"draft" => false}`. `json:encode/1` returns iodata, which Cowboy accepts as a response body as it is. Decoded keys are always binaries. JSON `null` is the atom `null`.

### Modules and functions

```erlang
-module(essentials).
-export([greet/1]).

-doc "Greets someone by name.".
-spec greet(binary()) -> binary().
greet(Name) ->
    <<"Hello, ", Name/binary>>.
```

A function is identified by name and arity (`greet/1`), and only exported functions can be called from other modules. `-doc` attributes document modules and functions, and `-spec` declares types for Dialyzer.

### Funs

Functions are values. Nova route handlers and security callbacks are references to named functions:

```erlang
Named = fun essentials:greet/1,
Anonymous = fun(X) -> X + 1 end,
Ignore = fun(_) -> {status, 200} end.
```

Prefer `fun Module:Function/Arity` whenever a function crosses a module boundary: it survives code reloading and shows up with a readable name in tools such as `rebar3 nova routes`.

### Logging

Use the logger macros with a map report:

```erlang
-include_lib("kernel/include/logger.hrl").

log_demo(PostId) ->
    ?LOG_INFO(#{msg => ~"post published", post_id => PostId}).
```

The macros add the module, function and line to the event's metadata, and a map report keeps the data structured for formatters that emit JSON.

## OTP in 5 minutes

### Applications

An OTP application is a component with a start/stop lifecycle. Your Nova project is one. It has:

- an `.app.src` file with metadata and the applications it depends on (they are started first, in order)
- a module implementing the `application` behaviour (`blog_app.erl`)
- a top-level supervisor (`blog_sup.erl`)

### Supervisors

Supervisors start child processes and restart them when they crash, according to a strategy (`one_for_one`, `one_for_all`, ...). Crashing and restarting from a known state is how Erlang systems stay up.

### gen_server

A generic server process that owns some state and handles requests one at a time:

```erlang
-module(post_counter).
-behaviour(gen_server).

-export([start_link/0, increment/0, count/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    gen_server:cast(?MODULE, increment).

count() ->
    gen_server:call(?MODULE, count).

init([]) ->
    {ok, #{count => 0}}.

handle_call(count, _From, #{count := Count} = State) ->
    {reply, Count, State}.

handle_cast(increment, #{count := Count} = State) ->
    {noreply, State#{count := Count + 1}}.

handle_info(_Info, State) ->
    {noreply, State}.
```

`call` waits for a reply, `cast` does not. In a real application you start the server from a supervisor rather than by hand.

## Rebar3 basics

```shell
rebar3 compile          # Compile the project
rebar3 shell            # Start an interactive shell with the project running
rebar3 eunit            # Run EUnit tests
rebar3 ct               # Run Common Test suites
rebar3 fmt              # Format the code (erlfmt plugin)
rebar3 xref             # Find calls to undefined functions
rebar3 dialyzer         # Static type analysis
rebar3 as prod release  # Build a production release
rebar3 as prod tar      # Build a release tarball
rebar3 nova serve       # Development server with code reloading
rebar3 nova routes      # List all registered routes
```
