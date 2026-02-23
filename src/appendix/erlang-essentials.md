# Erlang Essentials

This appendix is not a full Erlang tutorial. It provides a quick reference for the Erlang concepts used in this book and links to comprehensive learning resources.

## Learning resources

- **[Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/)** — The best free online book for learning Erlang from scratch. Covers everything from syntax to OTP.
- **[Adopting Erlang](https://adoptingerlang.org/)** — Practical guide for teams adopting Erlang, covering development setup, building, and running in production.
- **[Erlang/OTP Documentation](https://www.erlang.org/doc/)** — Official reference documentation.

## Installing Erlang and Rebar3

We recommend [mise](https://mise.jdx.dev/) for managing tool versions:

```shell
# Install mise (if not already installed)
curl https://mise.run | sh

# Install Erlang and rebar3
mise use erlang@26
mise use rebar@3.23

# Verify
erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell
rebar3 version
```

Alternatively, use [asdf](https://asdf-vm.com/):

```shell
asdf plugin add erlang
asdf plugin add rebar
asdf install erlang 26.2.2
asdf install rebar 3.22.1
```

## Quick reference

### Atoms

Atoms are constants. They start with a lowercase letter or are quoted with single quotes:

```erlang
ok, error, true, false, undefined
'Content-Type', 'my-atom'
```

### Binaries and strings

Erlang has two string types. Binaries (double quotes with `<<>>`) are what you will use most:

```erlang
<<"hello">>          %% binary string
"hello"              %% list of integers (less common in Nova)
```

### Tuples

Fixed-size containers, often used for tagged return values:

```erlang
{ok, Value}
{error, not_found}
{json, #{users => []}}
```

### Maps

Key-value data structures. Nova uses maps extensively for requests, responses, and configuration:

```erlang
%% Creating
#{name => <<"Alice">>, age => 30}

%% Pattern matching
#{name := Name} = Map

%% Updating
Map#{age => 31}
```

### Pattern matching

Erlang's most powerful feature. Used in function heads, case expressions, and assignments:

```erlang
%% Function clause matching
handle(#{method := <<"GET">>} = Req) -> get_handler(Req);
handle(#{method := <<"POST">>} = Req) -> post_handler(Req).

%% Case expression
case blog_repo:get(post, Id) of
    {ok, Post}         -> handle_post(Post);
    {error, not_found} -> not_found
end.
```

### Lists and list comprehensions

```erlang
[1, 2, 3]
[Head | Tail] = [1, 2, 3]   %% Head = 1, Tail = [2, 3]

%% List comprehension
[X * 2 || X <- [1, 2, 3]]   %% [2, 4, 6]

%% With maps
[row_to_map(R) || R <- Rows]
```

### Modules and functions

```erlang
-module(my_module).
-export([my_function/1]).

my_function(Arg) ->
    %% function body
    ok.
```

### Anonymous functions (funs)

Used extensively in Nova for route handlers and security functions:

```erlang
fun my_module:my_function/1       %% Reference to named function
fun(X) -> X + 1 end              %% Anonymous function
fun(_) -> {status, 200} end      %% Ignore argument
```

## OTP in 5 minutes

### Applications

An OTP application is a component with a defined start/stop lifecycle. Your Nova project is an application. It has:
- An `.app.src` file describing metadata and dependencies
- An `_app.erl` module implementing the `application` behaviour
- A `_sup.erl` module implementing the `supervisor` behaviour

### Supervisors

Supervisors manage child processes and restart them if they crash. The generated `blog_sup.erl` is your application's supervisor.

### gen_server

A generic server process. Used for stateful workers:

```erlang
-module(my_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.  %% Initial state

handle_call(Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
```

## Rebar3 basics

```shell
rebar3 compile          # Compile the project
rebar3 shell            # Start an interactive shell
rebar3 eunit            # Run EUnit tests
rebar3 ct               # Run Common Test suites
rebar3 as prod release  # Build a production release
rebar3 as prod tar      # Build a release tarball
rebar3 nova serve       # Development server with hot-reload
rebar3 nova routes      # List all registered routes
```
