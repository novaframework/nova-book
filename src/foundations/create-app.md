# Your First Nova Application

The fastest way to start a Nova project is the `rebar3_nova` plugin. It adds a project template to `rebar3 new` and a family of `rebar3 nova` commands (`serve`, `routes`, `doctor`, generators and more) that we will use throughout the book.

## Installing the rebar3 plugin

You need Erlang/OTP 29 and rebar3 (Nova itself runs on OTP 28; Arizona in Part VI needs 29, so start on 29). If you do not have them yet, the [Erlang Essentials](../appendix/erlang-essentials.md#installing-erlang-and-rebar3) appendix shows how to install both with mise.

`rebar3_nova` is published on Hex. Register it as a global plugin by adding it to `~/.config/rebar3/rebar.config` (create the file if it does not exist):

```erlang
{plugins, [
    {rebar3_nova, "~> 0.13"}
]}.
```

The next `rebar3` command fetches the plugin. Check that it is there:

```shell
rebar3 plugins list
```

```
--- Global plugins ---
rebar3_nova (0.13.3)
```

When a new release comes out, `rebar3 plugins upgrade rebar3_nova` updates it.

```admonish note
The `rebar3_nova` repository also ships an `install.sh` script. It installs the plugin from the GitHub `master` branch rather than from Hex, and it rewrites your global `rebar.config` (dropping any comments in it). Editing the file by hand as above gives you a released, versioned plugin and leaves the rest of your config alone.
```

## Creating a new project

With the plugin installed, rebar3 has a `nova` template:

```shell
rebar3 new nova blog
```

```
===> Writing blog/config/dev_sys.config.src
===> Writing blog/config/prod_sys.config.src
===> Writing blog/src/blog.app.src
===> Writing blog/src/blog_app.erl
===> Writing blog/src/blog_sup.erl
===> Writing blog/src/blog_router.erl
===> Writing blog/src/controllers/blog_main_controller.erl
===> Writing blog/rebar.config
===> Writing blog/config/vm.args.src
===> Writing blog/priv/assets/favicon.ico
===> Writing blog/src/views/blog_main.dtl
===> Writing blog/.tool-versions
===> Writing blog/.gitignore
```

```admonish tip
`rebar3 nova new` is a newer command with flags such as `--kura`, `--arizona` and `--docker`. In 0.13 those flags still generate older dependency sets (for example the deprecated `arizona_core`), so this book starts from the plain `rebar3 new nova` template and adds dependencies by hand as each part needs them.
```

## Project structure

```
blog
├── .gitignore
├── .tool-versions
├── config
│   ├── dev_sys.config.src
│   ├── prod_sys.config.src
│   └── vm.args.src
├── priv
│   └── assets
│       └── favicon.ico
├── rebar.config
└── src
    ├── blog.app.src
    ├── blog_app.erl
    ├── blog_router.erl
    ├── blog_sup.erl
    ├── controllers
    │   └── blog_main_controller.erl
    └── views
        └── blog_main.dtl
```

- **`src/blog_router.erl`** - the route table (next chapter)
- **`src/controllers/`** - controller modules, plain functions that take a request and return a tuple
- **`src/views/`** - ErlyDTL (Django-style) templates, compiled to Erlang modules before the sources
- **`src/blog_app.erl`, `src/blog_sup.erl`** - the OTP application callback and its top supervisor
- **`config/dev_sys.config.src`** - development configuration, used by `rebar3 shell` and `rebar3 nova serve`
- **`config/prod_sys.config.src`, `config/vm.args.src`** - release configuration and VM flags
- **`priv/`** - files shipped with the application (static assets, data files)
- **`rebar.config`** - dependencies, profiles, release settings and plugins

The generated `rebar.config` depends on `nova` from Hex and lists `rebar3_nova` again as a *project* plugin, fetched from the GitHub `master` branch. Inside the project that copy wins over your global one, so `rebar3 nova ...` in the project directory runs whatever is on `master` at the time you first fetched it. If you prefer a released version, change the entry to `{rebar3_nova, "~> 0.13"}`.

```admonish note
`.tool-versions` pins Erlang 28.0.4 and rebar 3.25.0 for mise and asdf. The book uses OTP 29 and rebar3 3.27; bump the pins or delete the file if mise or asdf would otherwise install the older toolchain.
```

## Running the application

Start the development server:

```shell
cd blog
rebar3 nova serve
```

`serve` compiles the project, boots it in an interactive shell and watches `src/`, `src/controllers/`, `src/views/` and `priv/` for changes. Save a controller or a template and it is recompiled and reloaded into the running node:

```
===> Compiled blog_main_controller with warnings: []
```

The watcher uses the [`fs`](https://hex.pm/packages/fs) library. macOS works out of the box. On Linux `fs` needs `inotifywait` from the `inotify-tools` package (`apt install inotify-tools`, `dnf install inotify-tools`, ...). Without it the server still starts, but every watched directory logs

```
backend port not found: inotifywait
```

and nothing is reloaded. `rebar3 shell` starts the same node without the watcher.

```admonish warning title="Router changes need a restart"
The watcher recompiles `blog_router.erl` like any other module, but the route table is built once at startup. After editing the router, stop the shell (`Ctrl+G`, then `q`) and run `rebar3 nova serve` again.
```

Among the boot logs you should find:

```
=NOTICE REPORT==== 3-Nov-2026::08:26:52.248271 ===
    msg: <<"Nova is running">>
    url: <<"http://0.0.0.0:8080">>
    app: blog
    cowboy_version: <<"2.18.0">>
    nova_version: <<"0.18.1">>
```

That message is buried in a wall of `PROGRESS REPORT`s, because the generated `dev_sys.config.src` sets the log level to `debug` and its formatter setting never takes effect.

```admonish bug title="Fixing the generated logger config"
The template puts a bare `#{formatter => ...}` map in the `kernel` section, which OTP ignores. Wrap it in a `logger` handler entry instead (this is what the companion app uses):

~~~erlang
{kernel, [
    {logger_level, debug},
    {logger, [
        {handler, default, logger_std_h, #{
            formatter => {flatlog, #{
                map_depth => 3,
                term_depth => 50,
                colored => true,
                template => [colored_start, "[\033[1m", level, "\033[0m", colored_start, "] [", time, "]",
                             colored_end, " ", msg, " (", mfa, ")\n"]
            }}
        }}
    ]}
]},
~~~

Now each event is a single line:

~~~
[notice] [2026-11-03T07:25:12.812456+00:00] nova_version=0.18.1 cowboy_version=2.18.0 app=blog url=http://0.0.0.0:8080 msg="Nova is running"  (nova_sup:setup_cowboy/1)
~~~
```

Open `http://localhost:8080` in a browser to see the welcome page rendered from `blog_main.dtl`, or check the health endpoint from another terminal:

```shell
curl -i localhost:8080/heartbeat
```

```
HTTP/1.1 200 OK
content-length: 0
date: Tue, 03 Nov 2026 07:25:21 GMT
server: Cowboy/Nova
vary: accept-encoding
set-cookie: session_id=vphyPZUZo8EpU1BEFd0Eqya5eC6A+aO+Nne/MKDITxA=
```

The `session_id` cookie is Nova's session layer giving every new visitor a session. We come back to it in the [Sessions](../auth-sessions/sessions.md) chapter.

## Listing routes

```shell
rebar3 nova routes
```

```
Host: '_'
     ├─  GET /heartbeat (blog, blog_router:-routes/1-fun-0-/1)
     └─  GET / (blog, blog_main_controller:index/1)
```

The heartbeat route is an anonymous function in the router, hence the odd-looking name.

## Checking the project

`rebar3 nova doctor` runs a set of health checks over the toolchain, project layout, configuration, routes and dependencies:

```shell
rebar3 nova doctor
```

```
=== Nova Doctor ===

✅ Toolchain
    (3 ok, run with -v for detail)

✅ Project structure
    (3 ok, run with -v for detail)

⚠️ Configuration    2 warning(s)
    ! json_lib defaults to thoas
        hint: Set {json_lib, json} in sys.config to use OTP 27+ built-in json module
    ! session_backend unset (defaults to ETS)
        hint: Fine for dev; pick a durable backend for production

✅ Routes
    (3 ok, run with -v for detail)

✅ Dependencies
    (2 ok, run with -v for detail)

✅ Security hygiene
    (2 ok, run with -v for detail)

✅ Build artifacts
    (2 ok, run with -v for detail)

--
Summary: 16 ok, 2 warning(s), 0 error(s)
```

```admonish warning
Do not follow the `json_lib` hint literally. Nova reads `json_lib` from your *application's* environment, not from the `nova` section, and expects a `decode/1` that returns `{ok, Term}`. OTP's `json:decode/1` returns the term directly, so `{json_lib, json}` makes every JSON request body fail with a 400. The [Plugins](plugins.md#choosing-the-json-library) chapter shows the three-line adapter the blog uses instead. The doctor keeps showing both warnings either way; they are advisory.
```

## From scaffold to the companion app

The finished blog lives in [`examples/blog`](https://github.com/novaframework/nova-book/tree/main/examples/blog) in the book's repository. It started as exactly this scaffold. Over the course of the book it grows:

- **Dependencies** - `rebar.config` gains `kura` and `kura_postgres` (data layer on PostgreSQL via minato), `nova_auth` (authentication), `shigoto` (background jobs), `hikyaku` (email), `arizona_nova` (real-time views) and `opentelemetry_nova`/`opentelemetry_kura` (observability). The test profile adds `nova_test`.
- **Plugins** - `rebar3_kura` joins the project plugins and a `{compile, {kura, compile}}` hook runs next to the ErlyDTL one.
- **Configuration** - `config/test_sys.config` is added for Common Test, the configs gain a `kura` section with the repo settings, and a `blog` section holds the app's own settings (the JSON library, tokens and so on).
- **Code** - controllers, views, schemas, migrations and tests under `src/`, `priv/` and `test/`, chapter by chapter.

You can follow along in your own scaffold or check out the companion app and read the code as you go. Its tests run against PostgreSQL started with `docker compose up -d`:

```shell
cd examples/blog
docker compose up -d
rebar3 ct
```

```admonish bug title="If rebar3 ct fails on a fresh scaffold"
The template's test profile pins `meck` 1.1.0, which does not compile on OTP 29 (`'catch ...' is deprecated` becomes an error under `warnings_as_errors`). Change the pin in `rebar.config` to `{meck, "1.2.0"}`. If you already ran `rebar3 ct` once, also run `rebar3 as test upgrade meck` so the old copy in `_build/test` is replaced.
```

---

Now that you have a running application, let's look at how [routing](routing.md) works in Nova.
