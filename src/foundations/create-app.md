# Your First Nova Application

The fastest way to start a Nova project is the `rebar3_nova` plugin. It adds a project template to `rebar3 new` and a family of `rebar3 nova` commands (`serve`, `routes`, `doctor`, generators and more) that we will use throughout the book.

## Installing the rebar3 plugin

You need Erlang/OTP 29 and rebar3 (Nova itself runs on OTP 28; Arizona in Part VI needs 29, so start on 29). If you do not have them yet, the [Erlang Essentials](../appendix/erlang-essentials.md#installing-erlang-and-rebar3) appendix shows how to install both with mise.

`rebar3_nova` is published on Hex. Register it as a global plugin by adding it to `~/.config/rebar3/rebar.config` (create the file if it does not exist):

```erlang
{plugins, [
    {rebar3_nova, "~> 0.14"}
]}.
```

The next `rebar3` command fetches the plugin. Check that it is there:

```shell
rebar3 plugins list
```

```
--- Global plugins ---
rebar3_nova (0.14.0)
```

When a new release comes out, `rebar3 plugins upgrade rebar3_nova` updates it.

```admonish note
The `rebar3_nova` repository also has a one-line installer, fetched from the `v0.14.0` release tag:

~~~shell
bash -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/rebar3_nova/v0.14.0/install.sh)"
~~~

It adds `{rebar3_nova, "~> 0.13"}` to the same file as text, keeps the rest of the file as it is, and leaves the file alone if `rebar3_nova` is already listed. `~> 0.13` accepts every 0.x release from 0.13 on, so it installs 0.14.0 too. The book uses commands that need 0.14, which is why the line above says `~> 0.14`.
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
===> Writing blog/config/dev_vm.args.src
===> Writing blog/rel/hooks/require_cookie
===> Writing blog/priv/assets/favicon.ico
===> Writing blog/src/views/blog_main.dtl
===> Writing blog/.tool-versions
===> Writing blog/.gitignore
```

```admonish tip
`rebar3 nova new` is a newer command with flags such as `--kura`, `--arizona` and `--docker` that add a data layer, live views or a Dockerfile in one go. This book starts from the plain `rebar3 new nova` template instead and adds each dependency in the chapter that needs it, so you see every piece arrive.
```

## Project structure

```
blog
├── .gitignore
├── .tool-versions
├── config
│   ├── dev_sys.config.src
│   ├── dev_vm.args.src
│   ├── prod_sys.config.src
│   └── vm.args.src
├── priv
│   └── assets
│       └── favicon.ico
├── rebar.config
├── rel
│   └── hooks
│       └── require_cookie
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
- **`config/prod_sys.config.src`, `config/vm.args.src`** - production release configuration and VM flags
- **`config/dev_vm.args.src`** - VM flags for a development release (`rebar3 release`)
- **`rel/hooks/require_cookie`** - a start hook for production releases (below)
- **`priv/`** - files shipped with the application (static assets, data files)
- **`rebar.config`** - dependencies, profiles, release settings and plugins

The generated `rebar.config` depends on `{nova, "~> 0.18"}` from Hex and lists `{rebar3_nova, "~> 0.13"}` again as a *project* plugin. Inside the project that copy wins over your global one. Both come from Hex, and a fresh project resolves `~> 0.13` to the newest 0.x release, 0.14.0 at the time of writing. The companion app pins `{rebar3_nova, "~> 0.14"}`, since the book relies on 0.14's commands.

`.tool-versions` pins Erlang 29.1.2 and rebar 3.27.0 for mise and asdf, the toolchain this book uses.

The release configs name the node `blog@localhost` and keep Erlang distribution on the loopback interface. A development release takes the distribution cookie from `~/.erlang.cookie`. A production release (`rebar3 as prod release`) reads it from the `RELEASE_COOKIE` environment variable, and `rel/hooks/require_cookie` stops `bin/blog` from starting the node without it. [Deployment](../production/deployment.md) covers releases.

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

Editing the router works the same way: serve recompiles it and registers its routes again, so a new route answers without a restart:

```
===> Compiled blog_router with warnings: []
===> Reloaded routes for blog
```

The watcher uses the [`fs`](https://hex.pm/packages/fs) library. macOS works out of the box. On Linux `fs` needs `inotifywait` from the `inotify-tools` package (`apt install inotify-tools`, `dnf install inotify-tools`, ...). Without it the server still starts, but nothing is reloaded, and serve says so when it boots:

```
===> rebar3 nova serve needs inotifywait to watch files. Install inotify-tools (apt install inotify-tools, dnf install inotify-tools, pacman -S inotify-tools). Code reloading is off; restart serve after installing it.
```

`rebar3 shell` starts the same node without the watcher.

Among the boot logs you should find:

```
[notice] [2026-11-07T22:23:55.856654+00:00] nova_version=0.18.1 cowboy_version=2.18.0 app=blog url=http://0.0.0.0:8080 msg="Nova is running"  (nova_sup:setup_cowboy/1)
```

The generated `dev_sys.config.src` formats every log event as one line with [flatlog](https://hex.pm/packages/flatlog) and sets the level to `debug`, so the supervisors' progress reports are printed too. Raise `logger_level` to `notice` if they get in the way.

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
get      /                                        blog_main_controller:index/1
get      /heartbeat                               blog_router:-routes/1-fun-0-/1
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

✅ Configuration
    (3 ok, run with -v for detail)

✅ Routes
    (3 ok, run with -v for detail)

✅ Dependencies
    (2 ok, run with -v for detail)

✅ Security hygiene
    (2 ok, run with -v for detail)

✅ Build artifacts
    (2 ok, run with -v for detail)

--
Summary: 18 ok, 0 warning(s), 0 error(s)
```

`rebar3 nova doctor -v` prints every check. The configuration checks read `json_lib` and `use_sessions` from your application's environment, where Nova reads them, and check that the JSON module has `encode/1` and `decode/1`. Run the doctor again whenever you change the config or the router.

## From scaffold to the companion app

The finished blog lives in [`examples/blog`](https://github.com/novaframework/nova-book/tree/main/examples/blog) in the book's repository. It started as exactly this scaffold. Over the course of the book it grows:

- **Dependencies** - `rebar.config` gains `kura` and `kura_postgres` (data layer on PostgreSQL via minato), `nova_auth` (authentication), `shigoto` (background jobs), `hikyaku` (email), `arizona_nova` (real-time views), `nova_json_schemas` (request validation) and `opentelemetry`, `opentelemetry_exporter`, `opentelemetry_nova` and `opentelemetry_kura` (observability). The test profile adds `nova_test` and `gun` (a WebSocket client for the real-time tests).
- **Plugins** - `rebar3_kura` joins the project plugins and a `{compile, {kura, compile}}` hook runs next to the ErlyDTL one.
- **Releases** - the blog was scaffolded before the template split its VM flags, so its development flags are in `config/vm.args.src` and its production flags in `config/prod_vm.args.src` (see [Configuration](../production/configuration.md)).
- **Configuration** - `config/test_sys.config` is added for Common Test, the configs gain a `kura` section with the repo settings, and a `blog` section holds the app's own settings (the JSON library, tokens and so on).
- **Code** - controllers, views, schemas, migrations and tests under `src/`, `priv/` and `test/`, chapter by chapter.

You can follow along in your own scaffold or check out the companion app and read the code as you go. Its tests run against PostgreSQL started with `docker compose up -d`:

```shell
cd examples/blog
docker compose up -d
rebar3 ct
```

---

Now that you have a running application, let's look at how [routing](routing.md) works in Nova.
