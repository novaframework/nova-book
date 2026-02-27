# Create a New Application

The fastest way to get started with Nova is the `rebar3_nova` plugin. It provides project templates that scaffold a complete, runnable Nova application.

## Installing the rebar3 plugin

Run the installer script to set up `rebar3_nova`:

```shell
sh -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh)"
```

This checks for rebar3 (installing it if needed) and adds the `rebar3_nova` plugin to your global rebar3 config.

## Creating a new project

Rebar3's `new` command generates project scaffolding. With the Nova plugin installed, you have a `nova` template:

```shell
rebar3 new nova blog
```

This creates a directory with everything needed for a running Nova application:

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
The generated `.tool-versions` file works with mise and asdf. Run `mise install` or `asdf install` to get the exact Erlang and rebar3 versions for this project.
```

## Project structure

Here is what was generated:

- **`src/`** — Your source code
  - **`src/controllers/`** — Controller modules that handle request logic
  - **`src/views/`** — ErlyDTL (Django-style) templates for HTML rendering
  - **`blog_router.erl`** — Route definitions
  - **`blog_app.erl`** — OTP application callback
  - **`blog_sup.erl`** — Supervisor
- **`config/`** — Configuration files
  - **`dev_sys.config.src`** — Development config (used by `rebar3 shell`)
  - **`prod_sys.config.src`** — Production config (used in releases)
  - **`vm.args.src`** — Erlang VM arguments
- **`rebar.config`** — Build configuration, dependencies, and release settings

## Running the application

Start the development server:

```shell
cd blog
rebar3 nova serve
```

This compiles your code, starts an Erlang shell, and watches for file changes — when you save a file, it is automatically recompiled and reloaded. No restart needed.

```admonish note
`rebar3 nova serve` requires [enotify](https://github.com/tsloughter/enotify). On Linux, install `inotify-tools` from your package manager. On macOS, `fsevent` is used automatically.

If enotify is not available, use `rebar3 shell` instead. It works the same but without automatic recompilation.
```

Once the node is up, open your browser to `http://localhost:8080`. You should see the Nova welcome page.

You can also verify the application is running with curl:

```shell
curl -v localhost:8080/heartbeat
```

A `200 OK` response means everything is working.

## Listing routes

To see all registered routes:

```shell
rebar3 nova routes
```

```
Host: '_'
     ├─  /assets
        └─  _ /[...] (blog, cowboy_static:init/1)
     └─  GET / (blog, blog_main_controller:index/1)
```

This shows the static asset handler and the index route that renders the welcome page.

---

Now that you have a running application, let's look at how [routing](routing.md) works in Nova.
