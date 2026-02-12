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
rebar3 new nova my_first_nova
```

This creates a directory with everything needed for a running Nova application:

```
===> Writing my_first_nova/config/dev_sys.config.src
===> Writing my_first_nova/config/prod_sys.config.src
===> Writing my_first_nova/src/my_first_nova.app.src
===> Writing my_first_nova/src/my_first_nova_app.erl
===> Writing my_first_nova/src/my_first_nova_sup.erl
===> Writing my_first_nova/src/my_first_nova_router.erl
===> Writing my_first_nova/src/controllers/my_first_nova_main_controller.erl
===> Writing my_first_nova/rebar.config
===> Writing my_first_nova/config/vm.args.src
===> Writing my_first_nova/priv/assets/favicon.ico
===> Writing my_first_nova/src/views/my_first_nova_main.dtl
===> Writing my_first_nova/.tool-versions
===> Writing my_first_nova/.gitignore
```

```admonish tip
The generated `.tool-versions` file works with mise and asdf. Run `mise install` or `asdf install` to get the exact Erlang and rebar3 versions for this project.
```

## Project structure

Here is what was generated:

- **`src/`** — Your source code
  - **`src/controllers/`** — Controller modules that handle request logic
  - **`src/views/`** — ErlyDTL (Django-style) templates for HTML rendering
  - **`my_first_nova_router.erl`** — Route definitions
  - **`my_first_nova_app.erl`** — OTP application callback
  - **`my_first_nova_sup.erl`** — Supervisor
- **`config/`** — Configuration files
  - **`dev_sys.config.src`** — Development config (used by `rebar3 shell`)
  - **`prod_sys.config.src`** — Production config (used in releases)
  - **`vm.args.src`** — Erlang VM arguments
- **`rebar.config`** — Build configuration, dependencies, and release settings

## Running the application

Start the development server:

```shell
cd my_first_nova
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
        └─  _ /[...] (my_first_nova, cowboy_static:init/1)
     └─  GET / (my_first_nova, my_first_nova_main_controller:index/1)
```

This shows the static asset handler and the index route that renders the welcome page.

---

Now that you have a running application, let's look at how [routing](routing.md) works in Nova.
