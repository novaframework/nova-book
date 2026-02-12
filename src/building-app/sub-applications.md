# Sub-Applications

Nova applications are composable — you can mount one Nova app inside another. This lets you use third-party Nova packages or split a large application into independent modules that each manage their own routes.

## Adding a sub-application

We will use [Nova Admin](https://github.com/novaframework/nova_admin) as an example. It provides an observer-like web interface for monitoring your running Erlang node.

### Adding the dependency

In `rebar.config`:

```erlang
{deps, [
        nova,
        {flatlog, "0.1.2"},
        pgo,
        {nova_admin, ".*", {git, "git@github.com:novaframework/nova_admin.git", {branch, "master"}}}
       ]}.
```

### Configuring the sub-application

Tell Nova about the new application in `dev_sys.config.src`. Nova has a `nova_apps` configuration that you set in your own application's environment:

```erlang
{my_first_nova, [
    {nova_apps, [
        {nova_admin, #{prefix => "/admin"}}
    ]}
]}
```

The `prefix` option means all routes from `nova_admin` are mounted under `/admin`. So if nova_admin has a route for `/`, it becomes `/admin/` in your application.

Your full `dev_sys.config.src` should look like:

```erlang
[
  {kernel, [
    {logger_level, debug},
    {logger, [
      {handler, default, logger_std_h,
        #{formatter => {flatlog, #{
            map_depth => 3,
            term_depth => 50,
            colored => true,
            template => [colored_start, "[\033[1m", level, "\033[0m",
                         colored_start, "] ", msg, "\n", colored_end]
          }}}}
    ]}
  ]},
  {nova, [
         {use_stacktrace, true},
         {environment, dev},
         {cowboy_configuration, #{port => 8080}},
         {dev_mode, true},
         {bootstrap_application, my_first_nova},
         {plugins, [
                    {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
                   ]}
        ]},
  {my_first_nova, [
      {nova_apps, [
          {nova_admin, #{prefix => "/admin"}}
      ]}
  ]}
].
```

### How it works

When Nova starts, it reads the `bootstrap_application` setting and looks for the `nova_apps` configuration in that application's environment. It compiles routes from all listed sub-applications and merges them into the routing tree.

Each sub-application is a standalone Nova app with its own router module. Nova Admin has a `nova_admin_router.erl` that defines its own routes. When you set `prefix => "/admin"`, Nova prepends that prefix to all of nova_admin's routes.

### Starting it up

```shell
rebar3 nova serve
```

Check the routes:

```shell
rebar3 nova routes
```

You should see nova_admin's routes mounted under `/admin`. Visit `localhost:8080/admin` to see the Nova Admin interface — it shows running processes, memory usage, and other information about your Erlang node.

## Multiple sub-applications

Mount multiple sub-applications, each with their own prefix:

```erlang
{my_first_nova, [
    {nova_apps, [
        {nova_admin, #{prefix => "/admin"}},
        {my_api_app, #{prefix => "/api"}}
    ]}
]}
```

## Using sub-applications in your own projects

The same pattern works for any Nova application:

1. Add it as a dependency in `rebar.config`
2. Add it to `nova_apps` in your sys.config with an optional prefix
3. Start your application

This is one of Nova's powerful features — you can compose applications from multiple Nova apps, each handling their own piece of the system.

---

Our application is feature-complete. Let's prepare it for production with [deployment](deployment.md).
