# Routing

In the previous chapter we created a Nova application and saw it running. Now let's understand how requests are matched to controller functions.

## The router module

When Nova generated our project, it created `my_first_nova_router.erl`:

```erlang
-module(my_first_nova_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

routes(_Environment) ->
    [#{prefix => "",
      security => false,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
      }].
```

The `routes/1` function returns a list of **route groups**. Each group is a map with these keys:

| Key | Description |
|---|---|
| `prefix` | Path prefix prepended to all routes in this group |
| `security` | `false` or a fun reference to a security module |
| `routes` | List of route tuples |

Each route tuple has the form `{Path, Handler, Options}`:
- **Path** — the URL pattern (e.g. `"/users/:id"`)
- **Handler** — a fun reference like `fun Module:Function/1`
- **Options** — a map, typically `#{methods => [get, post, ...]}`

## Adding a route

Let's add a login page route:

```erlang
routes(_Environment) ->
  [#{prefix => "",
      security => false,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}}
                ]
      }].
```

We will implement the `login/1` function in the [Views](views.md) chapter.

## Prefixes for grouping

The `prefix` key groups related routes under a common path. For example, to build an API:

```erlang
#{prefix => "/api/v1",
  security => false,
  routes => [
             {"/users", fun my_api_controller:list_users/1, #{methods => [get]}},
             {"/users/:id", fun my_api_controller:get_user/1, #{methods => [get]}}
            ]
}
```

These routes become `/api/v1/users` and `/api/v1/users/:id`.

## Environment-based routing

The `routes/1` function receives the environment atom configured in `sys.config` (`dev` or `prod`). You can use pattern matching to add development-only routes:

```erlang
routes(prod) ->
  prod_routes();
routes(dev) ->
  prod_routes() ++ dev_routes().

prod_routes() ->
  [#{prefix => "",
      security => false,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
      }].

dev_routes() ->
  [#{prefix => "",
     security => false,
     routes => [
                {"/dev-tools", fun my_first_nova_dev_controller:index/1, #{methods => [get]}}
               ]
  }].
```

```admonish note
`rebar3 nova routes` shows production routes only. Development-only routes won't appear in the output.
```

## Route parameters

Path segments starting with `:` are captured as bindings:

```erlang
{"/users/:id", fun my_controller:show/1, #{methods => [get]}}
```

In the controller, access bindings from the request map:

```erlang
show(#{bindings := #{<<"id">> := Id}}) ->
    {json, #{id => binary_to_integer(Id)}}.
```

## Inline handlers

For simple responses you can use an anonymous function directly in the route:

```erlang
{"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
```

This is useful for health checks and other trivial endpoints.

---

Next, let's look at [plugins](plugins.md) — the middleware layer that processes requests before and after your controllers.
