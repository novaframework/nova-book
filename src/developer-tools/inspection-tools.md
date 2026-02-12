# Inspection & Audit Tools

The `rebar3_nova` plugin includes commands for inspecting your application's configuration, middleware chains, and security posture.

## View configuration

The `nova config` command displays all Nova configuration values with their defaults:

```shell
rebar3 nova config
=== Nova Configuration ===

  bootstrap_application     my_first_nova
  environment               dev
  cowboy_configuration      #{port => 8080}
  plugins                   [{pre_request,nova_request_plugin,
                              #{decode_json_body => true,
                                read_urlencoded_body => true}}]
  json_lib                  thoas (default)
  use_stacktrace            true
  dispatch_backend          persistent_term (default)
```

Keys showing `(default)` are using the built-in default rather than an explicit setting.

| Key | Default | Description |
|-----|---------|-------------|
| `bootstrap_application` | (required) | Main application to bootstrap |
| `environment` | `dev` | Current environment |
| `cowboy_configuration` | `#{port => 8080}` | Cowboy listener settings |
| `plugins` | `[]` | Global middleware plugins |
| `json_lib` | `thoas` | JSON encoding library |
| `use_stacktrace` | `false` | Include stacktraces in error responses |
| `dispatch_backend` | `persistent_term` | Backend for route dispatch storage |

## Inspect middleware chains

The `nova middleware` command shows the global and per-route-group plugin chains:

```shell
rebar3 nova middleware
=== Global Plugins ===
  pre_request: nova_request_plugin #{decode_json_body => true,
                                     read_urlencoded_body => true}

=== Route Groups (my_first_nova_router) ===

  Group: prefix=  security=false
  Plugins:
    (inherits global)
  Routes:
    GET /login -> my_first_nova_main_controller:login
    GET /heartbeat -> (inline fun)
    WS /ws -> my_first_nova_ws_handler

  Group: prefix=/api  security=false
  Plugins:
    (inherits global)
  Routes:
    GET /users -> my_first_nova_api_controller:index
    GET /users/:id -> my_first_nova_api_controller:show
    POST /users -> my_first_nova_api_controller:create
    GET /products -> my_first_nova_products_controller:list
    GET /products/:id -> my_first_nova_products_controller:show
    POST /products -> my_first_nova_products_controller:create
    PUT /products/:id -> my_first_nova_products_controller:update
    DELETE /products/:id -> my_first_nova_products_controller:delete
```

Each route group shows its prefix, security callback, and which plugins apply. Groups without their own plugins inherit the global list.

## Security audit

The `nova audit` command scans your routes and flags potential security issues:

```shell
rebar3 nova audit
=== Security Audit ===

  WARNINGS:
    POST /api/users (my_first_nova_api_controller) has no security
    POST /api/products (my_first_nova_products_controller) has no security
    PUT /api/products/:id (my_first_nova_products_controller) has no security
    DELETE /api/products/:id (my_first_nova_products_controller) has no security

  INFO:
    GET /login (my_first_nova_main_controller) has no security
    GET /heartbeat has no security
    GET /api/users (my_first_nova_api_controller) has no security
    GET /api/products (my_first_nova_products_controller) has no security

  Summary: 4 warning(s), 4 info(s)
```

The audit classifies findings into two levels:

- **WARNINGS** — mutation methods (POST, PUT, DELETE, PATCH) without security, wildcard method handlers
- **INFO** — GET routes without security (common for public endpoints but worth reviewing)

```admonish tip
Run `rebar3 nova audit` before deploying to make sure you haven't left endpoints unprotected by mistake.
```

To fix the warnings, add a security callback to the route group:

```erlang
#{prefix => "/api",
  security => fun my_first_nova_auth:validate_token/1,
  routes => [
    {"/products", fun my_first_nova_products_controller:list/1, #{methods => [get]}},
    {"/products/:id", fun my_first_nova_products_controller:show/1, #{methods => [get]}},
    {"/products", fun my_first_nova_products_controller:create/1, #{methods => [post]}},
    {"/products/:id", fun my_first_nova_products_controller:update/1, #{methods => [put]}},
    {"/products/:id", fun my_first_nova_products_controller:delete/1, #{methods => [delete]}}
  ]}
```

## Listing routes

The `nova routes` command displays the compiled routing tree:

```shell
rebar3 nova routes
Host: '_'
     ├─  /api
     │   ├─  GET /users (my_first_nova, my_first_nova_api_controller:index/1)
     │   ├─  GET /users/:id (my_first_nova, my_first_nova_api_controller:show/1)
     │   ├─  POST /users (my_first_nova, my_first_nova_api_controller:create/1)
     │   ├─  GET /products (my_first_nova, my_first_nova_products_controller:list/1)
     │   ├─  GET /products/:id (my_first_nova, my_first_nova_products_controller:show/1)
     │   ├─  POST /products (my_first_nova, my_first_nova_products_controller:create/1)
     │   ├─  PUT /products/:id (my_first_nova, my_first_nova_products_controller:update/1)
     │   └─  DELETE /products/:id (my_first_nova, my_first_nova_products_controller:delete/1)
     ├─  GET /login (my_first_nova, my_first_nova_main_controller:login/1)
     ├─  POST / (my_first_nova, my_first_nova_main_controller:index/1)
     ├─  GET /heartbeat
     └─  WS /ws (my_first_nova, my_first_nova_ws_handler)
```

## Summary

| Command | Purpose |
|---------|---------|
| `rebar3 nova config` | Show Nova configuration with defaults |
| `rebar3 nova middleware` | Show global and per-group plugin chains |
| `rebar3 nova audit` | Find routes missing security callbacks |
| `rebar3 nova routes` | Display the compiled routing tree |

Use `config` to verify settings, `middleware` to trace request processing, `audit` to check security coverage, and `routes` to see the endpoint map.
