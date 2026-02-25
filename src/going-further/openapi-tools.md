# OpenAPI, Inspection & Audit

The `rebar3_nova` plugin includes tools for generating API documentation, inspecting your application's configuration, and auditing security. This chapter covers all three.

## OpenAPI documentation

### Prerequisites

For the OpenAPI generator to produce schema definitions, you need JSON schema files in `priv/schemas/`. If you used `nova gen_resource` (see [JSON API with Generators](../building-api/json-api.md)) these were created for you. Otherwise create them by hand:

```shell
mkdir -p priv/schemas
```

`priv/schemas/post.json`:
```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "id": { "type": "integer", "description": "Unique identifier" },
    "title": { "type": "string", "description": "Post title" },
    "body": { "type": "string", "description": "Post body" },
    "status": { "type": "string", "enum": ["draft", "published", "archived"] }
  },
  "required": ["title", "body"]
}
```

### Generating the spec

Run the OpenAPI generator:

```shell
rebar3 nova openapi
===> Generated openapi.json
===> Generated swagger.html
```

This reads your compiled routes and JSON schemas, then produces two files:
- `openapi.json` — the OpenAPI 3.0.3 specification
- `swagger.html` — a standalone Swagger UI page

Customize the output:

```shell
rebar3 nova openapi \
    --output priv/assets/openapi.json \
    --title "Blog API" \
    --api-version 1.0.0
```

| Flag | Default | Description |
|------|---------|-------------|
| `--output` | `openapi.json` | Output file path |
| `--title` | app name | API title in the spec |
| `--api-version` | `0.1.0` | API version string |

### What gets generated

The generator inspects every route registered with Nova. For each route it creates a path entry with the correct HTTP method, operation ID, path parameters, and response schema. It skips static file handlers and error controllers.

A snippet from a generated spec:

```json
{
  "openapi": "3.0.3",
  "info": {
    "title": "Blog API",
    "version": "1.0.0"
  },
  "paths": {
    "/api/posts": {
      "get": {
        "operationId": "blog_posts_controller.list",
        "responses": {
          "200": {
            "description": "OK",
            "content": {
              "application/json": {
                "schema": { "$ref": "#/components/schemas/post" }
              }
            }
          }
        }
      },
      "post": {
        "operationId": "blog_posts_controller.create",
        "requestBody": {
          "content": {
            "application/json": {
              "schema": { "$ref": "#/components/schemas/post" }
            }
          }
        },
        "responses": {
          "201": { "description": "Created" }
        }
      }
    }
  }
}
```

### Swagger UI

The generated `swagger.html` loads the Swagger UI from a CDN and points it at your `openapi.json`. If you place both files in `priv/assets/`, you can serve them through Nova by adding a static route:

```erlang
{"/docs/[...]", cowboy_static, {priv_dir, blog, "assets"}}
```

Then navigate to `http://localhost:8080/docs/swagger.html` to browse your API interactively.

### Auto-generating on release

The `nova release` command automatically regenerates the OpenAPI spec before building a release:

```shell
rebar3 nova release
===> Generated priv/assets/openapi.json
===> Generated priv/assets/swagger.html
===> Release successfully assembled: _build/prod/rel/blog
```

This means your deployed application always has up-to-date API documentation bundled in.

## Inspection tools

### View configuration

The `nova config` command displays all Nova configuration values with their defaults:

```shell
rebar3 nova config
=== Nova Configuration ===

  bootstrap_application     blog
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

### Inspect middleware chains

The `nova middleware` command shows the global and per-route-group plugin chains:

```shell
rebar3 nova middleware
=== Global Plugins ===
  pre_request: nova_request_plugin #{decode_json_body => true,
                                     read_urlencoded_body => true}

=== Route Groups (blog_router) ===

  Group: prefix=  security=false
  Plugins:
    (inherits global)
  Routes:
    GET /login -> blog_main_controller:login
    GET /heartbeat -> (inline fun)

  Group: prefix=/api  security=false
  Plugins:
    (inherits global)
  Routes:
    GET /posts -> blog_posts_controller:list
    POST /posts -> blog_posts_controller:create
    GET /posts/:id -> blog_posts_controller:show
    PUT /posts/:id -> blog_posts_controller:update
    DELETE /posts/:id -> blog_posts_controller:delete
```

### Listing routes

The `nova routes` command displays the compiled routing tree:

```shell
rebar3 nova routes
Host: '_'
     ├─  /api
     │   ├─  GET /posts (blog, blog_posts_controller:list/1)
     │   ├─  GET /posts/:id (blog, blog_posts_controller:show/1)
     │   ├─  POST /posts (blog, blog_posts_controller:create/1)
     │   ├─  PUT /posts/:id (blog, blog_posts_controller:update/1)
     │   └─  DELETE /posts/:id (blog, blog_posts_controller:delete/1)
     ├─  GET /login (blog, blog_main_controller:login/1)
     └─  GET /heartbeat
```

## Security audit

The `nova audit` command scans your routes and flags potential security issues:

```shell
rebar3 nova audit
=== Security Audit ===

  WARNINGS:
    POST /api/posts (blog_posts_controller) has no security
    PUT /api/posts/:id (blog_posts_controller) has no security
    DELETE /api/posts/:id (blog_posts_controller) has no security

  INFO:
    GET /login (blog_main_controller) has no security
    GET /heartbeat has no security
    GET /api/posts (blog_posts_controller) has no security

  Summary: 3 warning(s), 3 info(s)
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
  security => fun blog_auth:validate_token/1,
  routes => [
    {"/posts", fun blog_posts_controller:list/1, #{methods => [get]}},
    {"/posts/:id", fun blog_posts_controller:show/1, #{methods => [get]}},
    {"/posts", fun blog_posts_controller:create/1, #{methods => [post]}},
    {"/posts/:id", fun blog_posts_controller:update/1, #{methods => [put]}},
    {"/posts/:id", fun blog_posts_controller:delete/1, #{methods => [delete]}}
  ]}
```

## Command summary

| Command | Purpose |
|---------|---------|
| `rebar3 nova openapi` | Generate OpenAPI 3.0.3 spec + Swagger UI |
| `rebar3 nova config` | Show Nova configuration with defaults |
| `rebar3 nova middleware` | Show global and per-group plugin chains |
| `rebar3 nova audit` | Find routes missing security callbacks |
| `rebar3 nova routes` | Display the compiled routing tree |
| `rebar3 nova release` | Build release with auto-generated OpenAPI |

Use `config` to verify settings, `middleware` to trace request processing, `audit` to check security coverage, and `routes` to see the endpoint map.

---

Next, let's learn how to write [custom plugins and handle CORS](plugins-cors.md).
