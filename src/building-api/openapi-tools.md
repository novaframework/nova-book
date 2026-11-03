# API Documentation & Inspection

An API is only as useful as its documentation. `rebar3 nova openapi` builds an [OpenAPI](https://spec.openapis.org/oas/v3.0.3) description from the routes you already have. This chapter generates one for the blog, looks honestly at what it contains, and serves the API part of it. Then we point the inspection commands from Part I (`routes`, `middleware`, `audit`, `doctor`) and `rebar3 nova config` at the API.

## Generating the spec

```shell
rebar3 nova openapi --output priv/openapi/openapi.json --title "Blog API" --api-version 1.0.0
```

```
===> Verifying dependencies...
===> Running erlydtl...
===> kura: schemas up to date
===> Analyzing applications...
===> Compiling blog
===> OpenAPI spec written to priv/openapi/openapi.json
===> Swagger UI written to priv/openapi/swagger.html
```

| Flag | Default | Meaning |
|---|---|---|
| `--output`, `-o` | `priv/assets/openapi.json` | Where to write the spec |
| `--title`, `-t` | the application name | `info.title` |
| `--api-version`, `-v` | `0.1.0` | `info.version` |

The command compiles the project, calls your router and builds the routing table from it, so the spec lists the routes the code actually defines. (The plugin brings its own copy of Nova, 0.15.3 in rebar3_nova 0.13.4, to do this; for ordinary route groups the result is the same.) It also reads every `*.json` file in `priv/schemas/` into `components.schemas`, named after the file.

```admonish warning
Do not keep the default output directory. `priv/assets/` is served by the blog's `/assets/[...]` route, and the command writes a `swagger.html` next to the spec that loads Swagger UI's JavaScript and CSS from `unpkg.com`. The blog writes the spec to `priv/openapi/`, which is not served, and lists both `swagger.html` locations in `.gitignore`.
```

### What is in it

A route with a `json_schema` in its `extra_state` (the same option `nova_json_schemas` uses, see [the previous chapter](json-api.md#routes)) gets a request body and a response that point at that schema. Here is `/api/tags`, formatted:

```json
{
  "paths": {
    "/api/tags": {
      "get": {
        "operationId": "blog_tags_controller.list",
        "responses": {
          "200": {
            "description": "Successful response"
          }
        }
      },
      "post": {
        "operationId": "blog_tags_controller.create",
        "requestBody": {
          "content": {
            "application/json": {
              "schema": {
                "$ref": "#/components/schemas/tag"
              }
            }
          },
          "required": true
        },
        "responses": {
          "200": {
            "content": {
              "application/json": {
                "schema": {
                  "$ref": "#/components/schemas/tag"
                }
              }
            },
            "description": "Successful response"
          }
        }
      }
    }
  }
}
```

Bindings become path parameters: `/tags/:id` is documented as `/api/tags/{id}` with a required string parameter `id`. The `operationId` is `module.function`, which makes it easy to find the code behind an operation.

```admonish bug title="Limits of the generator (rebar3_nova 0.13.x)"
- **Every route is documented**, not only the API: the HTML pages, `/heartbeat` (as `blog_router.-routes/1-fun-0-`) and `/robots.txt` with GET, POST, PUT, DELETE and PATCH, because static file routes are not skipped. Wildcard directory routes and status-code routes are left out.
- **Every operation answers `200 Successful response`.** The 201 from `create`, the 204 from `delete` and all error responses are missing.
- **One schema per route** serves as both request body and response, although the response has `id` and `inserted_at` and the request must not.
- **Component schemas keep their `$schema` key**, which an OpenAPI 3.0 schema object does not allow; strict validators complain.
- **GET routes cannot carry a schema.** Adding `json_schema` to a GET route for documentation makes `nova_json_schemas` try to validate a body that is not there, and the request crashes.
```

Treat the generated file as an accurate inventory of routes and request bodies, and as a starting point for hand-written documentation.

## Serving the spec

The blog publishes the API part of the spec at `/api/openapi.json`. `src/controllers/blog_api_docs_controller.erl` reads the generated file and keeps only the paths under `/api/`:

```erlang
-module(blog_api_docs_controller).

-export([openapi/1]).

openapi(_Req) ->
    {ok, Spec} = file:read_file(filename:join(code:priv_dir(blog), "openapi/openapi.json")),
    #{~"paths" := Paths} = Decoded = json:decode(Spec),
    {json, Decoded#{~"paths" := #{P => Ops || P := Ops <- Paths, is_api_path(P)}}}.

is_api_path(<<"/api/", _/binary>>) -> true;
is_api_path(_) -> false.
```

The route is in the tags group shown in [the previous chapter](json-api.md#routes):

```erlang
{"/openapi.json", fun blog_api_docs_controller:openapi/1, #{methods => [get]}}
```

```shell
curl -i localhost:8080/api/openapi.json
```

```
HTTP/1.1 200 OK
content-length: 2983
content-type: application/json
...

{"components":{"schemas":{"tag":{"$schema":"http://json-schema.org/draft-06/schema#",...}}},"info":{"title":"Blog API","version":"1.0.0"},"openapi":"3.0.3","paths":{"/api/openapi.json":{...},"/api/posts":{...},...}}
```

The test `openapi_spec_lists_api_routes` in `blog_api_tags_SUITE` checks that the tags operations are there and nothing outside `/api/` is.

The spec is a build artifact checked into the repository. Regenerate it with the command above whenever you change the API routes or schemas, and commit it with the change.

### Swagger UI

The generated `swagger.html` would give you an interactive page, but it loads its code from a CDN, which the blog does not do (see [Security](../production/security.md)). If you want a browsable UI, download the `swagger-ui-dist` package, copy its files into a directory under `priv/` and write your own page that points at `/api/openapi.json`. The blog leaves that out to keep the repository small. Any OpenAPI viewer or client generator can read the served spec directly.

```admonish warning title="rebar3 nova release"
`rebar3 nova release` is meant to regenerate the spec and build a release in one go. In 0.13.4 it regenerates into the default `priv/assets/` (CDN page included) and then crashes inside rebar3 3.27 with `{case_clause,["prod"]}`. Build releases with `rebar3 as prod release` (see [Deployment](../production/deployment.md)) and regenerate the spec yourself.
```

## Inspecting the API

[Part I](../foundations/routing.md#listing-the-routes) introduced the inspection commands. With the API in place they answer API questions.

### Routes

`rebar3 nova routes` shows every API group under the shared `/api` prefix: the posts API, the token routes from [Authentication](../auth-sessions/authentication.md#api-tokens) and the tags API (bindings are printed without the colon, so `/id` is `/:id`). The output is trimmed to the `/api` subtree:

```
     ├─  /api
         ├─  GET /openapi.json (blog, blog_api_docs_controller:openapi/1)
         ├─  POST /tags (blog, blog_tags_controller:create/1)
         ├─  GET /tags (blog, blog_tags_controller:list/1)
             └─  DELETE /id (blog, blog_tags_controller:delete/1)
             └─  PUT /id (blog, blog_tags_controller:update/1)
             └─  GET /id (blog, blog_tags_controller:show/1)
         ├─  POST /tokens (blog, blog_tokens_controller:create/1)
         ├─  DELETE /tokens (blog, blog_tokens_controller:delete/1)
         └─  POST /posts (blog, blog_posts_controller:create/1)
         └─  GET /posts (blog, blog_posts_controller:index/1)
             └─  DELETE /id (blog, blog_posts_controller:delete/1)
             └─  PUT /id (blog, blog_posts_controller:update/1)
             └─  GET /id (blog, blog_posts_controller:show/1)
                 ├─  PUT /tags (blog, blog_posts_controller:update_tags/1)
                 └─  POST /comments (blog, blog_posts_controller:create_comment/1)
```

### Plugin chains

`rebar3 nova middleware` confirms which plugins and which security function each group runs. The posts groups inherit the global request plugin, the token route has its own rate limiter, and the tags group has its own chain. The output is trimmed to the four `/api` groups:

```
  Group: prefix=/api  security=false
  Plugins:
    (inherits global)
    pre_request: nova_request_plugin #{decode_json_body => true,
                                       read_urlencoded_body => true}
  Routes:
    GET /posts -> blog_posts_controller:index
    GET /posts/:id -> blog_posts_controller:show

  Group: prefix=/api  security=fun blog_security:require_bearer/1
  Plugins:
    (inherits global)
    pre_request: nova_request_plugin #{decode_json_body => true,
                                       read_urlencoded_body => true}
  Routes:
    POST /posts -> blog_posts_controller:create
    PUT /posts/:id -> blog_posts_controller:update
    DELETE /posts/:id -> blog_posts_controller:delete
    POST /posts/:id/comments -> blog_posts_controller:create_comment
    PUT /posts/:id/tags -> blog_posts_controller:update_tags
    DELETE /tokens -> blog_tokens_controller:delete

  Group: prefix=/api  security=false
  Plugins:
    pre_request: nova_request_plugin #{decode_json_body => true}
    pre_request: blog_rate_limit #{limit => 5,algorithm => sliding_window,
                                   key_fun => fun blog_rate_limit:client_ip/1,
                                   limiter => blog_login_limiter,
                                   window => 60000}
  Routes:
    POST /tokens -> blog_tokens_controller:create

  Group: prefix=/api  security=false
  Plugins:
    pre_request: nova_request_plugin #{decode_json_body => true}
    pre_request: blog_require_json_plugin #{}
    pre_request: nova_json_schemas #{render_errors => false}
  Routes:
    GET /tags -> blog_tags_controller:list
    GET /tags/:id -> blog_tags_controller:show
    POST /tags -> blog_tags_controller:create
    PUT /tags/:id -> blog_tags_controller:update
    DELETE /tags/:id -> blog_tags_controller:delete
    GET /openapi.json -> blog_api_docs_controller:openapi
```

The command still stops with an error when it reaches the last group, as described in [Plugins](../foundations/plugins.md); everything before it prints first.

### Security audit

`rebar3 nova audit` lists every write route without a security function. The output is trimmed to the API lines:

```
  WARNINGS:
    ...
    POST /api/tokens (blog_tokens_controller) has no security
    PUT /api/tags/{id} (blog_tags_controller) has no security
    DELETE /api/tags/{id} (blog_tags_controller) has no security
    POST /api/tags (blog_tags_controller) has no security
    ...
  INFO:
    ...
    GET /api/posts/{id} (blog_posts_controller) has no security
    GET /api/posts (blog_posts_controller) has no security
    GET /api/tags/{id} (blog_tags_controller) has no security
    GET /api/tags (blog_tags_controller) has no security
    GET /api/openapi.json (blog_api_docs_controller) has no security
    ...
  Summary: 17 warning(s), 14 info(s)
```

`POST /api/tokens` is open on purpose: it is how a client gets a token, and it is rate-limited. Reading posts and tags is meant to be public. The post writes already need a bearer token (Part III); the tag writes are the gap that remains, closed in [Security](../production/security.md#tag-writes-need-an-admin).

### Doctor

`rebar3 nova doctor -v` (see [Your First Nova Application](../foundations/create-app.md#checking-the-project)) compiles the routes and lists each one with its controller, a quick way to see that a new resource is wired up:

```
✅ Routes
    ✓ 27 routes compiled
    ...
    ✓ POST /api/tags -> blog_tags_controller (callback fun)
    ✓ GET /api/tags -> blog_tags_controller (callback fun)
    ✓ DELETE /api/tags/{id} -> blog_tags_controller (callback fun)
    ✓ PUT /api/tags/{id} -> blog_tags_controller (callback fun)
    ✓ GET /api/tags/{id} -> blog_tags_controller (callback fun)
    ...
```

### Configuration

`rebar3 nova config` prints the `nova` section of the shell config (`config/dev_sys.config.src`):

```
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

```admonish note
The command shows a fixed list of seven keys and reads them only from the `nova` section. `json_lib thoas (default)` is therefore wrong for the blog: Nova reads `json_lib` from the bootstrap application's environment, where the blog sets `{json_lib, blog_json}` (see [Plugins](../foundations/plugins.md)). Options such as `render_error_pages` ([Error Handling](error-handling.md#turning-error-pages-off)) are not shown at all.
```

## Command summary

| Command | Use it to |
|---|---|
| `rebar3 nova gen_resource --name N` | Scaffold a controller and JSON schema and print route hints |
| `rebar3 nova gen_controller --name N` | Scaffold a controller only |
| `rebar3 nova gen_test --name N` | Scaffold a Common Test suite |
| `rebar3 nova openapi -o PATH` | Write an OpenAPI 3.0.3 spec of all routes |
| `rebar3 nova routes` | Show the routing tree |
| `rebar3 nova middleware` | Show each group's plugin chain |
| `rebar3 nova audit` | List routes without a security function |
| `rebar3 nova doctor -v` | Check toolchain, config and routes |
| `rebar3 nova config` | Show the `nova` config keys |

---

Next, [error handling](error-handling.md): where errors come from, and how to answer each one in a form an API client can use.
