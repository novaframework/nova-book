# API Documentation & Inspection

An API is only as useful as its documentation. `rebar3 nova openapi` builds an [OpenAPI](https://spec.openapis.org/oas/v3.0.3) description from the routes you already have. This chapter generates one for the blog's API, looks honestly at what it contains, and serves it. Then we point the inspection commands from Part I (`routes`, `middleware`, `audit`, `doctor`) and `rebar3 nova config` at the API.

## Generating the spec

```shell
rebar3 nova openapi --output priv/openapi/openapi.json --title "Blog API" --api-version 1.0.0 --prefix /api
```

```
===> Verifying dependencies...
===> Running erlydtl...
===> kura: schemas up to date
===> Analyzing applications...
===> Compiling blog
===> OpenAPI spec written to priv/openapi/openapi.json
```

| Flag | Default | Meaning |
|---|---|---|
| `--output`, `-o` | `priv/assets/openapi.json` | Where to write the spec |
| `--title`, `-t` | the application name | `info.title` |
| `--api-version`, `-v` | `0.1.0` | `info.version` |
| `--prefix`, `-p` | every controller route | Only document paths under this prefix |

The command compiles the project, calls your router and builds the routing table with the project's own Nova, so the spec lists the routes the code actually defines. Static file routes, status-code routes and inline funs such as `/heartbeat` are left out. `--prefix /api` leaves out the HTML pages as well, so the spec is the API and nothing else. The command also reads every `*.json` file under `priv/schemas/`, subdirectories included, into `components.schemas`, named after the file's path (`tag.json` is `tag`, `v1/user.json` would be `v1.user`).

```admonish warning
Keep `--output`. The default, `priv/assets/`, is served by the blog's `/assets/[...]` route, so the spec would be public at `/assets/openapi.json` whether you meant to publish it or not. The blog writes it to `priv/openapi/`, which no route serves as a directory, and publishes that one file below.
```

### What is in it

A route with a `json_schema` in its `extra_state` (the same option `nova_json_schemas` uses, see [the previous chapter](json-api.md#routes)) gets a request body that points at that schema. The responses follow the method: `201` for a `POST`, `204` for a `DELETE`, `200` otherwise, plus `400` when there is a request schema and `404` when the path has a binding. Here is `/api/tags`:

```json
{
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
        "201": {
          "description": "Created"
        },
        "400": {
          "description": "Invalid request body"
        }
      }
    }
  }
}
```

Bindings become path parameters: `/tags/:id` is documented as `/api/tags/{id}` with a required string parameter `id`. The `operationId` is `module.function`, which makes it easy to find the code behind an operation. When one function serves several methods, as `blog_posts_controller:index/1` serves `GET` and `OPTIONS`, the method and path are appended (`blog_posts_controller.index:get:/api/posts`) so every operation keeps a unique id. The `$schema` key of `tag.json` is dropped from the component, since an OpenAPI 3.0 schema object does not allow it.

```admonish note title="What the generator cannot know"
- **Responses carry a status and a description, no body.** The spec does not say that `show` returns a tag, or which errors the security function (401, 403), `blog_require_json_plugin` (415) and the changeset (422) add. `json_schema` describes the request body only.
- **GET routes cannot carry a schema.** Adding `json_schema` to a GET route makes `nova_json_schemas` try to validate a body that is not there, and the request crashes.
```

Treat the generated file as an accurate inventory of the API's routes and request bodies, and add hand-written documentation for the responses where clients need it.

## Serving the spec

The blog publishes the spec at `/api/openapi.json` with a static file route in the tags group shown in [the previous chapter](json-api.md#routes):

```erlang
{"/openapi.json", "openapi/openapi.json", #{mimetype => ~"application/json"}}
```

Like `/robots.txt` in [Routing](../foundations/routing.md), the path is relative to the application's `priv` directory, and `mimetype` sets the `content-type`. The file is served as it was generated:

```shell
curl -i localhost:8080/api/openapi.json
```

```
HTTP/1.1 200 OK
content-length: 6074
content-type: application/json
...

{
  "components": {
    "schemas": {
      "tag": {
...
```

The test `openapi_spec_documents_the_api` in `blog_api_tags_SUITE` fetches it and checks the tags operations and their responses, that the `tag` schema has no `$schema` key, and that nothing outside `/api/` is in it.

The spec is a build artifact checked into the repository. Regenerate it with the command above whenever you change the API routes or schemas, and commit it with the change.

### Swagger UI

The spec is all the blog serves. If you want a browsable UI, download the `swagger-ui-dist` package, copy its files into a directory under `priv/` and write your own page that points at `/api/openapi.json`; do not load Swagger UI from a CDN (see [Security](../production/security.md#no-third-party-scripts)). The blog leaves that out to keep the repository small. Any OpenAPI viewer or client generator can read the served spec directly.

```admonish warning title="rebar3 nova release"
`rebar3 nova release` regenerates the spec and then builds a release with `rebar3 as prod release`. It regenerates with the defaults, though: every controller route, written to `priv/assets/openapi.json`, which the release then serves at `/assets/openapi.json`. Build releases with `rebar3 as prod release` (see [Deployment](../production/deployment.md)) and regenerate the spec yourself with the command above.
```

## Inspecting the API

[Part I](../foundations/routing.md#listing-the-routes) introduced the inspection commands. With the API in place they answer API questions.

### Routes

`rebar3 nova routes` lists every route, sorted by path. The output is trimmed to the `/api` routes: the posts API, the token routes from [Authentication](../auth-sessions/authentication.md#api-tokens), the tags API and the spec:

```
...
any      /api/openapi.json                        nova_file_controller:get_file/1
get      /api/posts                               blog_posts_controller:index/1
post     /api/posts                               blog_posts_controller:create/1
delete   /api/posts/:id                           blog_posts_controller:delete/1
get      /api/posts/:id                           blog_posts_controller:show/1
put      /api/posts/:id                           blog_posts_controller:update/1
post     /api/posts/:id/comments                  blog_posts_controller:create_comment/1
put      /api/posts/:id/tags                      blog_posts_controller:update_tags/1
get      /api/tags                                blog_tags_controller:list/1
post     /api/tags                                blog_tags_controller:create/1
delete   /api/tags/:id                            blog_tags_controller:delete/1
get      /api/tags/:id                            blog_tags_controller:show/1
put      /api/tags/:id                            blog_tags_controller:update/1
delete   /api/tokens                              blog_tokens_controller:delete/1
post     /api/tokens                              blog_tokens_controller:create/1
...
```

A static file route answers every method (`any`).

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
    STATIC /openapi.json -> openapi/openapi.json
```

### Security audit

`rebar3 nova audit` lists every write route without a security function as a warning, and every read route without one as information. The output is trimmed to the API lines:

```
=== Security Audit ===

  Audited routes(dev) of blog. Not covered: static files and error pages, arizona_nova routes (served before nova_router). nova_apps read from ./config/dev_sys.config.src.

  WARNINGS:
    POST /api/tags (blog_tags_controller) has no security
    DELETE /api/tags/{id} (blog_tags_controller) has no security
    PUT /api/tags/{id} (blog_tags_controller) has no security
    POST /api/tokens (blog_tokens_controller) has no security
    ...

  INFO:
    ...
    GET /api/posts (blog_posts_controller) has no security
    GET /api/posts/{id} (blog_posts_controller) has no security
    GET /api/tags (blog_tags_controller) has no security
    GET /api/tags/{id} (blog_tags_controller) has no security
    ...

  Summary: 8 warning(s), 12 info(s)
```

`POST /api/tokens` is open on purpose: it is how a client gets a token, and it is rate-limited. Reading posts and tags is meant to be public. The post writes already need a bearer token (Part III); the tag writes are the gap that remains, closed in [Security](../production/security.md#tag-writes-need-an-admin).

The first line says what the audit did not look at. Static files and status pages have no security function to check, and Arizona's routes never reach Nova's router. `--env prod --config config/prod_sys.config.src` audits the routes the production config would compile (with `${VAR}` references filled in from the environment or their defaults), and `--strict` makes any warning an error, which is useful in CI once the remaining warnings are the intended ones.

### Doctor

`rebar3 nova doctor -v` (see [Your First Nova Application](../foundations/create-app.md#checking-the-project)) compiles the routes and lists each one with its controller, a quick way to see that a new resource is wired up:

```
✅ Routes
    ✓ 42 routes compiled
    ...
    ✓ DELETE /api/tags/:id -> blog_tags_controller:delete/1
    ✓ GET /api/tags/:id -> blog_tags_controller:show/1
    ✓ PUT /api/tags/:id -> blog_tags_controller:update/1
    ✓ GET /api/tags -> blog_tags_controller:list/1
    ✓ POST /api/tags -> blog_tags_controller:create/1
    ...
```

A route whose controller does not export the function is reported as an error.

### Configuration

`rebar3 nova config` prints the Nova settings of the shell config (`config/dev_sys.config.src`), grouped by the application they are read from:

```
=== Nova Configuration ===

  {nova, [...]}
    bootstrap_application     blog
    environment               dev
    cowboy_configuration      #{port => 8080}
    plugins                   [{pre_request,nova_request_plugin,
                                   #{decode_json_body => true,
                                     read_urlencoded_body => true}}]
    use_stacktrace            true
    render_error_pages        true (default)
    use_strict_routing        false (default)
    session_manager           nova_session_ets (default)
    dispatch_backend          persistent_term (default)

  {blog, [...]}
    json_lib                  blog_json
    use_sessions              true (default)
```

`json_lib` and `use_sessions` are listed under `blog`, the bootstrap application, because that is where Nova reads them; the blog sets `{json_lib, blog_json}` (see [Plugins](../foundations/plugins.md)). Keys you did not set show their default. `render_error_pages` comes back in [Error Handling](error-handling.md#turning-error-pages-off).

## Command summary

| Command | Use it to |
|---|---|
| `rebar3 nova gen_resource --name N` | Scaffold a controller and JSON schema and print route group hints |
| `rebar3 nova gen_controller --name N` | Scaffold a controller only |
| `rebar3 nova gen_test --name N` | Scaffold a Common Test suite from the compiled routes |
| `rebar3 nova openapi -o PATH --prefix P` | Write an OpenAPI 3.0.3 spec of the routes under `P` |
| `rebar3 nova routes` | List every route with its handler |
| `rebar3 nova middleware` | Show each group's plugin chain |
| `rebar3 nova audit` | List routes without a security function |
| `rebar3 nova doctor -v` | Check toolchain, config and routes |
| `rebar3 nova config` | Show the Nova settings by application |

---

Next, [error handling](error-handling.md): where errors come from, and how to answer each one in a form an API client can use.
