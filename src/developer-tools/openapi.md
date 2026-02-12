# OpenAPI & API Documentation

The `rebar3_nova` plugin can generate an OpenAPI 3.0.3 specification from your routes and JSON schemas. It also produces a Swagger UI page so you can browse and test your API from a browser.

## Prerequisites

For the OpenAPI generator to produce schema definitions, you need JSON schema files in `priv/schemas/`. If you used `nova gen_resource` (see [Code Generators](code-generators.md)) these were created for you. Otherwise create them by hand:

```shell
mkdir -p priv/schemas
```

`priv/schemas/user.json`:
```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "id": { "type": "integer", "description": "Unique identifier" },
    "name": { "type": "string", "description": "User's full name" },
    "email": { "type": "string", "format": "email", "description": "Email address" }
  },
  "required": ["name", "email"]
}
```

`priv/schemas/product.json`:
```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "id": { "type": "integer", "description": "Unique identifier" },
    "name": { "type": "string", "description": "Product name" },
    "price": { "type": "number", "description": "Price in cents" }
  },
  "required": ["name", "price"]
}
```

## Generating the spec

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
    --title "My First Nova API" \
    --api-version 1.0.0
```

| Flag | Default | Description |
|------|---------|-------------|
| `--output` | `openapi.json` | Output file path |
| `--title` | app name | API title in the spec |
| `--api-version` | `0.1.0` | API version string |

## What gets generated

The generator inspects every route registered with Nova. For each route it creates a path entry with the correct HTTP method, operation ID, path parameters, and response schema. It skips static file handlers and error controllers.

A snippet from a generated spec:

```json
{
  "openapi": "3.0.3",
  "info": {
    "title": "My First Nova API",
    "version": "1.0.0"
  },
  "paths": {
    "/api/users": {
      "get": {
        "operationId": "my_first_nova_api_controller.index",
        "responses": {
          "200": {
            "description": "OK",
            "content": {
              "application/json": {
                "schema": { "$ref": "#/components/schemas/user" }
              }
            }
          }
        }
      },
      "post": {
        "operationId": "my_first_nova_api_controller.create",
        "requestBody": {
          "content": {
            "application/json": {
              "schema": { "$ref": "#/components/schemas/user" }
            }
          }
        },
        "responses": {
          "201": { "description": "Created" }
        }
      }
    },
    "/api/products": {
      "get": {
        "operationId": "my_first_nova_products_controller.list",
        "responses": {
          "200": {
            "description": "OK",
            "content": {
              "application/json": {
                "schema": { "$ref": "#/components/schemas/product" }
              }
            }
          }
        }
      }
    }
  },
  "components": {
    "schemas": {
      "user": { "..." : "loaded from priv/schemas/user.json" },
      "product": { "..." : "loaded from priv/schemas/product.json" }
    }
  }
}
```

## Swagger UI

The generated `swagger.html` loads the Swagger UI from a CDN and points it at your `openapi.json`. If you place both files in `priv/assets/`, you can serve them through Nova by adding a static route:

```erlang
{"/docs/[...]", cowboy_static, {priv_dir, my_first_nova, "assets"}}
```

Then navigate to `http://localhost:8080/docs/swagger.html` to browse your API interactively.

## Auto-generating on release

The `nova release` command automatically regenerates the OpenAPI spec before building a release. If you have a `priv/schemas/` directory, it runs the OpenAPI generator targeting `priv/assets/openapi.json` before calling the standard release build:

```shell
rebar3 nova release
===> Generated priv/assets/openapi.json
===> Generated priv/assets/swagger.html
===> Release successfully assembled: _build/prod/rel/my_first_nova
```

You can also specify a release profile:

```shell
rebar3 nova release --profile staging
```

This means your deployed application always has up-to-date API documentation bundled in.

## Full workflow

From scratch:

```shell
# Generate a resource with schema
rebar3 nova gen_resource --name products

# Edit the schema to match your data model
vi priv/schemas/product.json

# Implement the controller
vi src/controllers/my_first_nova_products_controller.erl

# Add routes to your router
vi src/my_first_nova_router.erl

# Generate the OpenAPI spec
rebar3 nova openapi --output priv/assets/openapi.json \
    --title "My API" --api-version 1.0.0

# Start the dev server and browse the docs
rebar3 nova serve
# Open http://localhost:8080/docs/swagger.html
```

---

Next, let's look at the [inspection and audit tools](inspection-tools.md) that help you understand and verify your application's configuration.
