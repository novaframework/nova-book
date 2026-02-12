# Code Generators

The `rebar3_nova` plugin includes generators that scaffold controllers, resources, and test suites. Instead of writing boilerplate by hand, run a single command and get a working starting point.

## Generate a controller

The `nova gen_controller` command creates a controller module with stub action functions:

```shell
rebar3 nova gen_controller --name products
===> Writing src/controllers/my_first_nova_products_controller.erl
```

By default it generates five actions: `list`, `show`, `create`, `update`, and `delete`. Pick specific actions with the `--actions` flag:

```shell
rebar3 nova gen_controller --name products --actions list,show
```

The generated controller:

```erlang
-module(my_first_nova_products_controller).
-export([
         list/1,
         show/1,
         create/1,
         update/1,
         delete/1
        ]).

list(_Req) ->
    {json, #{<<"message">> => <<"TODO">>}}.

show(_Req) ->
    {json, #{<<"message">> => <<"TODO">>}}.

create(_Req) ->
    {status, 201, #{}, #{<<"message">> => <<"TODO">>}}.

update(_Req) ->
    {json, #{<<"message">> => <<"TODO">>}}.

delete(_Req) ->
    {status, 204}.
```

Every action returns a valid Nova response tuple so you can compile and run immediately. Replace the `TODO` values with your actual logic.

## Generate a full resource

The `nova gen_resource` command is the most powerful generator. It creates a controller, a JSON schema, and prints route definitions you can paste into your router:

```shell
rebar3 nova gen_resource --name products
===> Writing src/controllers/my_first_nova_products_controller.erl
===> Writing priv/schemas/product.json

Add these routes to your router:

  {<<"/products">>, {my_first_nova_products_controller, list}, #{methods => [get]}}
  {<<"/products/:id">>, {my_first_nova_products_controller, show}, #{methods => [get]}}
  {<<"/products">>, {my_first_nova_products_controller, create}, #{methods => [post]}}
  {<<"/products/:id">>, {my_first_nova_products_controller, update}, #{methods => [put]}}
  {<<"/products/:id">>, {my_first_nova_products_controller, delete}, #{methods => [delete]}}
```

The generated JSON schema in `priv/schemas/product.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "id": { "type": "integer" },
    "name": { "type": "string" }
  },
  "required": ["id", "name"]
}
```

Edit this schema to match your data model. It will be picked up by the [OpenAPI generator](openapi.md) to produce API documentation automatically.

## Generate a test suite

The `nova gen_test` command generates a Common Test suite with test cases for each CRUD action:

```shell
rebar3 nova gen_test --name products
===> Writing test/my_first_nova_products_controller_SUITE.erl
```

The generated suite:

```erlang
-module(my_first_nova_products_controller_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_list/1, test_show/1, test_create/1, test_update/1, test_delete/1]).

all() ->
    [test_list, test_show, test_create, test_update, test_delete].

init_per_suite(Config) ->
    application:ensure_all_started(my_first_nova),
    Config.

end_per_suite(_Config) ->
    ok.

test_list(_Config) ->
    {ok, {{_, 200, _}, _, _Body}} =
        httpc:request(get, {"http://localhost:8080/products", []}, [], []).

test_show(_Config) ->
    {ok, {{_, 200, _}, _, _Body}} =
        httpc:request(get, {"http://localhost:8080/products/1", []}, [], []).

test_create(_Config) ->
    {ok, {{_, 201, _}, _, _Body}} =
        httpc:request(post, {"http://localhost:8080/products", [],
                             "application/json", "{}"}, [], []).

test_update(_Config) ->
    {ok, {{_, 200, _}, _, _Body}} =
        httpc:request(put, {"http://localhost:8080/products/1", [],
                            "application/json", "{}"}, [], []).

test_delete(_Config) ->
    {ok, {{_, 204, _}, _, _Body}} =
        httpc:request(delete, {"http://localhost:8080/products/1", []}, [], []).
```

Update the request bodies and assertions as you flesh out the controller logic.

## Typical workflow

Adding a new resource to your API:

```shell
# 1. Generate the resource (controller + schema + route hints)
rebar3 nova gen_resource --name products

# 2. Copy the printed routes into your router

# 3. Edit the JSON schema to match your data model

# 4. Generate a test suite
rebar3 nova gen_test --name products

# 5. Implement the controller logic

# 6. Run the tests
rebar3 ct
```

This saves you from writing boilerplate and gives you a consistent structure across resources.

---

Now let's see how the [OpenAPI generator](openapi.md) uses these schemas to produce full API documentation.
