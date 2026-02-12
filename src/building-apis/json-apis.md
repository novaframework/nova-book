# JSON APIs

So far we have been rendering HTML views with ErlyDTL templates. Now let's build a REST API that returns JSON.

## The JSON return tuple

Instead of returning `{ok, Variables}` (which renders a template), return `{json, Data}` and Nova encodes it as JSON with the correct `Content-Type` header.

You can scaffold an API controller quickly with the code generator:

```shell
rebar3 nova gen_resource --name users --actions index,show,create
```

This generates a controller with stub functions, a JSON schema, and prints route definitions. See [Code Generators](../developer-tools/code-generators.md) for the full details.

Let's write the controller by hand so we can see what each part does. Create `src/controllers/my_first_nova_api_controller.erl`:

```erlang
-module(my_first_nova_api_controller).
-export([
         index/1,
         show/1,
         create/1
        ]).

index(_Req) ->
    Users = [
        #{id => 1, name => <<"Alice">>, email => <<"alice@example.com">>},
        #{id => 2, name => <<"Bob">>, email => <<"bob@example.com">>}
    ],
    {json, #{users => Users}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    {json, #{id => binary_to_integer(Id), name => <<"Alice">>, email => <<"alice@example.com">>}};
show(_Req) ->
    {status, 400, #{}, #{error => <<"missing id">>}}.

create(#{params := #{<<"name">> := Name, <<"email">> := Email}}) ->
    {json, 201, #{}, #{id => 3, name => Name, email => Email}};
create(_Req) ->
    {status, 422, #{}, #{error => <<"name and email required">>}}.
```

Here is what each function does:

- **`index/1`** — returns a list of users as JSON with status 200
- **`show/1`** — uses `bindings` from a route with a path parameter like `/users/:id`
- **`create/1`** — reads `params` from the decoded request body and returns `{json, 201, #{}, Data}` to set a custom status code

## Adding the routes

Use a prefix to group API routes:

```erlang
#{prefix => "/api",
  security => false,
  routes => [
             {"/users", fun my_first_nova_api_controller:index/1, #{methods => [get]}},
             {"/users/:id", fun my_first_nova_api_controller:show/1, #{methods => [get]}},
             {"/users", fun my_first_nova_api_controller:create/1, #{methods => [post]}}
            ]
}
```

The full paths become `/api/users` and `/api/users/:id`.

## Configuring JSON decoding

For POST endpoints, Nova needs to decode incoming JSON bodies. Update the plugin configuration in `dev_sys.config.src`:

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{
        decode_json_body => true,
        read_urlencoded_body => true
    }}
]}
```

With `decode_json_body => true`, the plugin decodes incoming JSON and puts it in the `params` key of the request map.

## JSON library

Nova uses `thoas` as the default JSON encoder/decoder. To use a different library:

```erlang
{my_first_nova, [
    {json_lib, jsx}
]}
```

The library module needs to export `encode/1` and `decode/1`.

## Testing with curl

Start the node and test:

```shell
# Get all users
curl -s localhost:8080/api/users | python3 -m json.tool

# Get a single user
curl -s localhost:8080/api/users/1 | python3 -m json.tool

# Create a user
curl -s -X POST localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"name": "Charlie", "email": "charlie@example.com"}' | python3 -m json.tool
```

## Response format reference

```erlang
%% Simple JSON response (status 200)
{json, #{key => value}}

%% JSON with custom status code and optional headers
{json, StatusCode, Headers, Body}

%% Status response (also encodes maps as JSON)
{status, StatusCode}
{status, StatusCode, Headers, Body}

%% Redirect
{redirect, "/some/path"}
```

Adding custom headers:

```erlang
index(_Req) ->
    {json, 200, #{<<"x-request-id">> => <<"abc123">>}, #{users => []}}.
```

---

JSON APIs are great for structured data, but sometimes you need real-time communication. Let's look at [WebSockets](websockets.md).
