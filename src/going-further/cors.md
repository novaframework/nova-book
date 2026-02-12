# CORS

If your API is consumed by a frontend on a different domain, the browser blocks requests unless your server sends the right CORS (Cross-Origin Resource Sharing) headers. Nova includes a CORS plugin that handles this.

## Using nova_cors_plugin

Add it to your plugin configuration:

```erlang
{plugins, [
    {pre_request, nova_cors_plugin, #{allow_origins => <<"*">>}},
    {pre_request, nova_request_plugin, #{decode_json_body => true}}
]}
```

```admonish warning
Using `<<"*">>` allows requests from any origin. For production, restrict this to your frontend's domain:

~~~erlang
{pre_request, nova_cors_plugin, #{allow_origins => <<"https://myapp.com">>}}
~~~
```

## What the plugin does

1. **Adds CORS headers** to every response:
   - `Access-Control-Allow-Origin` — set to your `allow_origins` value
   - `Access-Control-Allow-Headers` — set to `*`
   - `Access-Control-Allow-Methods` — set to `*`

2. **Handles preflight requests** — when an `OPTIONS` request comes in, the plugin responds with 200 and the CORS headers, then stops the pipeline. The request never reaches your controller.

## Per-route CORS

Apply CORS only to API routes:

```erlang
routes(_Environment) ->
  [
    %% API routes with CORS
    #{prefix => "/api",
      plugins => [
          {pre_request, nova_cors_plugin, #{allow_origins => <<"https://myapp.com">>}},
          {pre_request, nova_request_plugin, #{decode_json_body => true}}
      ],
      routes => [
                 {"/users", fun my_first_nova_api_controller:index/1, #{methods => [get]}},
                 {"/users", fun my_first_nova_api_controller:create/1, #{methods => [post]}},
                 {"/users/:id", fun my_first_nova_api_controller:show/1, #{methods => [get]}},
                 {"/users/:id", fun my_first_nova_api_controller:update/1, #{methods => [put]}},
                 {"/users/:id", fun my_first_nova_api_controller:delete/1, #{methods => [delete]}}
                ]
    },

    %% HTML routes without CORS
    #{prefix => "",
      plugins => [
          {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
      ],
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get, post]}}
                ]
    }
  ].
```

## Writing a custom CORS plugin

The built-in plugin hardcodes `Allow-Headers` and `Allow-Methods` to `*`. For more control:

```erlang
-module(my_first_nova_cors_plugin).
-behaviour(nova_plugin).

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

pre_request(Req, _Env, Options, State) ->
    Origins = maps:get(allow_origins, Options, <<"*">>),
    Methods = maps:get(allow_methods, Options, <<"GET, POST, PUT, DELETE, OPTIONS">>),
    Headers = maps:get(allow_headers, Options, <<"Content-Type, Authorization">>),
    MaxAge = maps:get(max_age, Options, <<"86400">>),

    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origins, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Methods, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, Headers, Req2),
    Req4 = cowboy_req:set_resp_header(<<"access-control-max-age">>, MaxAge, Req3),

    Req5 = case maps:get(allow_credentials, Options, false) of
               true ->
                   cowboy_req:set_resp_header(
                       <<"access-control-allow-credentials">>, <<"true">>, Req4);
               false ->
                   Req4
           end,

    case cowboy_req:method(Req5) of
        <<"OPTIONS">> ->
            Reply = cowboy_req:reply(204, Req5),
            {stop, Reply, State};
        _ ->
            {ok, Req5, State}
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    {<<"my_first_nova_cors_plugin">>,
     <<"1.0.0">>,
     <<"My First Nova">>,
     <<"Configurable CORS plugin">>,
     [allow_origins, allow_methods, allow_headers, max_age, allow_credentials]}.
```

Configure with all options:

```erlang
{pre_request, my_first_nova_cors_plugin, #{
    allow_origins => <<"https://myapp.com">>,
    allow_methods => <<"GET, POST, PUT, DELETE">>,
    allow_headers => <<"Content-Type, Authorization, X-Request-ID">>,
    max_age => <<"3600">>,
    allow_credentials => true
}}
```

## Testing CORS

Verify headers with curl:

```shell
# Check preflight response
curl -v -X OPTIONS localhost:8080/api/users \
  -H "Origin: https://myapp.com" \
  -H "Access-Control-Request-Method: POST"

# Check actual response headers
curl -v localhost:8080/api/users \
  -H "Origin: https://myapp.com"
```

You should see the `Access-Control-Allow-Origin` header in the response.

---

For the final chapter, let's add observability with [OpenTelemetry](opentelemetry.md).
