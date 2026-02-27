# Controllers & Responses

Every Nova controller function receives a request map and returns a tuple that tells Nova how to respond. This chapter covers all the response types and patterns you'll use.

## The request map

When Nova calls your controller function, it passes a map containing everything about the request:

```erlang
show(#{method := <<"GET">>,
       bindings := #{<<"id">> := Id},
       auth_data := #{username := User}} = Req) ->
    ...
```

Key fields available in the request map:

| Key | Description |
|---|---|
| `method` | HTTP method as uppercase binary (`<<"GET">>`, `<<"POST">>`, etc.) |
| `bindings` | Path parameters (e.g. `#{<<"id">> => <<"42">>}`) |
| `auth_data` | Data from the security function (if any) |
| `json` | Decoded JSON body (if `nova_request_plugin` is configured) |
| `params` | Decoded form params (if `nova_request_plugin` is configured) |
| `parsed_qs` | Parsed query string (if `nova_request_plugin` is configured) |
| `csrf_token` | CSRF token (if `nova_csrf_plugin` is enabled) |
| `correlation_id` | Request correlation ID (if `nova_correlation_plugin` is enabled) |

You can also access raw Cowboy request data using `cowboy_req` functions on the request map.

## Response tuples

### JSON responses

```erlang
%% 200 OK with JSON body (201 for POST requests)
{json, #{message => <<"hello">>}}

%% Custom status, headers, and body
{json, 201, #{<<"location">> => <<"/api/posts/1">>}, #{id => 1, title => <<"New Post">>}}
```

### Template rendering

```erlang
%% Render the default template (derived from module name)
{ok, [{title, <<"My Blog">>}]}

%% Render a specific template
{ok, [{error, <<"Invalid">>}], #{view => login}}

%% Render with custom status
{ok, [{error, <<"Not Found">>}], #{view => error_page, status_code => 404}}
```

### Status codes

```erlang
%% Bare status code
{status, 204}

%% Status with headers and body
{status, 404, #{}, #{error => <<"not found">>}}
```

### Redirects

```erlang
%% 302 redirect
{redirect, "/login"}

%% Redirect with modified request (e.g. after deleting a session cookie)
{redirect, "/login", Req1}
```

### File responses

```erlang
{sendfile, 200, #{}, {0, FileSize, "/path/to/file.pdf"}, <<"application/pdf">>}
```

## Complete reference

| Return | Description |
|---|---|
| `{ok, Variables}` | Render the default template with variables |
| `{ok, Variables, #{view => Name}}` | Render a specific template |
| `{ok, Variables, #{view => Name, status_code => Code}}` | Render template with custom status |
| `{json, Data}` | JSON response (status 200, or 201 for POST) |
| `{json, StatusCode, Headers, Body}` | JSON response with custom status and headers |
| `{status, StatusCode}` | Bare status code response |
| `{status, StatusCode, Headers, Body}` | Status with headers and body |
| `{redirect, Path}` | HTTP 302 redirect |
| `{redirect, Path, Req}` | Redirect with modified request |
| `{sendfile, StatusCode, Headers, {Offset, Length, Path}, MimeType}` | Send a file |

## Handling multiple HTTP methods

A single controller function can handle different methods using pattern matching:

```erlang
login(#{method := <<"GET">>}) ->
    {ok, [], #{view => login}};
login(#{method := <<"POST">>, params := Params} = Req) ->
    %% process login form
    {redirect, "/"}.
```

Or use separate functions for clarity:

```erlang
%% In the router
{"/login", fun blog_main_controller:login/1, #{methods => [get]}},
{"/login", fun blog_main_controller:login_post/1, #{methods => [post]}}
```

## Custom response handlers

Nova uses a handler registry that maps return tuple atoms to handler functions:

```erlang
nova_handlers:register_handler(xml, fun my_xml_handler:handle/3).
```

Then return from controllers:

```erlang
my_action(_Req) ->
    {xml, <<"<user><name>Alice</name></user>">>}.
```

The handler function receives `(ReturnTuple, CallbackFun, Req)` and must return `{ok, Req2}`.

## Fallback controllers

If a controller returns an unrecognized value, Nova can delegate to a fallback controller:

```erlang
-module(blog_posts_controller).
-fallback_controller(blog_error_controller).

index(_Req) ->
    case do_something() of
        {ok, Data} -> {json, Data};
        unexpected_value -> unexpected_value  %% Goes to fallback
    end.
```

The fallback module needs `resolve/2`:

```erlang
resolve(Req, InvalidReturn) ->
    logger:warning("Unexpected controller return: ~p", [InvalidReturn]),
    {status, 500, #{}, #{error => <<"internal server error">>}}.
```

---

Next, let's look at [plugins and middleware](plugins.md) — the layer that processes requests before and after your controllers.
