# Error Handling

When something goes wrong, you want to show a useful error page instead of a cryptic response. Let's look at how Nova handles errors and how to create custom error pages.

## Nova's default error handling

Nova comes with default handlers for 404 (not found) and 500 (server error) responses. In development mode, 500 errors show crash details. In production they return a bare status code.

## Status code routes

Nova lets you register custom handlers for specific HTTP status codes directly in your router. Use a status code integer instead of a path:

```erlang
routes(_Environment) ->
  [
    #{routes => [
        {404, fun blog_error_controller:not_found/1, #{}},
        {500, fun blog_error_controller:server_error/1, #{}}
     ]},

    #{prefix => "",
      security => false,
      routes => [
                 {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
    }
  ].
```

Your status code handlers override Nova's defaults because your routes are compiled after Nova's built-in routes.

## Creating an error controller

Create `src/controllers/blog_error_controller.erl`:

```erlang
-module(blog_error_controller).
-export([
         not_found/1,
         server_error/1
        ]).

not_found(_Req) ->
    {ok, [{title, <<"404 - Not Found">>},
          {message, <<"The page you are looking for does not exist.">>}],
     #{view => error_page, status_code => 404}}.

server_error(_Req) ->
    {ok, [{title, <<"500 - Server Error">>},
          {message, <<"Something went wrong. Please try again later.">>}],
     #{view => error_page, status_code => 500}}.
```

The `status_code` option in the return map sets the HTTP status code on the response.

## Error view template

Create `src/views/error_page.dtl`:

```html
<html>
<head><title>{{ title }}</title></head>
<body>
  <h1>{{ title }}</h1>
  <p>{{ message }}</p>
  <a href="/">Go back home</a>
</body>
</html>
```

## JSON error responses

For APIs, return JSON instead of HTML. Check the `Accept` header to decide:

```erlang
not_found(Req) ->
    case cowboy_req:header(<<"accept">>, Req) of
        <<"application/json">> ->
            {json, 404, #{}, #{error => <<"not_found">>,
                               message => <<"Resource not found">>}};
        _ ->
            {ok, [{title, <<"404">>}, {message, <<"Page not found">>}],
             #{view => error_page, status_code => 404}}
    end.
```

## Rendering changeset errors as JSON

When using Kura, changeset validation errors are structured data. A helper function makes it easy to return them as JSON:

```erlang
changeset_errors_to_json(#kura_changeset{errors = Errors}) ->
    maps:from_list([{atom_to_binary(Field), Msg} || {Field, Msg} <- Errors]).
```

Use it in your controllers:

```erlang
create(#{params := Params}) ->
    CS = post:changeset(#{}, Params),
    case blog_repo:insert(CS) of
        {ok, Post} ->
            {json, 201, #{}, post_to_json(Post)};
        {error, #kura_changeset{} = CS1} ->
            {json, 422, #{}, #{errors => changeset_errors_to_json(CS1)}}
    end.
```

This returns errors like `{"errors": {"title": "can't be blank", "email": "has already been taken"}}`.

## Handling controller crashes

When a controller crashes, Nova catches the exception and triggers the 500 handler. The request map passed to your error controller will contain `crash_info`:

```erlang
server_error(#{crash_info := CrashInfo} = _Req) ->
    logger:error("Controller crash: ~p", [CrashInfo]),
    {ok, [{title, <<"500">>},
          {message, <<"Internal server error">>}],
     #{view => error_page, status_code => 500}};
server_error(_Req) ->
    {ok, [{title, <<"500">>},
          {message, <<"Internal server error">>}],
     #{view => error_page, status_code => 500}}.
```

## More status codes

Register handlers for any HTTP status code:

```erlang
#{routes => [
    {400, fun blog_error_controller:bad_request/1, #{}},
    {401, fun blog_error_controller:unauthorized/1, #{}},
    {403, fun blog_error_controller:forbidden/1, #{}},
    {404, fun blog_error_controller:not_found/1, #{}},
    {500, fun blog_error_controller:server_error/1, #{}}
 ]}
```

```erlang
bad_request(_Req) ->
    {json, 400, #{}, #{error => <<"bad_request">>}}.

unauthorized(_Req) ->
    {json, 401, #{}, #{error => <<"unauthorized">>}}.

forbidden(_Req) ->
    {json, 403, #{}, #{error => <<"forbidden">>}}.
```

## Error flow in the pipeline

Here is how errors flow through Nova:

1. **Route not found** — triggers the 404 handler
2. **Security function returns false** — triggers the 401 handler
3. **Controller crashes** — Nova catches the exception, triggers the 500 handler
4. **Plugin returns `{error, Reason}`** — triggers the 500 handler
5. **Controller returns `{status, Code}`** — if a handler is registered for that code, it is used

For each case, Nova looks up your registered status code handler. If none is registered, it falls back to its own default.

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

## Disabling error page rendering

To skip Nova's error page rendering entirely:

```erlang
{nova, [
    {render_error_pages, false}
]}
```

---

With error handling in place, our application is more robust. Next, let's add real-time features with [WebSockets](../production/websockets.md).
