# Controllers & Responses

A Nova controller is a plain module of functions. Each action takes the request map and returns a tuple; the first element of the tuple names a **handler** that turns it into an HTTP response. No behaviour, no callbacks to implement.

## The request map

The argument is Cowboy's request map with a few keys added by Nova and its plugins. Pattern match on what you need:

```erlang
greet(#{bindings := #{~"name" := Name}, parsed_qs := Query}) ->
```

The keys you will reach for most:

| Key | Set by | Contents |
|---|---|---|
| `method` | Cowboy | Uppercase binary: `~"GET"`, `~"POST"`, ... |
| `path`, `qs` | Cowboy | Raw path and query string, both binaries |
| `headers` | Cowboy | Map of lowercase binary header names to values |
| `host`, `port`, `scheme`, `peer` | Cowboy | Where the request came from and went to |
| `bindings` | Router | Path parameters, binary keys and values |
| `extra_state` | Router | The route's `extra_state` option, if any |
| `auth_data` | Security function | Whatever `{true, AuthData}` returned |
| `body` | `nova_request_plugin` | Raw body, when the plugin read it |
| `json` | `nova_request_plugin` | Decoded JSON body (`decode_json_body`) |
| `params` | `nova_request_plugin`, `nova_multipart_plugin` | Form fields (`read_urlencoded_body`, multipart) |
| `parsed_qs` | `nova_request_plugin` | Query string as a map (`parse_qs`) |
| `files` | `nova_multipart_plugin` | Uploaded file parts |
| `csrf_token` | `nova_csrf_plugin` | The session's CSRF token |
| `correlation_id` | `nova_correlation_plugin` | The request's correlation ID |

Everything else (`pid`, `streamid`, `ref`, `resp_headers`, ...) is Cowboy plumbing. The map is a normal `cowboy_req:req()`, so `cowboy_req` functions work on it: the blog's 404 handler uses `cowboy_req:header(~"accept", Req, ~"")` and `cowboy_req:path(Req)`.

```admonish note
The plugin-provided keys only exist when the plugin ran for that route and the request had the right content type. A clause matching `#{json := ...}` does not match a form post. Write one clause per shape you accept and a final catch-all, as `create/1` below does.
```

## Rendering templates

`{ok, Variables}` renders the controller's default template, derived from the module name: `blog_main_controller` renders `blog_main.dtl`. That is how the scaffold's index page works:

```erlang
index(_Req) ->
    {ok, [{message, "Hello world!"}]}.
```

The blog's about page lives in `blog_pages_controller`, which has no template of its own, so it names one:

```erlang
about(_Req) ->
    {ok, [{title, ~"About this blog"}], #{view => blog_about}}.
```

`#{view => blog_about}` renders `src/views/blog_about.dtl`. The options map also takes `status_code` (default 200) and `headers` (replaces the default `content-type: text/html` header). Variables can be a proplist or a map. `{view, ...}` is an alias for `{ok, ...}` with the same forms.

[Views & Templates](../views-templates/templates.md) covers ErlyDTL itself.

## JSON responses

`{json, Data}` encodes `Data` and answers `200`, or `201` for a `POST`:

```erlang
{json, #{greeting => greeting(Name, Query)}}
```

When you need a specific status or headers, use `{json, Status, Headers, Data}`. Creating a greeting answers `201` with a `location` header:

```erlang
create(#{json := #{~"name" := Name}}) ->
    created(Name);
create(#{params := #{~"name" := Name}}) ->
    created(Name);
create(_Req) ->
    {error, missing_name}.

created(Name) ->
    maybe
        ok ?= validate_name(Name),
        {json, 201, #{~"location" => <<"/hello/", Name/binary>>}, #{name => Name}}
    end.
```

The same action accepts a JSON body or a form post, because the `/hello` group's request plugin decodes both (see [Plugins](plugins.md)):

```shell
curl -i -X POST localhost:8080/hello -H 'content-type: application/json' -d '{"name":"ada"}'
```

```
HTTP/1.1 201 Created
...
content-type: application/json
date: Tue, 03 Nov 2026 07:25:21 GMT
location: /hello/ada
server: Cowboy/Nova
...

{"name":"ada"}
```

```shell
curl -i -X POST localhost:8080/hello -d 'name=grace'
```

```
HTTP/1.1 201 Created
...
location: /hello/grace
...

{"name":"grace"}
```

JSON keys can be atoms or binaries when you encode. Keys of a *decoded* body are always binaries, which is why `create/1` matches `~"name"`. Never turn them into atoms.

## Status codes

`{status, Code}` answers with just a status. The scaffold's heartbeat returns `{status, 200}`. The longer forms add headers and a body:

| Return | Response |
|---|---|
| `{status, Code}` | Status only, with an empty body. The router's status-code handlers are *not* used for a returned status (see [Error Handling](../building-api/error-handling.md#which-errors-reach-a-status-handler)). |
| `{status, Code, Headers}` | Same, with extra headers |
| `{status, Code, Headers, Body}` | A binary body is sent as is; a map is encoded as JSON with `content-type: application/json` |

The blog's 404 handler uses the map form: `{status, 404, #{}, #{error => ~"not_found"}}`.

## Redirects

`{redirect, Path}` answers `302 Found` with a `location` header. Path can be a string or a binary:

```erlang
about_moved(_Req) ->
    {redirect, "/about"}.
```

## Sending files

`{sendfile, Status, Headers, {Offset, Length, Path}, MimeType}` streams a file from disk without reading it into memory. The blog offers its about page as Markdown:

```erlang
about_markdown(_Req) ->
    Path = filename:join(code:priv_dir(blog), "pages/about.md"),
    {sendfile, 200, #{}, {0, filelib:file_size(Path), Path}, ~"text/markdown; charset=utf-8"}.
```

```shell
curl -i localhost:8080/about.md
```

```
HTTP/1.1 200 OK
content-length: 85
content-type: text/markdown; charset=utf-8
date: Tue, 03 Nov 2026 07:25:21 GMT
server: Cowboy/Nova
vary: accept-encoding
set-cookie: session_id=beqyG+82R+7bvrvqqGroB0wmqknK2o/faPbDvmPZLPY=

# About this blog

A small blog built with Nova while working through The Nova Book.
```

The path is fixed in code. Never build it from a binding or a query parameter; that is how path traversal bugs happen. To serve a whole directory, use a [static route](routing.md#static-files) instead.

## Returning a modified request

Each handler also has a form that carries a request map, for when you changed it with `cowboy_req` (most often to set a cookie):

| Return | Note |
|---|---|
| `{ok, Variables, Options, Req}` | |
| `{status, Code, Headers, Body, Req}` | |
| `{redirect, Path, Req}` | |
| `{sendfile, Status, Headers, FileInfo, Mime, Req}` | |
| `{json, Status, Headers, Req, Data}` | The request comes **before** the data |

```erlang
remember(Req) ->
    Req1 = cowboy_req:set_resp_cookie(~"seen", ~"1", Req, #{http_only => true}),
    {json, 200, #{}, Req1, #{remembered => true}}.
```

```admonish warning
Nova's own documentation for the JSON handler lists the five-element form as `{json, Status, Headers, Data, Req}`, but 0.18.1 matches `{json, Status, Headers, Req, Data}`. Put the request fourth.
```

## Throwing a status

Deep inside a helper it can be simpler to give up than to thread an error back up. Throwing `{Status, Reason}` makes Nova render the status page for `Status`:

```erlang
missing(_Req) ->
    throw({404, page_missing}).
```

The request ends up in the blog's 404 handler, exactly like an unknown path. Use this sparingly; a returned error is easier to follow.

## Fallback controllers

A controller can return something Nova has no handler for and hand it to a **fallback controller**. This keeps actions focused on the happy path: they return `{error, Reason}` and one module turns reasons into responses.

`blog_hello_controller` declares its fallback with a module attribute:

```erlang
-module(blog_hello_controller).
-fallback_controller(blog_fallback_controller).
```

Inside `greet/1` and `created/1`, the `maybe` expression returns `{error, invalid_name}` as soon as `validate_name/1` does, and `create/1` returns `{error, missing_name}` when the body has no name. Nova has no `error` handler, so it calls `blog_fallback_controller:resolve/2` with the request and the unhandled value:

```erlang
-module(blog_fallback_controller).

-include_lib("kernel/include/logger.hrl").

-export([resolve/2]).

resolve(_Req, {error, invalid_name}) ->
    {json, 422, #{}, #{error => ~"invalid_name"}};
resolve(_Req, {error, missing_name}) ->
    {json, 400, #{}, #{error => ~"missing_name"}};
resolve(_Req, {error, not_found}) ->
    {status, 404, #{}, #{error => ~"not_found"}};
resolve(_Req, Unexpected) ->
    ?LOG_ERROR(#{msg => ~"unexpected controller return", return => Unexpected}),
    {status, 500}.
```

```shell
curl -i localhost:8080/hello/J%C3%B6rgen
```

```
HTTP/1.1 422 Unprocessable Entity
...
content-type: application/json
...

{"error":"invalid_name"}
```

The fallback's return must be a normal response tuple; if it returns something unhandled too, Nova gives up with a 500. Only **tuples** reach the fallback. A bare atom such as `oops` makes Nova crash while looking up the handler, and the client gets a 500.

## When a controller crashes

An exception in a controller is caught, logged with the stacktrace (when `use_stacktrace` is on, as in the dev config) and answered with `500`, rendered by the 500 status handler. Only that request is affected. Let it crash: use pattern matching to decide which inputs are yours to handle, and let anything else fail loudly.

## Custom response types

The handler registry is open. The blog's `/version` endpoint answers in plain text with a `{text, Body}` tuple of its own:

```erlang
version(_Req) ->
    {ok, Vsn} = application:get_key(blog, vsn),
    {text, ["blog ", Vsn, "\n"]}.
```

A handler is a function of three arguments: the returned tuple, the controller function, and the request. It returns `{ok, Req}` with the response set on the request:

```erlang
-module(blog_text_handler).

-export([handle/3]).

handle({text, Body}, Callback, Req) ->
    handle({text, 200, Body}, Callback, Req);
handle({text, Status, Body}, _Callback, Req) ->
    Req1 = cowboy_req:set_resp_headers(#{~"content-type" => ~"text/plain; charset=utf-8"}, Req),
    Req2 = cowboy_req:set_resp_body(Body, Req1),
    {ok, Req2#{resp_status_code => Status}}.
```

Register it when the application starts, in `blog_app:start/2`:

```erlang
ok = nova_handlers:register_handler(text, fun blog_text_handler:handle/3),
```

```shell
curl -i localhost:8080/version
```

```
HTTP/1.1 200 OK
content-length: 31
content-type: text/plain; charset=utf-8
date: Tue, 03 Nov 2026 07:25:21 GMT
server: Cowboy/Nova
vary: accept-encoding
set-cookie: session_id=Dk5MkT/iuaWTZp8RqWZ7pOGcxH+fLoM45Fsq+Vfb8KE=

blog 0.0.0+build.35.refa0bfe00
```

The version comes from git (`{vsn, git}` in `blog.app.src`), so yours will differ.

```admonish note
`register_handler/2` also accepts `{Module, Function}`, but 0.18.1 turns that into a four-argument fun while handlers are called with three. Pass `fun Mod:Fun/3` as above.
```

## Complete reference

| Return | Response |
|---|---|
| `{ok, Vars}` / `{view, Vars}` | Render the default template |
| `{ok, Vars, #{view => V, status_code => S, headers => H}}` | Render a named template; all options optional |
| `{json, Data}` | JSON, 200 (201 for POST) |
| `{json, Status, Headers, Data}` | JSON with status and headers |
| `{status, Status}` | Status only, empty body |
| `{status, Status, Headers}` | Same, with headers |
| `{status, Status, Headers, Body}` | Binary body, or JSON for a map |
| `{redirect, Path}` | 302 to `Path` |
| `{sendfile, Status, Headers, {Offset, Length, Path}, Mime}` | Stream a file |
| Any of the above with a trailing `Req` | Use a modified request (JSON: `Req` before `Data`) |
| `throw({Status, Reason})` | Status page for `Status` |
| Any other tuple | Fallback controller, or 500 without one |

## Generating controllers

`rebar3 nova gen_controller` writes a stub module:

```shell
rebar3 nova gen_controller --name pages --actions about,contact
```

```
===> Created .../blog/src/controllers/blog_pages_controller.erl
```

The file, as generated:

```erlang
-module(blog_pages_controller).

-export([about/1, contact/1]).

about(#{req := _Req} = _NovaReq) ->
    {json, #{<<"message">> => <<"TODO">>}}.

contact(#{req := _Req} = _NovaReq) ->
    {json, #{<<"message">> => <<"TODO">>}}.
```

```admonish bug
The request map has no `req` key, so these clauses never match and every call fails with a 500. Change the heads to `about(_Req) ->` before you use the stubs.
```

---

Next, let's look at [plugins](plugins.md), the layer that processes requests before and after your controllers.
