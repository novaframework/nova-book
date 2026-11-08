# Plugins

Plugins are Nova's middleware. They run before and after your controller and handle the cross-cutting work: decoding bodies, CSRF checks, CORS headers, correlation IDs, uploads. Nova ships a handful of them, and in this chapter we use all of them in the blog.

## The request pipeline

Every request passes through the same stages:

1. **Router** - finds the route (or answers 404/405 straight away)
2. **Pre-request plugins** - in list order
3. **Security** - the group's security function, if any
4. **Controller**
5. **Post-request plugins** - in list order

Because pre-request plugins run *before* security, a plugin can prepare data the security function needs (a decoded body, a correlation ID in the logs). It also means plugins run for requests that security then rejects.

A plugin is a module implementing the `nova_plugin` behaviour: `pre_request/4`, `post_request/4` and `plugin_info/0`. Each callback gets `(Req, Env, Options, State)` and returns `{ok, Req, State}` to continue, or `{stop, Req, State}` after replying itself. They can add keys to the request map, which is where `json`, `params` and friends come from. Writing your own is covered in [Custom Plugins & CORS](../production/plugins-cors.md).

## Configuring plugins

Global plugins are listed in the `nova` section of the sys.config. The companion app's `dev_sys.config.src`:

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{decode_json_body => true,
                                         read_urlencoded_body => true}}
]}
```

Each entry is `{Phase, Module, Options}`, with `Phase` either `pre_request` or `post_request`. These apply to every route group that does not declare its own `plugins`. ([Custom Plugins & CORS](../production/plugins-cors.md) later wraps this list in a request logger.)

## nova_request_plugin

The request plugin reads the body and decodes it into the request map:

| Option | Request map key | Effect |
|---|---|---|
| `decode_json_body => true` | `json` | Decodes `application/json` bodies |
| `read_urlencoded_body => true` | `params` | Parses `application/x-www-form-urlencoded` bodies into a map |
| `parse_qs => true` | `parsed_qs` | Parses the query string into a map |
| `parse_qs => list` | `parsed_qs` | Same, as a list of `{Key, Value}` |

Keys and values are binaries in all three. A few details matter in practice:

- JSON decoding is skipped for `GET` and `DELETE`.
- A request that says `content-type: application/json` but has an empty or malformed body is answered with `400` and an empty body; the controller never runs.
- `multipart/form-data` bodies are never buffered here. That is the multipart plugin's job, below.
- A body that matches none of the enabled decoders is still read (into `body`) but not decoded, so a clause matching `#{json := ...}` simply does not match.

```shell
curl -i -X POST localhost:8080/hello -H 'content-type: application/json' -d '{"name":'
```

```
HTTP/1.1 400 Bad Request
...
content-length: 0
...
```

The `/hello` group enables all three options, so `greet/1` can read `?shout=true`:

```shell
curl 'localhost:8080/hello/nova?shout=true'
```

```
{"greeting":"HELLO, NOVA!"}
```

```erlang
greeting(Name, #{~"shout" := ~"true"}) ->
    string:uppercase(greeting(Name, #{}));
greeting(Name, _Query) ->
    <<"Hello, ", Name/binary, "!">>.
```

### Choosing the JSON library

Nova encodes and decodes JSON through the module named by `json_lib`, which defaults to `thoas`. Nova looks the setting up in the environment of your *bootstrap application* (`blog`), not of `nova`, and it expects `encode/1` plus a `decode/1` that returns `{ok, Term}` or an error.

OTP's own `json` module (OTP 27+) encodes fine, but `json:decode/1` returns the term itself and raises on bad input, so it cannot be plugged in directly. The blog uses a small adapter:

```erlang
-module(blog_json).

-export([encode/1, decode/1]).

encode(Term) ->
    json:encode(Term).

decode(Binary) ->
    try
        {ok, json:decode(Binary)}
    catch
        error:Reason -> {error, Reason}
    end.
```

and selects it in each sys.config:

```erlang
{blog, [{json_lib, blog_json}, {ops_token, ~"dev-ops-token"}]},
```

From now on every `{json, ...}` response, every decoded body and Nova's own error pages go through OTP `json`. The output is the same as with `thoas` for the maps in this book, and in your own code you use `json:encode/1` and `json:decode/1` directly.

## nova_correlation_plugin

Tags every request with a correlation ID so that log lines from one request (and from the services it calls) can be tied together:

```erlang
{pre_request, nova_correlation_plugin, #{
    request_correlation_header => ~"x-correlation-id",
    logger_metadata_key => correlation_id
}},
```

| Option | Default | Description |
|---|---|---|
| `request_correlation_header` | none | Reuse an incoming ID from this header |
| `logger_metadata_key` | `~"correlation-id"` | Logger metadata key for the ID |

The plugin takes the ID from the header, or generates a UUID v4, then puts it in the request map as `correlation_id`, in the process's logger metadata, and in an `x-correlation-id` response header:

```shell
curl -i -H 'x-correlation-id: abc-123' localhost:8080/hello/nova
```

```
HTTP/1.1 200 OK
...
x-correlation-id: abc-123
...

{"greeting":"Hello, nova!"}
```

Since the ID is in the logger metadata, the `?LOG_INFO(#{msg => ~"greeting", name => Name})` in `greet/1` carries it without the controller doing anything. Whether you see it depends on the formatter: the dev config's flatlog template prints only the message and location, while a formatter that includes metadata, such as Nova's `nova_jsonlogger`, prints it with every line.

```admonish note
Only accept a client-supplied correlation ID from callers you trust (your own front end or other services). The plugin does not validate the header's contents.
```

## nova_cors_plugin

Adds CORS headers so browsers on other origins may call the route, and answers preflight `OPTIONS` requests itself:

```erlang
{pre_request, nova_cors_plugin, #{allow_origins => ~"*"}},
```

`allow_origins` is required and becomes the `Access-Control-Allow-Origin` value. The plugin also sends `Access-Control-Allow-Headers: *` and `Access-Control-Allow-Methods: *`, on every request in the group. The greeting is public read-only data, so `*` is fine there; for anything with cookies or credentials name the exact origin.

The router only lets a request reach the plugins if the route accepts its method, which is why `/hello/:name` lists `options` next to `get`:

```shell
curl -i -X OPTIONS -H 'origin: https://example.com' localhost:8080/hello/nova
```

```
HTTP/1.1 200 OK
Access-Control-Allow-Headers: *
Access-Control-Allow-Methods: *
Access-Control-Allow-Origin: *
content-length: 0
...
```

The plugin stops on `OPTIONS`, so `greet/1` never sees the preflight. [Custom Plugins & CORS](../production/plugins-cors.md) builds a stricter variant.

## Per-route plugins

A route group with a `plugins` key uses exactly that list instead of the global one. The `/hello` group needs the query string and CORS, so it brings its own:

```erlang
#{
    prefix => "/hello",
    security => false,
    plugins => [
        {pre_request, nova_correlation_plugin, #{
            request_correlation_header => ~"x-correlation-id",
            logger_metadata_key => correlation_id
        }},
        {pre_request, nova_cors_plugin, #{allow_origins => ~"*"}},
        {pre_request, nova_request_plugin, #{
            decode_json_body => true,
            read_urlencoded_body => true,
            parse_qs => true
        }}
    ],
    routes => [
        {"/", fun blog_hello_controller:create/1, #{methods => [post]}},
        {"/:name", fun blog_hello_controller:greet/1, #{methods => [get, options]}}
    ]
},
```

Because the list *replaces* the global plugins, it repeats `nova_request_plugin`; leave it out and the group's controllers get no `json` or `params`. Order matters within the list: the correlation ID is set first so that everything after it logs with the ID, and CORS answers preflights before any body is read.

`rebar3 nova middleware` prints the effective plugin chain of every group:

```shell
rebar3 nova middleware
```

```
=== Global Plugins ===
  pre_request: nova_request_plugin #{decode_json_body => true,
                                     read_urlencoded_body => true}

=== Route Groups (blog_router) ===

  Group: prefix=/  security=false
  Plugins:
    (inherits global)
    pre_request: nova_request_plugin #{decode_json_body => true,
                                       read_urlencoded_body => true}
  Routes:
    GET / -> blog_main_controller:index
    GET /heartbeat -> fun in blog_router
...
  Group: prefix=/hello  security=false
  Plugins:
    pre_request: nova_correlation_plugin #{logger_metadata_key =>
                                               correlation_id,
                                           request_correlation_header =>
                                               <<"x-correlation-id">>}
    pre_request: nova_cors_plugin #{allow_origins => <<"*">>}
    pre_request: nova_request_plugin #{decode_json_body => true,
                                       read_urlencoded_body => true,
                                       parse_qs => true}
  Routes:
    POST / -> blog_hello_controller:create
    GET,OPTIONS /:name -> blog_hello_controller:greet
...
  Group: prefix=/  security=false
  Plugins:
    (inherits global)
    pre_request: nova_request_plugin #{decode_json_body => true,
                                       read_urlencoded_body => true}
  Routes:
    STATIC /robots.txt -> assets/robots.txt
    STATIC /assets/[...] -> assets
    STATUS 404 -> blog_error_controller:not_found
```

A group without `plugins` inherits the global list; a group with its own list shows only that list. Static file routes and status-code routes are listed too, so the last group shows `STATIC` and `STATUS` entries. The inline heartbeat fun is shown as `fun in blog_router`.

## CSRF protection

A form that changes state must prove it was served by your site, or another site can make your visitors' browsers submit it for them. `nova_csrf_plugin` implements the synchronizer token pattern: a random token is stored in the visitor's session and must come back with every `POST`, `PUT`, `PATCH` and `DELETE`.

| Option | Default | Description |
|---|---|---|
| `field_name` | `~"_csrf_token"` | Form field carrying the token |
| `header_name` | `~"x-csrf-token"` | Header carrying the token (for `fetch` calls) |
| `session_key` | `~"_csrf_token"` | Session key the token is stored under |
| `excluded_paths` | `[]` | Path prefixes to skip, such as `[~"/api/"]` |

On safe methods the plugin makes sure the session has a token and puts it in the request map as `csrf_token`. Nova passes it to every template automatically, so a form only has to include it:

```html
<form method="post" action="/contact" enctype="multipart/form-data">
  <input type="hidden" name="_csrf_token" value="{{ csrf_token }}">
  <label>Name <input name="name" value="{{ name }}"></label>
  <label>Message <textarea name="message">{{ message }}</textarea></label>
  <label>Attachment <input type="file" name="attachment"></label>
  <button type="submit">Send</button>
</form>
```

On unsafe methods it compares the submitted token with the session's in constant time and answers `403` if they differ. The session itself is the `session_id` cookie Nova hands every new visitor (you saw it in every response so far), so the plugin works without any setup; [Sessions](../auth-sessions/sessions.md) explains the session layer.

The blog's contact page is the first form. Its group:

```erlang
#{
    prefix => "/contact",
    security => false,
    plugins => [
        {pre_request, nova_multipart_plugin, #{
            handler => {nova_multipart_memory_handler, #{}},
            max_parts => 8,
            max_part_size => 1000000
        }},
        {pre_request, nova_csrf_plugin, #{}}
    ],
    routes => [
        {"/", fun blog_contact_controller:show/1, #{methods => [get]}},
        {"/", fun blog_contact_controller:submit/1, #{methods => [post]}}
    ]
},
```

The CSRF plugin reads the token from `params`, so whatever parses the form must run **before** it: here the multipart plugin, for a `urlencoded` form `nova_request_plugin` with `read_urlencoded_body`.

The controller only deals with valid submissions:

```erlang
-module(blog_contact_controller).

-export([
    show/1,
    submit/1
]).

show(_Req) ->
    {ok, [], #{view => blog_contact}}.

submit(#{params := Params, files := Files}) ->
    Name = string:trim(maps:get(~"name", Params, ~"")),
    Message = string:trim(maps:get(~"message", Params, ~"")),
    case Name =:= ~"" orelse Message =:= ~"" of
        true ->
            Vars = [
                {error, ~"Please fill in your name and a message."},
                {name, Name},
                {message, Message}
            ],
            {ok, Vars, #{view => blog_contact, status_code => 422}};
        false ->
            {ok, [{name, Name}, {attachments, attachments(Files)}], #{view => blog_contact_thanks}}
    end.

attachments(Files) ->
    [
        #{filename => Filename, size => byte_size(Body)}
     || #{filename := Filename, result := #{body := Body}} <- Files
    ].
```

Try it with curl, keeping the session cookie in a jar. Without the token:

```shell
curl -s -c jar.txt localhost:8080/contact > /dev/null
curl -i -b jar.txt -F name=Ada -F message=Hi localhost:8080/contact
```

```
HTTP/1.1 403 Forbidden
content-length: 30
content-type: text/plain
date: Tue, 03 Nov 2026 07:25:32 GMT
server: Cowboy/Nova
vary: accept-encoding

Forbidden - CSRF token invalid
```

With the token from the form:

```shell
TOKEN=$(curl -s -b jar.txt localhost:8080/contact | sed -n 's/.*name="_csrf_token" value="\([^"]*\)".*/\1/p')
curl -s -b jar.txt -F "_csrf_token=$TOKEN" -F name=Ada -F 'message=Hello there' \
     -F attachment=@notes.txt localhost:8080/contact
```

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Thanks</title>
</head>
<body>
  <h1>Thanks, Ada!</h1>
  
  <p>Received notes.txt (6 bytes).</p>
  
</body>
</html>
```

ErlyDTL escapes variables, so a name like `<script>` is rendered as `&lt;script&gt;`; the companion app's tests check that.

```admonish warning
Use `excluded_paths` only for routes that authenticate in a way browsers do not send automatically (an `Authorization: Bearer` header, for example). Anything authenticated by a cookie needs CSRF protection.
```

## File uploads

`nova_multipart_plugin` (new in Nova 0.18) reads `multipart/form-data` bodies. Ordinary fields go into `params`. Each file part is streamed, chunk by chunk, to a **handler** module, and the request gets a `files` list of `#{name, filename, content_type, result}` maps. On requests that are not multipart it sets `params => #{}` (unless an earlier plugin set it) and `files => []`, so a controller can always match both keys.

`handler` is required and is a `{Module, InitArgs}` pair implementing `nova_multipart_handler`. Nova ships two:

| Handler | `result` | Use for |
|---|---|---|
| `nova_multipart_memory_handler` | `#{body => Binary}` | Small attachments |
| `nova_multipart_file_handler` | `#{path => Path}` | Larger files, written to `InitArgs`'s `dir` under a random name |

The file handler never uses the client's filename in the path and only keeps extensions you allow (`extensions => [~"png", ~"jpg"]`). Treat `filename` and `content_type` as untrusted input: they are whatever the client sent.

The limits answer early instead of buffering an attacker's body:

| Option | Default | Exceeded |
|---|---|---|
| `max_parts` | 32 | 413 |
| `max_part_size` | 8 000 000 bytes | 413 |
| `max_field_size` | 65 536 bytes | 413 |
| `max_total_size` | 64 000 000 bytes | 413 |
| `read_timeout` | 60 000 ms | 408 |

The contact form caps attachments at 1 MB, so a larger one is refused with `413` before the controller runs.

```admonish note
In Nova 0.18.1 the part counter trips one part early: with `max_parts => 4` a form of exactly four parts is rejected. Leave some headroom.
```

## Built-in plugins

| Plugin | Purpose | Adds to the request map |
|---|---|---|
| `nova_request_plugin` | Reads and decodes JSON and form bodies, parses query strings | `body`, `json`, `params`, `parsed_qs` |
| `nova_correlation_plugin` | Correlation ID in logs and response header | `correlation_id` |
| `nova_cors_plugin` | CORS headers, answers preflights | |
| `nova_csrf_plugin` | Synchronizer-token CSRF protection | `csrf_token` |
| `nova_multipart_plugin` | Streams multipart uploads to a handler | `params`, `files` |

---

With routing, controllers and plugins in place, the blog is ready for real data. Next we set up the [database layer](../data-layer/setup.md).
