# Plugins

Plugins are Nova's middleware system. They run code before and after your controller handles a request — useful for decoding request bodies, adding headers, logging, rate limiting, and more.

## How the plugin pipeline works

Every HTTP request flows through a pipeline:

1. **Pre-request plugins** run in list definition order (first in the list runs first)
2. The **controller** handles the request
3. **Post-request plugins** run in list definition order

A plugin module implements the `nova_plugin` behaviour and exports `pre_request/4`, `post_request/4`, and `plugin_info/0`.

Each callback receives `(Req, Env, Options, State)` and returns `{ok, Req, State}` to pass control to the next plugin. Plugins can also enrich the request map — adding keys like `json`, `params`, or `correlation_id` — so that later plugins and controllers can use them.

## Configuring plugins

Plugins are configured in `sys.config` under the `nova` application key:

```erlang
{nova, [
    {plugins, [
        {pre_request, nova_request_plugin, #{decode_json_body => true}}
    ]}
]}
```

Each plugin entry is a tuple: `{Phase, Module, Options}` where Phase is `pre_request` or `post_request`.

## nova_request_plugin

This built-in plugin handles request body decoding and query string parsing. It supports three options:

| Option | Type | Request map key | Description |
|---|---|---|---|
| `decode_json_body` | `true` | `json` | Decodes JSON request bodies |
| `read_urlencoded_body` | `true` | `params` | Decodes URL-encoded form bodies |
| `parse_qs` | `true \| list` | `parsed_qs` | Parses the URL query string |

### decode_json_body

When enabled, requests with `Content-Type: application/json` have their body decoded and placed in the `json` key:

```erlang
{pre_request, nova_request_plugin, #{decode_json_body => true}}
```

```erlang
create(#{json := #{<<"title">> := Title}} = _Req) ->
    %% Use the decoded JSON body
    {json, #{created => Title}}.
```

If the content type is `application/json` but the body is empty or malformed, the plugin returns a 400 response and the controller is never called.

`decode_json_body` is skipped for GET and DELETE requests since they typically have no body.

### read_urlencoded_body

When enabled, requests with `Content-Type: application/x-www-form-urlencoded` have their body parsed into a map under the `params` key:

```erlang
{pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
```

```erlang
login(#{params := #{<<"username">> := User, <<"password">> := Pass}} = _Req) ->
    %% Use the decoded form params
    ...
```

### parse_qs

Parses the URL query string (e.g. `?page=2&limit=10`). The value controls the format:

- `true` — returns a map in the `parsed_qs` key
- `list` — returns a proplist of `{Key, Value}` tuples

```erlang
{pre_request, nova_request_plugin, #{parse_qs => true}}
```

```erlang
index(#{parsed_qs := #{<<"page">> := Page}} = _Req) ->
    %% Use query params
    ...
```

### Combining options

You can enable all three at once:

```erlang
{pre_request, nova_request_plugin, #{
    decode_json_body => true,
    read_urlencoded_body => true,
    parse_qs => true
}}
```

## nova_correlation_plugin

This plugin assigns a unique correlation ID to every request — essential for tracing requests across services in your logs.

```erlang
{pre_request, nova_correlation_plugin, #{
    request_correlation_header => <<"x-correlation-id">>,
    logger_metadata_key => correlation_id
}}
```

| Option | Default | Description |
|---|---|---|
| `request_correlation_header` | *(none — always generates)* | Header to read an existing correlation ID from. Cowboy lowercases all header names. |
| `logger_metadata_key` | `<<"correlation-id">>` | Key set in OTP logger process metadata |

The plugin:

1. Reads the correlation ID from the configured header, or generates a v4 UUID if missing
2. Sets the ID in OTP logger metadata (so all log messages for this request include it)
3. Adds an `x-correlation-id` response header
4. Stores the ID in the request map as `correlation_id`

Access it in your controller:

```erlang
show(#{correlation_id := CorrId} = _Req) ->
    logger:info("Handling request ~s", [CorrId]),
    ...
```

## nova_csrf_plugin

This plugin provides CSRF protection using the synchronizer token pattern. It generates a random token per session and validates it on state-changing requests.

```erlang
{pre_request, nova_csrf_plugin, #{}}
```

| Option | Default | Description |
|---|---|---|
| `field_name` | `<<"_csrf_token">>` | Form field name to check |
| `header_name` | `<<"x-csrf-token">>` | Header name to check (for AJAX) |
| `session_key` | `<<"_csrf_token">>` | Key used to store the token in the session |
| `excluded_paths` | `[]` | List of path prefixes to skip protection for |

### How it works

- **Safe methods** (GET, HEAD, OPTIONS): The plugin ensures a CSRF token exists in the session and injects it into the request map as `csrf_token`.
- **Unsafe methods** (POST, PUT, PATCH, DELETE): The plugin reads the submitted token from the form field or header and validates it against the session token. If the token is missing or doesn't match, the request is rejected with a 403 response.

### Template integration

In your ErlyDTL templates, include the token in forms as a hidden field:

```html
<form method="post" action="/login">
    <input type="hidden" name="_csrf_token" value="{{ csrf_token }}" />
    <!-- rest of form -->
    <button type="submit">Log in</button>
</form>
```

The `csrf_token` variable is available because the plugin adds it to the request map, and Nova passes request map values to templates as template variables.

For AJAX requests, send the token in a header instead:

```javascript
fetch('/api/resource', {
    method: 'POST',
    headers: {
        'X-CSRF-Token': csrfToken,
        'Content-Type': 'application/json'
    },
    body: JSON.stringify(data)
});
```

### Excluding API paths

If you have API routes that use a different authentication scheme (e.g. bearer tokens), exclude them from CSRF checks:

```erlang
{pre_request, nova_csrf_plugin, #{
    excluded_paths => [<<"/api/">>]
}}
```

```admonish warning
`nova_request_plugin` must run **before** `nova_csrf_plugin` so that form params are parsed into the `params` key. Plugin order matters — list `nova_request_plugin` first.
```

## Setting up for our login form

In the next chapter we will build a login form that sends URL-encoded data. To have Nova decode this automatically, update the plugin config in `dev_sys.config.src`:

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
]}
```

With this setting, form POST data is decoded and placed in the `params` key of the request map, ready for your controller to use.

```admonish tip
You can enable multiple decoders at once. We will add `decode_json_body => true` later when we build our [JSON API](../building-api/json-api.md).
```

## Per-route plugins

So far we've configured plugins globally in `sys.config`. You can also set plugins per route group by adding a `plugins` key to the group map in your router:

```erlang
routes(_Environment) ->
  [
    #{prefix => "/api",
      plugins => [
          {pre_request, nova_request_plugin, #{decode_json_body => true}}
      ],
      routes => [
          {"/posts", fun blog_posts_controller:list/1, #{methods => [get]}}
      ]
    },
    #{prefix => "",
      plugins => [
          {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
      ],
      routes => [
          {"/login", fun blog_main_controller:login/1, #{methods => [get, post]}}
      ]
    }
  ].
```

When `plugins` is set on a route group, it **overrides** the global plugin configuration for those routes. This lets you use JSON decoding for API routes and form decoding for HTML routes without conflict.

See [Custom Plugins and CORS](../going-further/plugins-cors.md) for more examples, including per-route CORS.

## Built-in plugins summary

| Plugin | Phase | Purpose | Key request map additions |
|---|---|---|---|
| `nova_request_plugin` | pre_request | Decodes JSON/form bodies, parses query strings | `json`, `params`, `parsed_qs` |
| `nova_correlation_plugin` | pre_request | Assigns correlation IDs for request tracing | `correlation_id` |
| `nova_csrf_plugin` | pre_request | CSRF protection via synchronizer token | `csrf_token` |
| `nova_cors_plugin` | pre_request | Adds CORS headers, handles preflight requests | *(headers only)* |

A realistic configuration using multiple plugins:

```erlang
{nova, [
    {plugins, [
        {pre_request, nova_correlation_plugin, #{
            request_correlation_header => <<"x-correlation-id">>,
            logger_metadata_key => correlation_id
        }},
        {pre_request, nova_request_plugin, #{
            decode_json_body => true,
            read_urlencoded_body => true,
            parse_qs => true
        }},
        {pre_request, nova_csrf_plugin, #{
            excluded_paths => [<<"/api/">>]
        }}
    ]}
]}
```

Ordering matters: `nova_correlation_plugin` runs first so all subsequent log messages include the correlation ID. `nova_request_plugin` runs before `nova_csrf_plugin` so form params are available for token validation.

---

With plugins configured to decode form data, we can now build our first [view and login page](views-auth-sessions.md).
