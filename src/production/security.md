# Security

This chapter describes how common web vulnerabilities can occur in a Nova application and the secure coding practices to prevent them. Nova provides built-in security plugins, but they must be enabled and configured correctly.

For additional Erlang-specific guidance, see the [ERLEF Secure Coding and Deployment Hardening Guidelines](https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/).

## Remote code execution

Remote code execution (RCE) is the most severe class of vulnerability — it gives an attacker full access to your production server.

### Unsafe functions

Never pass untrusted input to any of the following:

```erlang
%% Code evaluation
erl_eval:exprs(UserInput, Bindings)
erl_eval:expr(UserInput, Bindings)

%% OS command execution
os:cmd(UserInput)

%% Deserialization
erlang:binary_to_term(UserInput)
```

### OS commands

`os:cmd/1` passes its argument through the system shell, making it trivial to inject arbitrary commands.

```erlang
%% VULNERABLE — shell injection
os:cmd("convert " ++ UserFilename ++ " output.png")

%% SAFE — use open_port with explicit argv (no shell)
open_port({spawn_executable, "/usr/bin/convert"},
          [{args, [UserFilename, "output.png"]}, exit_status])
```

### Binary deserialization

`binary_to_term/2` with the `[safe]` option only prevents creation of new atoms. It does **not** prevent construction of executable terms — an attacker can craft a binary that triggers arbitrary function calls when deserialized.

```erlang
%% DANGEROUS — even with [safe], executable terms can be created
erlang:binary_to_term(UserInput, [safe])
```

If you need to exchange structured data with clients, use JSON. Nova uses `thoas` by default.

### Atom exhaustion

Atoms are never garbage collected. Converting untrusted input to atoms will eventually crash the VM.

```erlang
%% VULNERABLE
binary_to_atom(UserInput, utf8)
list_to_atom(UserInput)

%% SAFE — only succeeds if the atom already exists
binary_to_existing_atom(UserInput, utf8)
list_to_existing_atom(UserInput)
```

## SQL injection

SQL injection enables an attacker to read, modify, or delete arbitrary data in your database — and in some cases execute system commands.

### Parameterized queries with Kura

[Kura](../data-layer/crud.md) uses parameterized queries by default, which prevents injection:

```erlang
%% SAFE — parameterized automatically
Q = kura_query:from(fruit),
Q1 = kura_query:where(Q, [{quantity, '>=', MinQ}, {secret, false}]),
{ok, Fruits} = my_repo:all(Q1).
%% Generated: SELECT * FROM "fruits" WHERE "quantity" >= $1 AND "secret" = $2
```

### Raw SQL

When using `pgo` directly, always pass parameters as a list:

```erlang
%% VULNERABLE — direct interpolation
pgo:query("SELECT * FROM fruits WHERE quantity >= " ++ MinQ)

%% SAFE — parameterized
pgo:query("SELECT * FROM fruits WHERE quantity >= $1", [MinQ])
```

### Mass assignment

Kura changesets require explicit field whitelisting via `cast/3`. Including sensitive fields like `is_admin` exposes privilege escalation:

```erlang
%% VULNERABLE — user can escalate to admin
registration_changeset(User, Params) ->
    kura_changeset:cast(User, Params, [name, email, password, is_admin]).

%% SAFE — is_admin cannot be set from user input
registration_changeset(User, Params) ->
    kura_changeset:cast(User, Params, [name, email, password]).
```

## Server-side request forgery (SSRF)

SSRF occurs when your application makes HTTP requests using URLs derived from untrusted input. An attacker can route requests to internal services — cloud metadata endpoints (AWS `169.254.169.254`), databases, caches, or unpatched microservices.

```erlang
%% VULNERABLE — user controls the destination
handle(#{json := #{<<"url">> := Url}} = Req) ->
    {ok, _Status, _Headers, Body} = hackney:get(Url),
    {json, 200, #{}, #{<<"result">> => Body}, Req}.
```

```admonish danger
SSRF has been the root cause of major data breaches, including the 2019 Capital One breach where an attacker exploited SSRF to access AWS metadata credentials.
```

**Mitigations:**

- Avoid making HTTP requests based on user input whenever possible.
- Validate URLs against an allowlist of permitted hosts and schemes.
- Block requests to private IP ranges (`10.0.0.0/8`, `172.16.0.0/12`, `192.168.0.0/16`, `169.254.0.0/16`, `127.0.0.0/8`).

```erlang
allowed_hosts() -> [<<"api.example.com">>, <<"cdn.example.com">>].

validate_url(Url) ->
    #{host := Host, scheme := Scheme} = uri_string:parse(Url),
    case {Scheme, lists:member(Host, allowed_hosts())} of
        {<<"https">>, true} -> ok;
        _ -> {error, forbidden}
    end.
```

## Cross-site scripting (XSS)

XSS allows an attacker to execute arbitrary JavaScript in a victim's browser, stealing session cookies, credentials, or performing actions on their behalf.

### Default protection

Nova uses [ErlyDTL](../views-templates/templates.md) for HTML templates. ErlyDTL auto-escapes all variables by default — `<script>alert(1)</script>` renders as `&lt;script&gt;alert(1)&lt;/script&gt;`.

### Dangerous patterns

```erlang
%% VULNERABLE — raw HTML response with user input
handle(#{parsed_qs := #{<<"name">> := Name}} = Req) ->
    Html = <<"<html><body>Hello, ", Name/binary, "</body></html>">>,
    {ok, Req2} = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>}, Html, Req),
    {ok, Req2}.

%% VULNERABLE — user-controlled content-type
handle(#{parsed_qs := #{<<"type">> := ContentType}} = Req) ->
    {ok, Req2} = cowboy_req:reply(200,
        #{<<"content-type">> => ContentType}, Body, Req),
    {ok, Req2}.
```

The first example bypasses ErlyDTL's escaping by constructing HTML directly. The second lets an attacker set `text/html` as the content type for data that shouldn't be rendered as HTML.

### Safe patterns

```erlang
%% SAFE — ErlyDTL template (auto-escaping)
handle(#{parsed_qs := #{<<"name">> := Name}} = Req) ->
    {ok, [{name, Name}], Req}.
    %% Template: <body>Hello, {{ name }}</body>

%% SAFE — JSON response (no HTML interpretation)
handle(#{parsed_qs := #{<<"name">> := Name}} = Req) ->
    {json, 200, #{}, #{<<"greeting">> => <<"Hello, ", Name/binary>>}, Req}.
```

### Content Security Policy

CSP tells browsers which sources of scripts, styles, and other resources are permitted. Enable it via `nova_secure_headers_plugin`:

```erlang
{pre_request, nova_secure_headers_plugin, #{
    csp => <<"default-src 'self'; script-src 'self'; style-src 'self'">>
}}
```

### File upload XSS

If your application serves user-uploaded files, an attacker can upload an HTML file containing `<script>` tags. If served with `text/html`, the script executes in the context of your domain.

**Mitigations:**
- Validate file MIME types against an allowlist.
- Serve uploads from a separate domain or with `Content-Disposition: attachment`.
- Never let users control the `Content-Type` response header.

## Cross-site request forgery (CSRF)

CSRF tricks a user's browser into making state-changing requests to your application using their existing session. For example, a malicious site could include a form that POSTs to your `/transfer` endpoint — the browser automatically attaches the victim's session cookie.

Nova provides `nova_csrf_plugin`, which implements the Synchronizer Token Pattern.

### Enabling CSRF protection

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{read_urlencoded_body => true}},
    {pre_request, nova_csrf_plugin, #{}}
]}
```

```admonish warning
`nova_csrf_plugin` must run **after** `nova_request_plugin` so that form parameters are parsed before CSRF validation.
```

### How it works

1. For safe methods (GET, HEAD, OPTIONS), the plugin generates a token stored in the [session](../auth-sessions/sessions.md).
2. For unsafe methods (POST, PUT, PATCH, DELETE), it validates the submitted token against the session.
3. Tokens are compared using `crypto:hash_equals/2` — constant-time comparison prevents timing attacks.
4. The token is automatically available in ErlyDTL templates as `csrf_token`.

### Including the token in forms

```html
<form method="post" action="/update">
    <input type="hidden" name="_csrf_token" value="{{ csrf_token }}" />
    <button type="submit">Update</button>
</form>
```

### Including the token in AJAX requests

```javascript
const token = document.querySelector('meta[name="csrf-token"]').content;

fetch('/api/update', {
    method: 'POST',
    headers: {
        'Content-Type': 'application/json',
        'x-csrf-token': token
    },
    body: JSON.stringify(data)
});
```

### GET requests must not change state

Never allow state-changing operations via GET. Query parameters cannot be protected by CSRF tokens in the same way as POST bodies.

```erlang
%% VULNERABLE — state change via GET
{"/users/update_bio", fun user_controller:update_bio/1, #{methods => [get]}}
%% Attacker: <img src="https://yourapp.com/users/update_bio?bio=Hacked" />

%% SAFE — POST only
{"/users/update_bio", fun user_controller:update_bio/1, #{methods => [post]}}
```

### Excluding API routes

For API endpoints that use token-based [authentication](../auth-sessions/authorization.md) (not cookies), exclude them from CSRF validation:

```erlang
{pre_request, nova_csrf_plugin, #{
    excluded_paths => [<<"/api/">>]
}}
```

```admonish warning
Only exclude paths that do **not** rely on cookie-based authentication.
```

## CORS misconfiguration

Cross-Origin Resource Sharing (CORS) controls which domains can make requests to your API. An overly permissive policy allows malicious sites to read sensitive data from authenticated users.

This is covered in detail in [Custom Plugins & CORS](plugins-cors.md). The key rule is: **never use wildcard origins in production if your endpoints use cookie-based authentication.**

```erlang
%% DANGEROUS — any site can read authenticated responses
{pre_request, nova_cors_plugin, #{allow_origins => <<"*">>}}

%% SAFE — explicit allowlist
{pre_request, nova_cors_plugin, #{
    allow_origins => <<"https://app.example.com">>
}}
```

## Broken access control

Broken access control means an attacker can perform actions they shouldn't — viewing other users' data, escalating privileges, or bypassing authentication.

### Derive authorization from the session

Never trust client-supplied identifiers. Always use the server-side session or `auth_data` from the [security handler](../auth-sessions/authentication.md):

```erlang
%% VULNERABLE — trusts client-supplied user ID
update_profile(#{json := #{<<"user_id">> := UserId, <<"bio">> := Bio}} = Req) ->
    {ok, _} = my_repo:update(user, UserId, #{bio => Bio}),
    {json, 200, #{}, #{<<"status">> => <<"ok">>}, Req}.

%% SAFE — user ID from authenticated session
update_profile(#{auth_data := #{id := UserId}, json := #{<<"bio">> := Bio}} = Req) ->
    {ok, _} = my_repo:update(user, UserId, #{bio => Bio}),
    {json, 200, #{}, #{<<"status">> => <<"ok">>}, Req}.
```

### Resource ownership

Check ownership in the controller, as shown in the [Authorization](../auth-sessions/authorization.md) chapter:

```erlang
update(#{bindings := #{<<"id">> := Id}, json := Params,
         auth_data := #{id := UserId}}) ->
    case my_repo:get(post, binary_to_integer(Id)) of
        {ok, #{user_id := UserId} = Post} ->
            CS = post:changeset(Post, Params),
            case my_repo:update(CS) of
                {ok, Updated} -> {json, post_to_json(Updated)};
                {error, CS1} -> {json, 422, #{}, #{errors => changeset_errors(CS1)}}
            end;
        {ok, _Post} ->
            {status, 403};
        {error, not_found} ->
            {status, 404}
    end.
```

## Session security

Nova's session system is covered in the [Sessions](../auth-sessions/sessions.md) chapter. Here are the security-critical aspects.

### Secure cookie defaults

Nova sets these cookie attributes by default:

| Attribute | Default | Purpose |
|-----------|---------|---------|
| `http_only` | `true` | Prevents JavaScript access (mitigates XSS) |
| `secure` | `true` | Cookie only sent over HTTPS |
| `same_site` | `lax` | Browser-level CSRF mitigation |
| `path` | `/` | Cookie applies to all paths |

```admonish danger
Do not weaken these defaults unless you have a specific reason and understand the implications.
```

### Session fixation

After a user authenticates, rotate the session ID to prevent session fixation attacks — where an attacker sets a known session ID before the victim logs in:

```erlang
login(#{json := #{<<"email">> := Email, <<"password">> := Pass}} = Req) ->
    case blog_accounts:authenticate(Email, Pass) of
        {ok, User} ->
            ok = nova_session:rotate(Req),
            ok = nova_session:set(Req, <<"user_id">>, maps:get(id, User)),
            {json, 200, #{}, #{<<"status">> => <<"ok">>}, Req};
        error ->
            {json, 401, #{}, #{<<"error">> => <<"invalid credentials">>}, Req}
    end.
```

### Session expiration

Configure appropriate timeouts:

```erlang
{nova, [
    {session_max_age, 86400},       %% 24 hours absolute maximum
    {session_idle_timeout, 3600}    %% 1 hour idle timeout
]}
```

Expired and idle sessions are automatically cleaned up every 60 seconds.

### Sensitive data

Avoid storing sensitive data (passwords, API keys, credit card numbers) in sessions. If a process handles secrets, mark it:

```erlang
process_flag(sensitive, true)
```

This prevents the process state from appearing in crash dumps.

## Rate limiting

Rate limiting protects against brute-force attacks and abuse. Nova provides `nova_rate_limit_plugin`.

### Basic configuration

```erlang
{pre_request, nova_rate_limit_plugin, #{
    max_requests => 100,
    window_ms => 60000          %% 100 requests per minute
}}
```

### Targeted rate limiting for sensitive endpoints

Apply stricter limits to authentication endpoints:

```erlang
{pre_request, nova_rate_limit_plugin, #{
    max_requests => 5,
    window_ms => 300000,         %% 5 attempts per 5 minutes
    paths => [<<"/login">>, <<"/api/auth">>]
}}
```

### Custom key function

By default, rate limiting is per client IP. For API token-based limiting:

```erlang
{pre_request, nova_rate_limit_plugin, #{
    max_requests => 1000,
    window_ms => 3600000,
    key_fun => fun(Req) ->
        case cowboy_req:header(<<"authorization">>, Req) of
            undefined -> cowboy_req:peer(Req);
            Token -> Token
        end
    end
}}
```

When a client exceeds the limit, Nova returns `429 Too Many Requests` with a `Retry-After` header.

## HTTPS and transport security

### Force HTTPS

Use `nova_force_ssl_plugin` to redirect all HTTP traffic to HTTPS:

```erlang
{pre_request, nova_force_ssl_plugin, #{
    excluded_paths => [<<"/.well-known/">>, <<"/health">>]
}}
```

### HSTS

HTTP Strict Transport Security tells browsers to always use HTTPS for your domain. Enable it via `nova_secure_headers_plugin`:

```erlang
{pre_request, nova_secure_headers_plugin, #{
    hsts => true,
    hsts_max_age => 31536000,          %% 1 year
    hsts_include_subdomains => true
}}
```

### TLS configuration

Configure Cowboy with strong TLS settings:

```erlang
{cowboy_configuration, #{
    use_ssl => true,
    ssl_port => 443,
    ssl_options => #{
        certfile => "/path/to/cert.pem",
        keyfile => "/path/to/key.pem",
        versions => ['tlsv1.3', 'tlsv1.2'],
        honor_cipher_order => true
    }
}}
```

## Secure headers

Nova's `nova_secure_headers_plugin` sets defensive HTTP headers on every response:

| Header | Default | Protection |
|--------|---------|------------|
| `x-frame-options` | `DENY` | Clickjacking |
| `x-content-type-options` | `nosniff` | MIME sniffing |
| `x-xss-protection` | `1; mode=block` | Reflected XSS (legacy browsers) |
| `referrer-policy` | `strict-origin-when-cross-origin` | Information leakage via Referer |
| `permissions-policy` | `geolocation=(), camera=(), microphone=()` | Browser feature restriction |

Enable with all protections:

```erlang
{pre_request, nova_secure_headers_plugin, #{
    hsts => true,
    csp => <<"default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; font-src 'self'">>
}}
```

## Error handling and information leakage

In development, detailed error pages with stack traces help you debug. In production, they help attackers. See the [Error Handling](../building-api/error-handling.md) chapter for the full picture — the security essentials are:

```erlang
%% Production — generic error pages, no stacktraces
{nova, [
    {environment, prod},
    {use_stacktrace, false},
    {render_error_pages, true}
]}
```

```admonish warning
Never deploy with `{environment, dev}` or `{use_stacktrace, true}`.
```

Never log passwords, tokens, or PII:

```erlang
%% GOOD — structured, no secrets
logger:info(#{msg => "user_login", user_id => UserId}).

%% BAD — leaks credentials
logger:info("Login: ~p with password ~p", [Email, Password]).
```

## File serving

Nova's `nova_file_controller` blocks path traversal by rejecting `..` and `.` segments in file paths. Configure static file routes securely:

```erlang
{"/static/[...]", "priv/static", #{
    list_dir => false,              %% Never expose directory listings
    index_files => ["index.html"]
}}
```

When accepting file uploads:

1. **Validate MIME types** against an allowlist — never trust the client-supplied `Content-Type`.
2. **Limit file sizes** via Cowboy's body reading options.
3. **Store uploads outside the web root** to prevent direct execution.
4. **Generate random filenames** to prevent path traversal via crafted filenames.

## WebSocket security

Nova WebSocket connections go through the same plugin and [security handler](../auth-sessions/authentication.md) chain as HTTP requests — so authentication works the same way.

### Authenticate connections

```erlang
#{prefix => "/ws",
  security => fun blog_auth:session_auth/1,
  routes => [
      {"/chat", {blog_ws_handler, []}, #{protocol => ws}}
  ]}
```

### Validate incoming messages

All incoming WebSocket messages are untrusted input. Validate and size-limit them:

```erlang
websocket_handle({text, RawMsg}, State) ->
    case thoas:decode(RawMsg) of
        {ok, #{<<"type">> := <<"chat">>, <<"body">> := Body}}
          when is_binary(Body), byte_size(Body) =< 4096 ->
            handle_chat(Body, State);
        _ ->
            {reply, {text, <<"{\"error\":\"invalid message\"}">>}, State}
    end.
```

## Recommended plugin order

The order of plugins matters. Here is a recommended configuration for production:

```erlang
{plugins, [
    %% 1. Force HTTPS first
    {pre_request, nova_force_ssl_plugin, #{
        excluded_paths => [<<"/health">>]
    }},
    %% 2. Security headers on every response
    {pre_request, nova_secure_headers_plugin, #{
        hsts => true,
        csp => <<"default-src 'self'">>
    }},
    %% 3. Rate limiting before expensive processing
    {pre_request, nova_rate_limit_plugin, #{
        max_requests => 100,
        window_ms => 60000
    }},
    %% 4. Correlation ID for request tracing
    {pre_request, nova_correlation_plugin, #{}},
    %% 5. Parse request body
    {pre_request, nova_request_plugin, #{
        decode_json_body => true,
        read_urlencoded_body => true,
        parse_qs => true
    }},
    %% 6. CSRF validation (must be after request_plugin)
    {pre_request, nova_csrf_plugin, #{
        excluded_paths => [<<"/api/">>]
    }},
    %% 7. CORS for API routes
    {pre_request, nova_cors_plugin, #{
        allow_origins => <<"https://app.example.com">>
    }}
]}
```

## Production security checklist

Before deploying a Nova application:

- [ ] `environment` set to `prod`, `use_stacktrace` set to `false`
- [ ] HTTPS enforced via `nova_force_ssl_plugin`
- [ ] HSTS enabled via `nova_secure_headers_plugin`
- [ ] CSP configured for your application's needs
- [ ] CSRF protection enabled for all cookie-authenticated routes
- [ ] CORS origins explicitly allowlisted (no wildcards)
- [ ] Rate limiting on sensitive endpoints (login, registration, password reset)
- [ ] Session cookies: `http_only`, `secure`, `same_site` all set
- [ ] Session rotation on authentication (`nova_session:rotate/1`)
- [ ] Session timeouts configured (`session_max_age`, `session_idle_timeout`)
- [ ] File uploads validated (MIME type, size, filename)
- [ ] Directory listing disabled for static file serving
- [ ] No `os:cmd/1` — use `open_port/2` with explicit args
- [ ] No `binary_to_term/1` on untrusted input
- [ ] No `binary_to_atom/2` on untrusted input — use `binary_to_existing_atom/2`
- [ ] All database queries parameterized (Kura or explicit `$N` placeholders)
- [ ] Changeset `cast/3` fields explicitly whitelisted
- [ ] Authorization derived from server-side session, not client input
- [ ] WebSocket messages validated and size-limited
- [ ] Sensitive data not logged or stored in sessions
- [ ] Custom error pages configured (no stacktrace leakage)
- [ ] Erlang distribution secured or disabled if not needed

---

With security covered, let's look at [deployment](deployment.md).
