# Security

This chapter goes through the blog the way an attacker would: what input reaches it, what it trusts, what it writes to logs and what it ships. For each class of problem it says what the stack (Nova 0.18.1, Kura, nova_auth, erlydtl, Arizona) does by default, where that default is wrong, and how the blog closes the gap. Two of the gaps were real bugs in the blog as the earlier parts left it; they are fixed here, with tests.

For the general Erlang guidance behind much of this, see the [ERLEF Secure Coding and Deployment Hardening Guidelines](https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/).

## Untrusted input on the BEAM

### Atoms

Atoms are never garbage collected and the atom table has a fixed size (1,048,576 by default). Code that turns request data into atoms lets anyone crash the node by sending enough distinct values:

```erlang
%% Never on request data
binary_to_atom(Value)
list_to_atom(Value)
```

The stack is careful here, and the blog follows suit:

- Nova gives you path bindings, query parameters, form fields and decoded JSON with **binary** keys and values (`#{~"id" := Id}`).
- Kura casts `enum` fields with `binary_to_existing_atom/2`, so a status of `~"anything"` is a cast error, not a new atom.
- Where the blog needs an atom from input it matches on known binaries (`~"published"`) instead of converting.

If you do need a conversion, `binary_to_existing_atom/2` only succeeds for atoms that already exist; wrap it in `try` because it raises `badarg` otherwise.

### `binary_to_term`

`binary_to_term/1` on attacker-controlled bytes can create atoms and other resources that are never freed, and it can build funs. `binary_to_term(Bin, [safe])` refuses new atoms and new external function references, but it does not make the *data* safe: a decoded term may still contain a fun of an existing module that your code later calls, or simply a shape your code does not expect. Do not decode external term format from clients at all; the blog speaks JSON (through OTP `json`) to the outside.

`nova_auth_actor:fetch/1` (nova_auth 0.2.0) uses `binary_to_term/1` without `[safe]` on the actor it cached in the session. The bytes come from the server-side session store, never from the client, so this is not exploitable as the blog uses it, but it means that whoever can write to the session backend can make the node decode arbitrary terms. Keep the session store server-side (the default `nova_session_ets` is), and do not swap in a backend that stores session data in the cookie.

### OS commands

`os:cmd/1` runs its argument through `/bin/sh`, so any user-supplied part becomes shell syntax. If you ever need an external program, use `open_port({spawn_executable, Path}, [{args, Args}])`, which passes arguments to the program without a shell. The blog runs no external commands.

## SQL

Kura builds every query with placeholders and sends the values separately; there is no string interpolation of values anywhere in the query builder. You can see it in the spans from [Observability](opentelemetry.md):

```
SELECT * FROM "posts" WHERE "status" = $1 AND "deleted_at" IS NULL ORDER BY "inserted_at" DESC, "id" DESC LIMIT $2 OFFSET $3
```

The places where you can still get it wrong:

- **Raw SQL.** `blog_repo:query/2` takes SQL and a parameter list. Keep values in the list (`blog_repo:query(~"SELECT ... WHERE id = $1", [Id])`); never build the SQL text from input. The same goes for `{fragment, SQL, Params}` inside the query builder.
- **Identifiers.** Placeholders cover values, not column names or sort directions. If you let clients choose the sort column, map their input onto a fixed list of atoms (`~"title" -> title`) rather than passing it through. The blog's list parameters are cast through a schemaless changeset (`page`, `page_size`, `status`, `tag`) and there is no client-chosen column.
- **Types.** Cast IDs before using them. `blog_posts:get_post/2` runs `kura_types:cast(id, Id)` on the path binding and answers `not_found` for anything that is not an integer.

### Mass assignment

Changesets only take the fields you list in `cast/4`. The blog lists what a user may set and puts everything else in explicitly:

```erlang
%% blog_post: an author may set these
CS = kura_changeset:cast(?MODULE, Data, Params, [title, body, status]),

%% blog_posts:create_post/2: the author comes from the authenticated actor
blog_repo:insert(kura_changeset:put_change(CS, user_id, AuthorId)).
```

`user_id` is never cast from the body, so a client cannot post as someone else, and `role` is only changed by `blog_user:role_changeset/2`, which only the admin pages call. Registration casts `email`, `username`, `full_name`, `password` and `password_confirmation`, nothing more.

## Access control

Part III built authentication and a policy module. Reviewing the routes for this chapter turned up three places where the blog still trusted too much.

### Tag writes need an admin

`POST`, `PUT` and `DELETE /api/tags` from [JSON APIs with Generators](../building-api/json-api.md) had `security => false`: anyone could create, rename or delete tags. Tags are shared across all posts, so the write routes now get their own group behind a new security function, and the read routes stay public:

```erlang
#{
    prefix => "/api",
    security => false,
    plugins => tag_plugins(),
    routes => [
        {"/tags", fun blog_tags_controller:list/1, #{methods => [get]}},
        {"/tags/:id", fun blog_tags_controller:show/1, #{methods => [get]}},
        {"/openapi.json", "openapi/openapi.json", #{mimetype => ~"application/json"}}
    ]
},
#{
    prefix => "/api",
    security => fun blog_security:require_admin_bearer/1,
    plugins => tag_plugins(),
    routes => [
        {"/tags", fun blog_tags_controller:create/1, #{
            methods => [post], extra_state => #{json_schema => "schemas/tag.json"}
        }},
        {"/tags/:id", fun blog_tags_controller:update/1, #{
            methods => [put], extra_state => #{json_schema => "schemas/tag.json"}
        }},
        {"/tags/:id", fun blog_tags_controller:delete/1, #{methods => [delete]}}
    ]
},
```

`require_admin_bearer/1` is `require_bearer/1` plus a policy check, with a JSON 403 for an authenticated non-admin:

```erlang
require_admin_bearer(Req) ->
    case require_bearer(Req) of
        {true, Actor} ->
            case blog_policy:can(Actor, manage_tags, #{}) of
                true -> {true, Actor};
                false -> forbidden(~"admins only")
            end;
        Denied ->
            Denied
    end.

forbidden(Message) ->
    Headers = #{~"content-type" => ~"application/json"},
    {false, 403, Headers, iolist_to_binary(json:encode(#{error => Message}))}.
```

and the policy module gets one more action:

```erlang
policies(manage_tags) -> [nova_auth_policy:allow_role(admin)];
```

```erlang
writes_need_a_bearer_token(Config) ->
    {401, #{~"error" := ~"missing bearer token"}} =
        blog_data_fixtures:request(post, "/api/tags", #{name => ~"otp"}),
    {401, _} = blog_data_fixtures:request(put, ?config(tag_path, Config), #{name => ~"otp"}),
    {401, _} = blog_data_fixtures:request(delete, ?config(tag_path, Config)),
    {200, #{~"tags" := [_]}} = blog_data_fixtures:request(get, "/api/tags").

writes_need_the_admin_role(Config) ->
    Author = blog_data_fixtures:token(blog_data_fixtures:user(~"alice")),
    {403, #{~"error" := ~"admins only"}} =
        blog_data_fixtures:request_as(Author, post, "/api/tags", #{name => ~"otp"}),
    {403, _} = blog_data_fixtures:request_as(Author, delete, ?config(tag_path, Config)),
    {200, #{~"name" := ~"erlang"}} = blog_data_fixtures:request(get, ?config(tag_path, Config)).
```

The existing tag tests now create an admin in `init_per_testcase` and send its token with every write.

### Drafts in the JSON API

The HTML pages hid drafts since [Authorization](../auth-sessions/authorization.md#drafts), but `GET /api/posts?status=draft` listed every draft and `GET /api/posts/:id` returned any of them. The public `/api` read group now authenticates optionally: no `Authorization` header is fine, a header with a bad token is a 401 (better than silently treating a broken client as anonymous), and a good token puts the actor in `auth_data`:

```erlang
optional_bearer(Req) ->
    case cowboy_req:header(~"authorization", Req) of
        undefined -> true;
        _ -> require_bearer(Req)
    end.
```

The list query starts from a base query scoped to the viewer, so filtering happens in the database and the page counts stay right:

```erlang
visible_to(#{role := admin}) ->
    kura_query:from(blog_post);
visible_to(#{id := UserId}) ->
    kura_query:where(
        kura_query:from(blog_post), {'or', [{status, ~"published"}, {user_id, UserId}]}
    );
visible_to(undefined) ->
    kura_query:where(kura_query:from(blog_post), {status, ~"published"}).
```

```erlang
index(Req) ->
    Params = maps:from_list(cowboy_req:parse_qs(Req)),
    Viewer = maps:get(auth_data, Req, undefined),
    case blog_posts:list_posts(blog_posts:visible_to(Viewer), Params) of
        ...
```

`show/1` does what the HTML controller does: a published post for everyone, a draft only if `blog_policy:can(Viewer, view_draft, Post)`, and 404 otherwise, so a stranger cannot even learn that the draft exists. `blog_prod_security_SUITE` checks the four viewers:

```erlang
drafts_are_hidden_from_anonymous_readers(Config) ->
    {404, #{~"error" := ~"post not found"}} =
        blog_data_fixtures:request(get, post_path(?config(draft, Config))),
    {200, _} = blog_data_fixtures:request(get, post_path(?config(published, Config))),
    {200, #{~"posts" := [#{~"title" := ~"Out now"}]}} =
        blog_data_fixtures:request(get, "/api/posts"),
    {200, #{~"posts" := [], ~"total_entries" := 0}} =
        blog_data_fixtures:request(get, "/api/posts?status=draft").

drafts_are_shown_to_their_author(Config) ->
    Token = blog_data_fixtures:token(?config(alice, Config)),
    {200, #{~"title" := ~"Secret draft"}} =
        blog_data_fixtures:request_as(Token, get, post_path(?config(draft, Config))),
    {200, #{~"posts" := [#{~"title" := ~"Secret draft"}]}} =
        blog_data_fixtures:request_as(Token, get, "/api/posts?status=draft"),
    {200, #{~"total_entries" := 2}} = blog_data_fixtures:request_as(Token, get, "/api/posts").
```

plus `drafts_are_hidden_from_other_users`, `admins_see_every_draft` and `bad_token_on_a_public_read_is_401`. Tests in Parts II and III that read a draft through the API now send the author's token.

### Live views verify the token

Arizona routes never pass through Nova's security functions, so a live view has to find the user itself. Part VI's `blog_live_middleware` calls `blog_security:session_actor/1` instead of trusting the actor `nova_auth_actor` caches in the session. Here is why: that cache is not the source of truth. `nova_auth`'s password change and reset delete the user's tokens in the database but leave the cached actor, and so does revoking a token with `blog_accounts:revoke_token/1`. A middleware that read the cache would keep a revoked session working in the live editor until the next plain HTTP request cleared it.

`session_actor/1` is the function the HTML security functions use. It checks the session's token against the database on every mount and clears the cached actor when the token is gone:

```erlang
session_user(Req) ->
    case blog_security:session_actor(arizona_req:raw(Req)) of
        {ok, Actor} -> Actor;
        {error, _} -> undefined
    end.
```

The WebSocket upgrade runs the target page's middlewares, so the same check guards the live connection:

```erlang
live_pages_reject_a_revoked_token(Config) ->
    Alice = ?config(alice, Config),
    {ok, SessionId} = nova_session:generate_session_id(),
    Token = blog_data_fixtures:token(Alice),
    ok = nova_session_ets:set_value(SessionId, ~"user_token", Token),
    ok = nova_session_ets:set_value(
        SessionId, nova_auth_actor:session_key(), term_to_binary(blog_accounts:actor(Alice))
    ),
    {200, _} = live_get("/live/editor", SessionId),
    ok = blog_accounts:revoke_token(Token),
    {302, Location} = live_get("/live/editor", SessionId),
    ?assertEqual("/login", Location),
    ?assertEqual(
        {error, not_found}, nova_session_ets:get_value(SessionId, nova_auth_actor:session_key())
    ).

live_websocket_rejects_a_revoked_token(Config) ->
    Alice = ?config(alice, Config),
    Token = blog_data_fixtures:token(Alice),
    {ok, SessionId} = nova_session:generate_session_id(),
    ok = nova_session_ets:set_value(SessionId, ~"user_token", Token),
    Cookie = [{~"cookie", <<"session_id=", SessionId/binary>>}],
    {ok, Client} = blog_ws_client:connect("/live/ws?_az_path=/live/editor", Cookie),
    blog_ws_client:close(Client),
    ok = blog_accounts:revoke_token(Token),
    ?assertEqual(
        {error, 302}, blog_ws_client:connect("/live/ws?_az_path=/live/editor", Cookie)
    ).
```

The price is one indexed token lookup per page mount and per WebSocket connect, not per event. An already open WebSocket stays open until it reconnects; if you need revocation to cut live sessions immediately, broadcast a logout over [Pub/Sub](../real-time/pubsub.md) and have the view redirect.

### Auditing the routes again

`rebar3 nova audit` after the fixes:

```shell
rebar3 nova audit
```

```
=== Security Audit ===

  Audited routes(dev) of blog. Not covered: 2 websocket/cowboy handler route(s), static files and error pages, arizona_nova routes (served before nova_router). nova_apps read from ./config/dev_sys.config.src.

  WARNINGS:
    POST /api/tokens (blog_tokens_controller) has no security
    POST /contact (blog_contact_controller) has no security
    POST /hello (blog_hello_controller) has no security
    POST /login (blog_session_controller) has no security
    POST /password/forgot (blog_account_controller) has no security
    POST /register (blog_registration_controller) has no security

  INFO:
    GET / (blog_main_controller) has no security
    ...
    GET /api/tags (blog_tags_controller) has no security
    GET /api/tags/{id} (blog_tags_controller) has no security
    ...
    GET /health (blog_health_controller) has no security
    GET /heartbeat (blog_router) has no security
    ...

  Summary: 6 warning(s), 11 info(s)
```

The tag writes are gone from the warnings, and the posts API no longer shows up at all because its read group now has a security function. What is left is intended: the credential endpoints (rate-limited, `POST /password/forgot` included), and the Part I demos. The WebSocket routes are not covered by the audit; their upgrade is checked by `blog_ws_security:same_origin/1` ([WebSockets](../real-time/websockets.md#securing-the-upgrade)). Static files are not covered either. A static route accepts every method: a `POST` to `/robots.txt` gets the same 200 and body as a `GET`, and nothing is written.

`rebar3 nova audit --env prod --config config/prod_sys.config.src` audits the routes the production config compiles; it reads `${VAR}` references from the environment. Add `--strict` to fail on any warning.

## XSS

### Templates

erlydtl autoescapes every `{{ }}`, so a post titled `<script>` renders as text. The traps are in the filters, covered in [ErlyDTL Templates](../views-templates/templates.md#escaping):

- `|safe` turns escaping off. The blog uses it only on HTML its own code built.
- `linebreaks` is escaped a second time under autoescaping, and the obvious workaround, `{{ body|escape|linebreaks|safe }}`, does not escape at all, because `escape` is a no-op when autoescaping is on. The blog splits the body into paragraphs in Erlang (`blog_view:post/1`) and loops over them in the template.
- erlydtl renders the atoms `null` and `nil` literally; `blog_view` converts values before rendering. Not a security issue, but it is the kind of surprise that makes people reach for `|safe`.

Arizona escapes text and attribute values too; only `?raw/1` splices HTML unescaped ([Template syntax](../arizona/fundamentals.md#template-syntax)).

### JSON and static files

JSON responses go out as `application/json`, which browsers do not render as HTML. Uploaded files in the contact form stay in memory and are never served back. The file handler that stores uploads on disk ignores the client's filename; if you serve uploads, serve them from a separate domain or with `content-disposition: attachment`.

### No third-party scripts

Interactive API pages such as Swagger UI are often loaded from a CDN. A page that pulls JavaScript from a CDN at view time gives that CDN (and anyone who compromises it) script access to your origin, and it sends your visitors' IP addresses to a third party. The blog serves only `openapi.json` and no Swagger page ([API Documentation](../building-api/openapi-tools.md)); every script, stylesheet and font it uses is in its own `priv/`.

### Security headers

The blog does not set `content-security-policy`, `x-content-type-options`, `x-frame-options`, `referrer-policy` or `strict-transport-security` itself. Set them at the reverse proxy that terminates TLS for you, where they cover every response including static files and errors. A CSP for the blog can be strict: `default-src 'self'` with `connect-src 'self'` for Arizona's WebSocket, since nothing loads from elsewhere. Pages that carry a token in their URL (password reset, confirmation) already send `referrer-policy: no-referrer` themselves ([Transactional Email](../email/transactional-email.md)).

## CSRF

A browser attaches the blog's cookies to any request to the blog, whichever site triggered it. Three mechanisms keep that from being useful to another site:

- **HTML forms** go through `nova_csrf_plugin` in every HTML route group (`html_plugins()` in the router). A `POST` without the token from the session gets 403 before security or the controller runs. See [CSRF protection](../foundations/plugins.md#csrf-protection).
- **The JSON API** authenticates with a bearer token in the `Authorization` header. Browsers never add that header on their own, so there is nothing for another site to ride on, and the API groups need no CSRF token. Keep it that way: never let an API route fall back to the session cookie.
- **Arizona** checks the `Origin` header on every live route and on the WebSocket upgrade, and Nova's `/ws` routes use `blog_ws_security:same_origin/1`. A page on another origin cannot open a live connection in a reader's session ([CSRF and the Origin check](../arizona/fundamentals.md#csrf-and-the-origin-check)).

The session cookie's `SameSite=Lax` (next section) is defence in depth on top: browsers do not send it on cross-site `POST`s at all.

## Sessions

Nova's session defaults are not safe for logins: the `session_id` cookie has no `HttpOnly`, `Secure` or `SameSite`, the ID comes from `rand` rather than `crypto`, a client-chosen ID is accepted and never rotated, and sessions never expire. [Sessions](../auth-sessions/sessions.md) and [Authentication](../auth-sessions/authentication.md) fix all of it, and it is not repeated here. In short:

- `blog_session_h`, a stream handler after `nova_stream_h`, replaces every new session ID with 32 bytes from `crypto:strong_rand_bytes/1` and sets the cookie with `Path=/; Secure; HttpOnly; SameSite=Lax`.
- `blog_session:log_in/2` and `log_out/1` delete the old session and issue a new ID, so a fixed session ID is useless after login.
- The session holds a database token with an expiry, not the user; every request checks it.

The production response headers show the result:

```
set-cookie: session_id=NBT7swQKEwGQ3yTkaeWe5TI7s-A6DiXJwp3qwnXyAuw; Path=/; Secure; HttpOnly; SameSite=Lax
```

`Secure` comes from `{blog, [{secure_cookies, true}]}`, the default. Browsers drop a `Secure` cookie over plain HTTP, so production must be served over HTTPS. (They treat `http://localhost` as secure, which is why development works with the flag on.) `prod_is_hardened` in the [config tests](configuration.md#guarding-the-configs-with-a-test) fails if `blog_session_h` ever ends up before `nova_stream_h`, which would let Nova's weak cookie through.

```admonish warning title="OIDC logins"
`nova_auth_oidc` 0.2.1 stores the actor after the callback without renewing the session ID. If you add OIDC, wrap the callback and renew the session as `blog_session:log_in/2` does.
```

## Secrets

The blog has five: the database password, the Kura encryption key, the ops token, the distribution cookie, and whatever API key the mailer uses. The rules:

- **Not in the repository.** `prod_sys.config.src` and `prod_vm.args.src` only have `${VAR}` references, and a test checks it. The dev and test configs have fixed values for local databases only.
- **Into the environment of the node, from your secret store.** The orchestrator (systemd's `EnvironmentFile=` with mode 0600, Kubernetes secrets, your platform's equivalent) sets the variables; see [Deployment](deployment.md).
- **Know where they end up.** relx writes the substituted `sys.config` and `vm.args` next to the release (or to `RELX_OUT_FILE_PATH`). Anyone who can read those files has every secret.
- **Empty is not a value.** An unset `${VAR}` in quotes becomes `""`. `blog_ops_security` refuses an empty expected token, and `blog_app` refuses to start without an encryption key ([Configuration](configuration.md#quote-strings-give-numbers-a-default)).
- **The cookie is root.** Anyone who can reach the distribution port with the cookie can run any code in the node. The production `vm.args` binds distribution to loopback and takes the cookie from `RELEASE_COOKIE`; never use the default `blog_cookie` outside your laptop.
- **Compare secrets in constant time.** `blog_ops_security` compares SHA-256 digests with `crypto:hash_equals/2`, so the comparison time does not reveal how much of a guess was right.

```admonish warning title="Mail relay certificates"
hikyaku 0.2.0's SMTP adapter does not verify the relay's TLS certificate, so an attacker on the network path can read the SMTP password during STARTTLS. Until a release with the fix is on Hex, prefer an HTTP adapter or a relay on a trusted network ([Sending Email](../email/sending-email.md)).
```

## Error pages and information leakage

What a client sees when something breaks:

- **Stack traces.** With `{use_stacktrace, true}` Nova logs controller crashes with the stack trace, and the 500 handler receives it. The production config sets it to `false` ([Error Handling](../building-api/error-handling.md#crashes-and-use_stacktrace)); `prod_is_hardened` checks it.
- **Nova's default 500 page** renders a body of `undefined` fields; the blog registers its own `{500, fun blog_error_controller:server_error/1, #{}}` that answers a generic message.
- **Schema validation errors.** `nova_json_schemas` with `render_errors => true` puts the whole schema and its `file:///` path on the server into the 400 body. The blog uses `render_errors => false` and reports field errors from the changeset instead.
- **Security refusals** never reach status handlers, so a security function that returns plain `false` gives an empty 401. The blog's security functions return `{false, Status, Headers, Body}` with a JSON body that says no more than "missing bearer token" or "admins only".

## Logging hygiene

Logs live longer than requests and are read by more people. What the blog does to keep secrets and personal data out of them:

- **One request line without the dangerous parts.** `blog_request_logger` logs method, path without the query string, status, duration and user id. Never headers, cookies or bodies. A test sends a token, a cookie and a secret query parameter and checks none of them reach the report ([Custom Plugins & CORS](plugins-cors.md#testing-a-log-line)).
- **No request maps in crash logs.** A `function_clause` stack trace contains the arguments of the failed call, and for a controller that is the whole request map, `authorization` and cookies included. `use_stacktrace` is off in production. The same applies to plugins that return values Nova does not accept: the crash report dumps the request, which is one more reason the blog wraps `nova_auth_rate_limit`.
- **Log level `info` in production.** At `debug`, OTP logs a progress report for every process a supervisor starts, with its start arguments. Libraries are expected to keep secrets out of them (the database pool's report shows the password as a fun, not the text), but a debug log is still the most detailed record of the node's configuration. At `info` those reports are not logged; the production config also filters the remaining OTP progress reports.
- **Traces without values.** Kura spans carry SQL with placeholders and only the number of parameters. The HTTP spans have the path, not the query string. They do have the client's IP address (`network.peer.address`), which counts as personal data under the GDPR: set your trace retention accordingly.
- **No plain-text passwords in changesets.** `blog_user` removes `password` and `password_confirmation` from a changeset's `changes` and `params` once it has validated and hashed them, valid or not. An `{error, CS}` for "email already taken" can be logged or end up in a crash report without carrying the password ([Changesets](../data-layer/changesets.md#passwords)).
- **Job arguments are stored, so keep secrets out of them.** The email jobs in Part VIII carry a user id and generate the one-time token when they run; a token in the arguments would sit in the jobs table for weeks.

## Dependencies

Your code is a small part of what runs. Three checks, all run against this blog:

**Routes**, with `rebar3 nova audit`, as above. Run it whenever the router changes.

**Updates.** `rebar3 hex outdated` should list dependencies with newer releases, but with rebar3 3.27 and rebar3_hex 7.4.0 it crashes with a `badmatch` before printing anything. Until that is fixed, read the changelogs of what is in `rebar.lock` yourself.

**Known vulnerabilities in the image.** Trivy does not read `rebar.lock`, so it says nothing about the Erlang dependencies, but it does cover the operating system packages in the Docker image from [Deployment](deployment.md). The first scan was of an image built on a `debian:trixie-slim` that had sat in the local Docker cache for months:

```shell
docker save nova-book-blog:prod -o img.tar
docker run --rm -v "$PWD/img.tar:/img.tar:ro" aquasec/trivy:0.69.3 image \
    --quiet --scanners vuln --severity HIGH,CRITICAL --input /img.tar
```

```
Report Summary

┌────────────────────────┬────────┬─────────────────┐
│         Target         │  Type  │ Vulnerabilities │
├────────────────────────┼────────┼─────────────────┤
│ /img.tar (debian 13.5) │ debian │       94        │
└────────────────────────┴────────┴─────────────────┘
...
Total: 94 (HIGH: 91, CRITICAL: 3)
```

With `--ignore-unfixed` added, the ones that already had a fixed Debian package were in util-linux and its libraries, `login`, `gzip`, `perl-base`, `libpcre2-8-0` and `libsqlite3-0`. After rebuilding with `docker build --pull`, which fetches the current base image, the same scan with `--ignore-unfixed`:

```
┌────────────────────────┬────────┬─────────────────┐
│         Target         │  Type  │ Vulnerabilities │
├────────────────────────┼────────┼─────────────────┤
│ /img.tar (debian 13.7) │ debian │        0        │
└────────────────────────┴────────┴─────────────────┘
```

Without `--ignore-unfixed` it still reports 44, all without a fix in Debian yet; most are in packages the blog never runs (`mount`, `login`), which is the argument for a smaller base image if you want that list shorter. In practice: build releases with `--pull`, and rebuild on a schedule even when the code has not changed.

For the Erlang side, follow the security advisories of the packages you depend on (GitHub's advisory database covers Hex) and keep the lock file current. The ones that matter for this blog today are listed in the warnings in this chapter.

## Transport

- Serve the blog over HTTPS only; the session cookie requires it. The usual setup terminates TLS at a reverse proxy or load balancer, which also sets HSTS and redirects HTTP. Nova can terminate TLS itself (`use_ssl`, `ssl_port`, `ssl_options` in `cowboy_configuration`), but then certificate renewal is your release's problem.
- Behind a proxy, `cowboy_req:peer/1` is the proxy. The login rate limiter and Arizona's origin check need to know that; see the warnings in [Rate limiting login](../auth-sessions/authentication.md#rate-limiting-login) and [CSRF and the Origin check](../arizona/fundamentals.md#csrf-and-the-origin-check).
- The database connection in the book runs on localhost without TLS. Across a network, turn on TLS for the Kura pool with certificate verification (`verify_peer` with `public_key:cacerts_get()`); kura_postgres passes `ssl` and `ssl_options` through to the driver.

## Checklist

| | Where |
|---|---|
| No `binary_to_atom`, `list_to_atom`, `binary_to_term` or `os:cmd` on input | code review |
| Values only through Kura placeholders; identifiers from fixed lists | code review |
| Every write route has a security function | `rebar3 nova audit` |
| Drafts and other private records filtered in the query | `blog_prod_security_SUITE` |
| Live routes check the token, not the cached actor | `blog_prod_security_SUITE` |
| CSRF plugin on every cookie-authenticated form group | router |
| Session cookie `Secure; HttpOnly; SameSite=Lax`, renewed at login | [Sessions](../auth-sessions/sessions.md) |
| Secrets only from the environment; empty values refused | `blog_prod_config_SUITE` |
| `use_stacktrace` off, log level `info`, request log without secrets | `blog_prod_config_SUITE`, `blog_prod_plugins_SUITE` |
| No CDN-loaded scripts or fonts | `priv/` |
| Security headers and HTTPS at the proxy | proxy config |
| Image rebuilt with `--pull` and scanned | [Deployment](deployment.md) |

---

Next: [Deployment](deployment.md), where the blog becomes a release and a container.
