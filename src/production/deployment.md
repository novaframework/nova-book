# Deployment

This chapter turns the blog into an OTP release, boots it against Postgres with nothing but environment variables, and packages it in a Docker image that runs as an unprivileged user. Along the way it adds a readiness endpoint and looks at what happens at boot (migrations) and at shutdown (draining connections and live sessions).

## Releases

A release is a self-contained directory with your applications, their dependencies, the Erlang runtime (ERTS) and a start script. The target machine needs no Erlang installation. relx, built into rebar3, builds it from the `relx` section of `rebar.config`:

```erlang
{relx, [
    {release, {blog, git}, [
        blog,
        sasl
    ]},
    {mode, dev},
    {sys_config_src, "./config/dev_sys.config.src"},
    {vm_args_src, "./config/vm.args.src"}
]}.

{profiles, [
    {prod, [
        {relx, [
            {mode, prod},
            {sys_config_src, "./config/prod_sys.config.src"},
            {vm_args_src, "./config/prod_vm.args.src"}
        ]}
    ]},
```

`{mode, prod}` includes ERTS, strips debug info and copies files instead of symlinking them. `{blog, git}` takes the release version from the latest git tag; with no tags (or no `.git` directory, as in a Docker build) it falls back to `0.0.0`. Tag your releases.

```admonish bug title="rebar3 nova release"
`rebar3 nova release` (rebar3_nova 0.13.4) first writes `priv/assets/openapi.json` and a Swagger page that loads its scripts from a CDN, then crashes with `{case_clause,["prod"]}` inside rebar3 3.27. Use relx directly, as below.
```

## Building

```shell
rebar3 as prod tar
```

```
===> Verifying dependencies...
===> Running erlydtl...
===> kura: schemas up to date
===> Analyzing applications...
===> Compiling blog
===> Assembling release blog-0.0.0...
===> Release successfully assembled: _build/prod/rel/blog
===> Building release tarball blog-0.0.0.tar.gz...
===> Tarball successfully created: _build/prod/rel/blog/blog-0.0.0.tar.gz
```

`rebar3 as prod release` stops after "Release successfully assembled"; `tar` also packs the directory into a 35 MB tarball you can copy to a server. Build on the same OS family and glibc version as the machine that runs it: the release includes ERTS, which is linked against the build machine's libc and OpenSSL.

## Running the release

Unpack it anywhere and give it its environment. Every `${VAR}` from [Configuration](configuration.md) must be set; the numbers have defaults.

```shell
mkdir -p /opt/blog && tar -xzf blog-0.0.0.tar.gz -C /opt/blog
cd /opt/blog
export DB_HOST=localhost DB_PORT=5432 DB_NAME=blog_prod DB_USER=blog DB_PASSWORD=...
export KURA_ENCRYPTION_KEY=$(openssl rand -base64 32)   # once; store it, do not regenerate
export BLOG_OPS_TOKEN=$(openssl rand -hex 24)
export BLOG_BASE_URL=https://blog.example MAIL_FROM=noreply@blog.example
export MAILGUN_API_KEY=... MAILGUN_DOMAIN=mg.blog.example
export RELEASE_COOKIE=$(openssl rand -hex 24)
bin/blog daemon
```

The start script's main commands:

| Command | Does |
|---|---|
| `bin/blog foreground` | Runs in the foreground, logs to stdout. Use this under systemd, Docker or Kubernetes |
| `bin/blog daemon` | Runs in the background, logs to `log/erlang.log.N` |
| `bin/blog ping` | `pong` if the node is up |
| `bin/blog eval 'Expr'` | Evaluates an expression in the running node |
| `bin/blog remote_console` | A shell in the running node |
| `bin/blog stop` | Stops the node cleanly |

The remote commands connect over Erlang distribution, so they need the same `RELEASE_COOKIE` in their environment:

```shell
bin/blog ping
bin/blog eval 'blog_repo:query(~"SELECT count(*) FROM schema_migrations", []).'
```

```
pong
{ok, [#{count => 8}]}
```

Under systemd, use `Type=simple` with `ExecStart=/opt/blog/bin/blog foreground` and put the variables in an `EnvironmentFile=` readable only by the service user. The node writes the substituted `sys.config` into `releases/0.0.0/`, so that user needs write access there (or set `RELX_OUT_FILE_PATH`).

## Migrations at boot

`blog_app:start/2` runs pending migrations before it starts the supervision tree:

```erlang
start(_StartType, _StartArgs) ->
    ok = require_encryption_key(),
    {ok, _} = kura_migrator:migrate(blog_repo),
    ...
```

On an empty database the first boot logs each one:

```
{"pid":"<0.1362.0>","time":1793698355788428,"gl":"<0.1361.0>","level":"info","text":"Kura: up migration 20261103072001 (m20261103072001_update_schema)"}
{"pid":"<0.1362.0>","time":1793698355789679,"gl":"<0.1361.0>","level":"info","text":"Kura: up migration 20261103072100 (m20261103072100_alter_posts)"}
...
{"pid":"<0.1362.0>","time":1793698355821242,"gl":"<0.1361.0>","level":"info","text":"Kura: up migration 20261103081738 (m20261103081738_create_user_tokens)"}
```

Later boots find nothing to do. Note the order in the log: `nova` is a dependency of `blog`, so its listener is already accepting requests while the migrations run. `/health` passes as soon as the database answers, even halfway through a migration, which is one more reason to keep slow migrations out of the boot (below).

If the migration fails, `{ok, _} =` does not match, the application does not start, and the node exits: a release with a broken migration never serves traffic.

Two Kura settings matter in production:

- Kura takes a Postgres advisory lock around migrations, so several instances booting at once run them one at a time. `{kura, [{migration_lock_timeout, 60000}]}` makes an instance give up after a minute instead of waiting forever behind a stuck one (the default is `infinity`).
- By default the migrator first creates the database if it does not exist (`ensure_database`, default `true`). If your database user may not create databases, which is the usual production setup, create the database yourself and set `{kura, [{ensure_database, false}]}`.

Shigoto migrates its own tables when its application starts, which is before `blog_app:start/2` runs. With the database unreachable it tries 5 times, up to 5 seconds apart, so a release can take about 45 seconds to boot before it starts with its job queues held back ([Shigoto's tables](../jobs/background-jobs.md#shigotos-tables)). On Kubernetes, give the pod a `startupProbe` so the liveness probe does not restart it during that window.

Migrations that rewrite large tables lock them while they run, and the new code starts only after they finish. For those, deploy the schema change and the code that uses it separately.

## Health checks

The blog has two endpoints for orchestrators, in the first route group:

```erlang
{"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
{"/health", fun blog_health_controller:ready/1, #{methods => [get]}}
```

`/heartbeat` answers as long as the HTTP stack works, which is what a **liveness** probe should check: if it fails, restarting helps. `/health` is the **readiness** check: it runs a query, and a 503 tells the load balancer to send traffic elsewhere until the database is back, without restarting a node that is fine:

```erlang
-module(blog_health_controller).

-export([ready/1]).

ready(_Req) ->
    case blog_repo:query(~"SELECT 1", []) of
        {ok, [_Row]} -> {json, #{status => ~"ok"}};
        {error, _Reason} -> {json, 503, #{}, #{status => ~"unavailable"}}
    end.
```

```shell
curl -si localhost:8080/health
```

```
HTTP/1.1 200 OK
content-length: 15
content-type: application/json
date: Tue, 03 Nov 2026 09:33:04 GMT
server: Cowboy/Nova
vary: accept-encoding
set-cookie: session_id=NBT7swQKEwGQ3yTkaeWe5TI7s-A6DiXJwp3qwnXyAuw; Path=/; Secure; HttpOnly; SameSite=Lax

{"status":"ok"}
```

With Postgres stopped:

```shell
time curl -si localhost:8080/health | grep -E 'HTTP|status'
```

```
HTTP/1.1 503 Service Unavailable
{"status":"unavailable"}

real	0m5.013s
```

The answer takes five seconds because the query waits for a pool connection before giving up. Give the readiness probe a timeout above that (or it reports a timeout instead of the 503), and keep `/health` off the public internet: anyone who can call it can make the node run a query. Probes get a session cookie like every new visitor; nothing is stored for it.

Don't fold other checks into `/health` lightly: a readiness check that fails when the mail provider is down takes the whole blog out of the load balancer for a problem that only affects email.

`blog_prod_deploy_SUITE` tests both answers (it mocks `blog_repo:query/2` to fail) and that a missing encryption key stops the boot:

```erlang
health_is_503_without_the_database(_Config) ->
    ok = meck:new(blog_repo, [passthrough]),
    try
        ok = meck:expect(blog_repo, query, fun(_SQL, _Params) -> {error, closed} end),
        {503, #{~"status" := ~"unavailable"}} = blog_data_fixtures:request(get, "/health")
    after
        meck:unload(blog_repo)
    end.

missing_encryption_key_stops_the_boot(_Config) ->
    ok = application:stop(blog),
    {ok, Encryption} = application:get_env(kura, encryption),
    ok = application:set_env(kura, encryption, Encryption#{keys => [{1, ~""}]}),
    try
        {error, {blog, {bad_return, {_Start, {'EXIT', {Reason, _Stack}}}}}} =
            application:ensure_all_started(blog),
        ?assertEqual({missing_config, kura_encryption_key}, Reason)
    after
        ok = application:set_env(kura, encryption, Encryption),
        {ok, _} = application:ensure_all_started(blog)
    end.
```

## Docker

`examples/blog/Dockerfile` builds the release in one image and runs it in another:

```dockerfile
FROM erlang:29.1.2-slim AS build
RUN apt-get update \
    && apt-get install -y --no-install-recommends git ca-certificates \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /build
COPY rebar.config rebar.lock ./
RUN rebar3 as prod compile --deps_only
COPY config config
COPY priv priv
COPY src src
RUN rebar3 as prod tar \
    && mkdir /release \
    && tar -xzf _build/prod/rel/blog/blog-*.tar.gz -C /release

FROM debian:trixie-slim
RUN apt-get update \
    && apt-get install -y --no-install-recommends libssl3t64 libtinfo6 libstdc++6 libsctp1 ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --system --home-dir /app --shell /usr/sbin/nologin blog
WORKDIR /app
COPY --from=build --chown=blog:blog /release ./
USER blog
ENV RELX_OUT_FILE_PATH=/tmp
EXPOSE 8080
CMD ["/app/bin/blog", "foreground"]
```

and `.dockerignore` keeps local build output and tests out of the build context:

```
_build
test
log
logs
rebar3.crashdump
erl_crash.dump
docker-compose.yml
```

Every line is there for a reason:

- **OTP 29.** Arizona requires OTP 29, so the builder is `erlang:29.1.2-slim`, the same OTP release `.tool-versions` pins for local work.
- **git in the builder.** `rebar3_nova` and `rebar3_erlydtl_plugin` are project plugins fetched from git, and the slim image has no git.
- **Dependencies first.** Copying `rebar.config` and `rebar.lock` alone and compiling the dependencies gives Docker a cached layer that survives changes to `src/`.
- **Same Debian on both sides.** `erlang:29.1.2-slim` is Debian trixie; so is the runtime. ERTS from the builder is linked against trixie's glibc 2.41 and OpenSSL 3. On `debian:bookworm-slim` (glibc 2.36) the VM does not even start: `beam.smp: /lib/x86_64-linux-gnu/libc.so.6: version 'GLIBC_2.38' not found`. `ldd` on `beam.smp` and on `crypto.so` lists what the runtime needs: `libtinfo6`, `libstdc++6` and `libssl3t64`, all present in `trixie-slim` today and named here so a base image change cannot silently drop them. `libsctp1` only silences a `Failed open sctp dynamic library` warning at every boot.
- **ca-certificates.** `debian:trixie-slim` has no CA bundle. Without it every outgoing TLS connection that verifies the peer (the OTLP exporter, an email API, Postgres over TLS) fails, and `public_key:cacerts_get/0` raises instead of returning a list:

  ```
  {'EXIT',
      {{failed_load_cacerts,no_cacerts_found},
       [{pubkey_os_cacerts,get,0,
  ...
  ```

- **Not root.** The node runs as the system user `blog`. It owns the release files, and `RELX_OUT_FILE_PATH=/tmp` sends the generated `sys.config` and `vm.args` to `/tmp`, so `/app` could even be read-only.
- **`foreground`.** The VM is the container's main process, logs go to stdout, and `docker stop`'s SIGTERM reaches it directly.

Build it with `--pull`, so the base images are current (the [Security](security.md#dependencies) chapter shows what a stale base costs), and `--provenance=false`, because BuildKit's provenance attestation records build metadata you may not want in a public image:

```shell
docker build --pull --provenance=false -t nova-book-blog:prod .
```

```
...
#17 [build 9/9] RUN rebar3 as prod tar     && mkdir /release     && tar -xzf _build/prod/rel/blog/blog-*.tar.gz -C /release
#17 0.785 ===> Verifying dependencies...
...
#17 2.083 ===> Getting log of git repo failed in /build. Falling back to version 0.0.0
#17 2.159 ===> Assembling release blog-0.0.0...
#17 3.106 ===> Release successfully assembled: _build/prod/rel/blog
...
#19 naming to docker.io/library/nova-book-blog:prod done
```

The image is 238 MB, against 456 MB for the builder. The "git repo failed" line is the version fallback from above; pass the version in from your CI if you need it in the image.

Run it with an env file (the same variables as before, with `DB_HOST` pointing at the database as seen from the container):

```shell
docker run -d --name blog --env-file blog.env -p 8080:8080 -p 127.0.0.1:9464:9464 \
    --add-host=host.docker.internal:host-gateway nova-book-blog:prod
docker logs blog
```

```
Exec: /app/erts-17.1.1/bin/erlexec -noinput +Bd -boot /app/releases/0.0.0/start -mode embedded -boot_var SYSTEM_LIB_DIR /app/lib -config /tmp/sys.config -args_file /tmp/vm.args -- foreground
Root: /app
/app
{..."msg":"Starting nova","level":"notice","mfa":"nova_sup:init/1","environment":"prod"}
{..."msg":"Nova is running","level":"notice","mfa":"nova_sup:setup_cowboy/1","url":"http://0.0.0.0:8080","app":"blog","cowboy_version":"2.18.0","nova_version":"0.18.1"}
{..."level":"info","text":"Kura: up migration 20261103072001 (m20261103072001_update_schema)"}
...
```

```shell
docker exec blog id
docker exec blog /app/bin/blog ping
```

```
uid=999(blog) gid=999(blog) groups=999(blog)
pong
```

The metrics port is published on the host's loopback only. Inside the container the listener binds all interfaces (`ip => {0, 0, 0, 0}` in `prod_sys.config.src`), because opentelemetry_nova's default, `127.0.0.1`, is the container's own loopback and Docker's port forwarding could not reach it. `/metrics` has no authentication, so a plain `-p 9464:9464` would serve it on every interface of the host; point Prometheus at the host's loopback, or run it on the same Docker network and scrape `blog:9464` without publishing the port at all ([Observability](opentelemetry.md#metrics-for-prometheus)).

`bin/blog ping` works inside the container because the cookie is in the container's environment and distribution is bound to its loopback interface; nothing outside the container can reach it.

## Graceful shutdown

On SIGTERM the VM stops its applications in reverse start order, and both web layers get a chance to finish what they are doing:

1. **arizona_nova** stops first. Its `prep_stop/1` tells every live session to wind down and waits up to `{arizona_nova, [{drain_timeout, 5000}]}` milliseconds for live requests and WebSocket sessions to finish, then closes them.
2. **nova** waits `shutdown_delay` milliseconds (default 0), suspends its listeners so no new connections are accepted, waits up to `shutdown_drain_timeout` (the blog sets 10000) for open requests, then stops the listeners.
3. kura, the OpenTelemetry SDK and the rest stop after that.

`docker stop` on an idle node:

```shell
time docker stop blog
docker logs blog | tail -4
```

```
blog

real	0m0.385s
...
{..."level":"notice","text":"SIGTERM received - shutting down\n"}
{..."msg":"Suspending listeners","level":"notice","mfa":"nova_app:graceful_shutdown/0","listeners":"[nova_listener]"}
{..."msg":"Draining connections","level":"notice","mfa":"nova_app:graceful_shutdown/0","timeout_ms":10000}
{..."msg":"Stopping listeners","level":"notice","mfa":"nova_app:graceful_shutdown/0"}
```

The container exits with status 0. Two settings to align with your platform:

- The platform's grace period must be longer than the drains together: Docker's default `docker stop` timeout is 10 seconds and Kubernetes' `terminationGracePeriodSeconds` 30, while the blog may take 5 s (Arizona) + 10 s (Nova) + `shutdown_delay`. With Docker's default the node would be killed during a slow drain; use `docker stop -t 20` or lower the timeouts.
- A load balancer keeps sending new requests for a moment after it is told the instance is going away. `shutdown_delay` (for example 5000) keeps the listener open that long before suspending it, so those requests are served rather than refused.

## Clustering

The blog runs as a single node, and everything in this book works that way. If you run several:

- Give each node a routable name (`-name blog@10.0.0.5`) and bind distribution to the private interface instead of loopback, with the same `RELEASE_COOKIE`. Erlang distribution is not encrypted by default; run it on a private network only, or configure TLS distribution.
- Sessions in `nova_session_ets` are broadcast to the other nodes over `nova_pubsub`, so a reader can land on any node ([Distributed sessions](../auth-sessions/sessions.md#distributed-sessions)). Only writes are broadcast: a node that joins later does not have the sessions written before it joined.
- `nova_pubsub` uses `pg` groups, which span connected nodes, so the notifications from [Pub/Sub](../real-time/pubsub.md) published on one node reach subscribers on another.
- Migrations are serialised by Kura's advisory lock, so rolling deploys can boot several new nodes at once.
- Each node serves its own `/metrics`; Prometheus scrapes each one. The login rate limiter is per node, so the effective limit is the per-node limit times the number of nodes.

How the nodes find each other (a static list in `sys.config`, DNS) is outside the scope of this chapter, and none of the above was exercised for the book.

## Checklist

- [ ] `rebar3 ct` passes, including the `blog_prod_*` suites
- [ ] Release built with `rebar3 as prod tar` or the Dockerfile, with `--pull`
- [ ] All `${VAR}`s set from your secret store; encryption key backed up
- [ ] Database created; `ensure_database` set to `false` if the user may not create databases
- [ ] HTTPS and security headers at the proxy; `/health`, `/metrics` and the distribution port not public
- [ ] Liveness on `/heartbeat`, readiness on `/health` with a timeout above 5 s
- [ ] Platform grace period above `drain_timeout` + `shutdown_drain_timeout` + `shutdown_delay`
- [ ] `OTEL_EXPORTER_OTLP_ENDPOINT` pointing at your collector, or `traces_exporter` set to `none`
- [ ] Image rebuilt and scanned on a schedule
