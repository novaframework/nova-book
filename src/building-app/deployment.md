# Deployment

In development we use `rebar3 nova serve` with hot-reloading and debug logging. For production we need a proper OTP release — a self-contained package with your application, all dependencies, and optionally the Erlang runtime.

## Release basics

Rebar3 uses `relx` to build releases. The generated `rebar.config` includes a release configuration:

```erlang
{relx, [{release, {my_first_nova, "0.1.0"},
         [my_first_nova,
          sasl]},
        {dev_mode, true},
        {include_erts, false},
        {extended_start_script, true},
        {sys_config_src, "config/dev_sys.config.src"},
        {vm_args_src, "config/vm.args.src"}
       ]}.
```

This is the development release config — `dev_mode` symlinks to source, and ERTS is not included.

## Production profile

Override settings for production using a rebar3 profile:

```erlang
{profiles, [
    {prod, [
        {relx, [
            {dev_mode, false},
            {include_erts, true},
            {sys_config_src, "config/prod_sys.config.src"}
        ]}
    ]}
]}.
```

Key differences:
- `dev_mode` is `false` — files are copied into the release
- `include_erts` is `true` — the Erlang runtime is bundled
- Uses `prod_sys.config.src` with production settings

## Production configuration

`config/prod_sys.config.src`:

```erlang
[
  {kernel, [
    {logger_level, info},
    {logger, [
      {handler, default, logger_std_h,
        #{config => #{file => "log/erlang.log"},
          formatter => {flatlog, #{
            map_depth => 3,
            term_depth => 50,
            colored => false,
            template => ["[", level, "] ", msg, "\n"]
          }}}}
    ]}
  ]},
  {nova, [
         {use_stacktrace, false},
         {environment, prod},
         {cowboy_configuration, #{port => 8080}},
         {dev_mode, false},
         {bootstrap_application, my_first_nova},
         {plugins, [
                    {pre_request, nova_request_plugin, #{
                        decode_json_body => true,
                        read_urlencoded_body => true
                    }}
                   ]}
        ]},
  {my_first_nova, [
      {db, #{
          host => "${DB_HOST}",
          port => 5432,
          database => "${DB_NAME}",
          username => "${DB_USER}",
          password => "${DB_PASSWORD}"
      }}
  ]}
].
```

```admonish warning
- Logger level is `info` instead of `debug`
- `use_stacktrace` is `false` — don't leak stack traces to users
- Environment variables use `${VAR}` syntax — rebar3 substitutes these at release build time
```

## VM arguments

`config/vm.args.src` controls Erlang VM settings. For production:

```
-name my_first_nova@${HOSTNAME}
-setcookie ${RELEASE_COOKIE}
+K true
+A30
+sbwt very_long
+swt very_low
```

- `-name` instead of `-sname` for full node names (needed for clustering)
- `+sbwt` and `+swt` tune scheduler busy-wait for lower latency

## Building and running

Build a production release:

```shell
rebar3 as prod release
```

If you have JSON schemas in `priv/schemas/`, you can use `nova release` instead. It automatically regenerates the OpenAPI spec before building:

```shell
rebar3 nova release
===> Generated priv/assets/openapi.json
===> Generated priv/assets/swagger.html
===> Release successfully assembled: _build/prod/rel/my_first_nova
```

This ensures your deployed application always ships with up-to-date API documentation. See [OpenAPI & API Documentation](../developer-tools/openapi.md) for details.

Start it:

```shell
_build/prod/rel/my_first_nova/bin/my_first_nova foreground
```

Or as a daemon:

```shell
_build/prod/rel/my_first_nova/bin/my_first_nova daemon
```

Other commands:

```shell
# Check if the node is running
_build/prod/rel/my_first_nova/bin/my_first_nova ping

# Attach a remote shell
_build/prod/rel/my_first_nova/bin/my_first_nova remote_console

# Stop the node
_build/prod/rel/my_first_nova/bin/my_first_nova stop
```

## Building a tarball

For deployment to another machine:

```shell
rebar3 as prod tar
```

This creates `my_first_nova-0.1.0.tar.gz`. Since ERTS is included, the target server does not need Erlang installed:

```shell
# On the server
mkdir -p /opt/my_first_nova
tar -xzf my_first_nova-0.1.0.tar.gz -C /opt/my_first_nova
/opt/my_first_nova/bin/my_first_nova daemon
```

## SSL/TLS

Configure HTTPS in Nova:

```erlang
{nova, [
    {cowboy_configuration, #{
        use_ssl => true,
        ssl_port => 8443,
        ssl_options => #{
            certfile => "/etc/letsencrypt/live/myapp.com/fullchain.pem",
            keyfile => "/etc/letsencrypt/live/myapp.com/privkey.pem"
        }
    }}
]}
```

Alternatively, put a reverse proxy (Nginx, Caddy) in front and let it handle SSL termination. This is the more common approach.

## Systemd service

Run as a system service:

```ini
[Unit]
Description=My First Nova Application
After=network.target postgresql.service

[Service]
Type=forking
User=nova
Group=nova
WorkingDirectory=/opt/my_first_nova
ExecStart=/opt/my_first_nova/bin/my_first_nova daemon
ExecStop=/opt/my_first_nova/bin/my_first_nova stop
Restart=on-failure
RestartSec=5
Environment=DB_HOST=localhost
Environment=DB_NAME=my_first_nova_prod
Environment=DB_USER=nova
Environment=DB_PASSWORD=secret
Environment=RELEASE_COOKIE=my_secret_cookie

[Install]
WantedBy=multi-user.target
```

```shell
sudo systemctl daemon-reload
sudo systemctl enable my_first_nova
sudo systemctl start my_first_nova
```

## Docker

A multi-stage Dockerfile:

```dockerfile
FROM erlang:26 AS builder

WORKDIR /app
COPY . .

RUN rebar3 as prod tar

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y libssl3 libncurses6 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/_build/prod/rel/my_first_nova/*.tar.gz .
RUN tar -xzf *.tar.gz && rm *.tar.gz

EXPOSE 8080

CMD ["/app/bin/my_first_nova", "foreground"]
```

Build and run:

```shell
docker build -t my_first_nova .
docker run -p 8080:8080 \
  -e DB_HOST=host.docker.internal \
  -e DB_NAME=my_first_nova_prod \
  -e DB_USER=nova \
  -e DB_PASSWORD=secret \
  my_first_nova
```

## Summary

Deploying a Nova application follows standard OTP release practices:

1. Configure a production profile in `rebar.config`
2. Set up production config with proper logging and secrets
3. Build with `rebar3 as prod release` or `rebar3 as prod tar`
4. Deploy using systemd, Docker, or any process manager

OTP releases are self-contained — once built, everything you need is in a single directory or archive.

---

Our application is deployed. Now let's explore more advanced features, starting with [pub/sub](../going-further/pubsub.md).
