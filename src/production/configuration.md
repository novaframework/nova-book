# Configuration

Nova uses standard OTP application configuration via `sys.config`. This chapter covers organizing configuration across environments, using environment variables, and the key settings you'll need.

## Configuration files

The generated project includes two config files:

- `config/dev_sys.config.src` — development settings (used by `rebar3 shell` and `rebar3 nova serve`)
- `config/prod_sys.config.src` — production settings (used when building releases)

The `.src` suffix means rebar3 processes the file as a template — `${VAR}` references are replaced with environment variables at build time.

## Development config

```erlang
[
  {kernel, [
    {logger_level, debug},
    {logger, [
      {handler, default, logger_std_h,
        #{formatter => {flatlog, #{
            map_depth => 3,
            term_depth => 50,
            colored => true,
            template => [colored_start, "[\033[1m", level, "\033[0m",
                         colored_start, "] ", msg, "\n", colored_end]
          }}}}
    ]}
  ]},
  {nova, [
         {use_stacktrace, true},
         {environment, dev},
         {cowboy_configuration, #{port => 8080}},
         {dev_mode, true},
         {bootstrap_application, blog},
         {plugins, [
                    {pre_request, nova_request_plugin, #{
                        read_urlencoded_body => true,
                        decode_json_body => true,
                        parse_qs => true
                    }}
                   ]}
        ]},
  {blog, [
         {database, <<"blog_dev">>}
        ]}
].
```

## Production config

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
         {bootstrap_application, blog},
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
        ]},
  {blog, [
      {database, <<"${DB_NAME}">>},
      {db_host, <<"${DB_HOST}">>},
      {db_user, <<"${DB_USER}">>},
      {db_password, <<"${DB_PASSWORD}">>},
      {sendgrid_api_key, <<"${SENDGRID_API_KEY}">>}
  ]}
].
```

Key differences from development:
- Logger level is `info` instead of `debug`
- `use_stacktrace` is `false` — don't leak stack traces to users
- Correlation plugin is enabled for request tracing
- CSRF plugin is enabled
- Secrets use `${VAR}` environment variable substitution
- `sendgrid_api_key` is stored under the `blog` app — the `blog_mailer` module reads it via `application:get_env/3` (see [Sending Email](../email/sending-email.md))

## Nova configuration reference

| Key | Default | Description |
|-----|---------|-------------|
| `bootstrap_application` | *(required)* | Main application to bootstrap |
| `environment` | `dev` | Current environment (`dev` or `prod`) |
| `cowboy_configuration` | `#{port => 8080}` | Cowboy listener settings |
| `plugins` | `[]` | Global middleware plugins |
| `json_lib` | `thoas` | JSON encoding library |
| `use_stacktrace` | `false` | Include stacktraces in error responses |
| `use_sessions` | `true` | Enable session management |
| `session_manager` | `nova_session_ets` | Session backend module |
| `dev_mode` | `false` | Enable development features |
| `render_error_pages` | `true` | Use custom error page controllers |
| `dispatch_backend` | `persistent_term` | Route dispatch storage backend |

## Environment-based routing

The `routes/1` function receives the environment atom:

```erlang
routes(prod) -> prod_routes();
routes(dev) -> prod_routes() ++ dev_routes().
```

This lets you add development-only routes (debug tools, test endpoints) without them leaking into production.

## VM arguments

`config/vm.args.src` controls Erlang VM settings:

```
-name blog@${HOSTNAME}
-setcookie ${RELEASE_COOKIE}
+K true
+A30
+sbwt very_long
+swt very_low
```

- `-name` — full node name (needed for clustering)
- `-setcookie` — cluster security cookie
- `+K` — enable kernel poll
- `+A` — async thread pool size
- `+sbwt` / `+swt` — scheduler busy-wait tuning

---

Next: [Observability](opentelemetry.md) — tracing, metrics, and logging in production.
