# Integration Testing

Unit tests check functions. Integration tests check that the pieces work together: a request goes through Cowboy, the router, plugins, security functions, a controller, Kura and PostgreSQL, and comes back as the response a browser or API client sees. The blog has 27 Common Test suites doing that, one or more per chapter. This chapter shows how they are put together, so you can write the next one.

## The test database

The suites run against a real PostgreSQL, started with the `docker-compose.yml` from [Database Setup](../data-layer/setup.md):

```shell
docker compose up -d
```

`config/test_sys.config` is the configuration Common Test boots the blog with. The test profile in `rebar.config` points at it (`{ct_opts, [{sys_config, "./config/test_sys.config"}]}`). The parts that differ from development:

```erlang
 {kernel, [{logger_level, warning}]},
 {blog, [
     {json_lib, blog_json},
     {ops_token, ~"test-ops-token"},
     {mailer, #{adapter => hikyaku_adapter_test}}
 ]},
 {shigoto, [
     {repo, blog_repo},
     {queues, []}
 ]},
 {nova, [
     {environment, test},
     {cowboy_configuration, #{
         port => 8081,
         ...
```

```erlang
 {kura, [
     {repos, #{
         blog_repo => #{
             backend => kura_backend_postgres,
             host => "localhost",
             port => 5432,
             database => "blog_test",
             user => "postgres",
             password => "postgres",
             pool_size => 5
         }
     }},
```

- A separate `blog_test` database. Kura's migrator creates it on the first run and `blog_app:start/2` migrates it, so there is no setup step.
- Port 8081, so the suites can run while `rebar3 nova serve` holds 8080.
- `{queues, []}`: Shigoto starts no queue processes, so a job a test enqueues stays in the table until the test runs it on purpose (see [Operations](../jobs/operations.md)).
- The mailer delivers to `hikyaku_adapter_test`; tests that want the emails point it at themselves.
- `logger_level` `warning` keeps the CT logs readable.

## Booting the blog

Each suite starts the application in `init_per_suite/1`. The shared helper is `test/blog_data_fixtures.erl`:

```erlang
start() ->
    ok = application:set_env(nova_auth, pbkdf2_iterations, 1000),
    {ok, _} = application:ensure_all_started(blog),
    [#{app := blog}] = nova_sup:get_started_applications(),
    {ok, _} = application:ensure_all_started(inets),
    kura_sandbox:start().
```

Fewer PBKDF2 iterations make creating users fast; never do that outside tests. `inets` is OTP's HTTP client, `httpc`.

The third line is there because of a real afternoon lost while writing this chapter. When the HTTP port is already taken, Nova logs `"Cowboy could not start"` with `eaddrinuse` and **the application still starts**. `ensure_all_started/1` returns `{ok, _}`, and every request then goes to whatever else owns the port: a forgotten dev node, another project, another test run. The symptoms are 404s for routes that exist or `{error, socket_closed_remotely}`. `nova_sup:get_started_applications/0` only lists an application whose listener is bound, so the match fails in `init_per_suite` with a clear `{badmatch, []}` instead.

Suites stop the application in `end_per_suite/1` with `application:stop(blog)`, so the next suite boots it fresh.

## One transaction per test: `kura_sandbox`

Tests must not see each other's rows. Kura's sandbox wraps each test case in a transaction and rolls it back afterwards:

```erlang
checkout() ->
    kura_sandbox:checkout(blog_repo, #{shared => true}).

checkin() ->
    kura_sandbox:checkin(blog_repo).
```

and every database suite uses it the same way:

```erlang
init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().
```

`shared => true` makes every process on the node use the test's connection. That is what makes HTTP tests work at all: the request is handled in a Cowboy process, a live view in its own process, and both must see the user the test just inserted, which is not committed. A `blog_repo:transaction/1` inside the sandbox becomes a savepoint, so rollback behaves as in production ([Transactions & Bulk Operations](../data-layer/transactions-bulk.md)).

The sandbox has limits, and each of them shows up in the blog's suites:

- **Only Kura queries are in it.** Shigoto talks to the pool directly, so `shigoto_jobs` rows are committed for real. Jobs suites empty the table before checking out (`blog_jobs_fixtures:reset_jobs/0`, a `DELETE FROM shigoto_jobs` on the pool, see [Operations](../jobs/operations.md#the-database-and-the-sandbox)).
- **State outside PostgreSQL is not rolled back**: sessions in `nova_session_ets`, the login rate limiter's counters, pub/sub subscriptions, the notification log. Reset what a test depends on (next sections).
- **Sequences are not rolled back**, so ids keep growing between tests. Never assert on a literal id; read it from the row you created.
- **No parallel test cases.** With one shared connection, two cases at once would share a transaction. Do not put `parallel` in a suite's groups.

## Fixtures

The fixture functions create rows through the same code the app uses, so a fixture can never be valid in a way the app would reject:

```erlang
user(Name) ->
    Params = #{
        ~"email" => <<Name/binary, "@example.com">>,
        ~"username" => Name,
        ~"password" => ~"correct horse battery",
        ~"password_confirmation" => ~"correct horse battery"
    },
    {ok, User} = blog_repo:insert(blog_user:registration_changeset(#{}, Params)),
    User.

post(Author, Params) ->
    {ok, Post} = blog_posts:create_post(Author, maps:merge(#{~"body" => ~"Body text"}, Params)),
    Post.

token(User) ->
    {ok, Token} = blog_accounts:issue_token(User),
    Token.
```

## JSON APIs with `nova_test`

`nova_test` has an HTTP client on top of `httpc` that reads the port from `cowboy_configuration`. `nova_test:start(App, Config)` starts the app and puts the port in the CT config, and its assertion macros work on the response. `test/blog_nova_test_SUITE.erl` uses it for the API from [Part V](../building-api/json-api.md):

```erlang
-module(blog_nova_test_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    nova_test:start(blog, Config).

end_per_suite(Config) ->
    nova_test:stop(Config).

posts_api_lists_published_posts(Config) ->
    Alice = ?config(alice, Config),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Out now", ~"status" => ~"published"}),
    {ok, Resp} = nova_test:get("/api/posts?status=published", Config),
    ?assertStatus(200, Resp),
    ?assertHeader("content-type", "application/json", Resp),
    ?assertJson(#{~"posts" := [#{~"title" := ~"Out now"}], ~"total_entries" := 1}, Resp).

creating_a_post_needs_a_token(Config) ->
    Post = #{json => #{~"title" => ~"Via nova_test", ~"body" => ~"Hello"}},
    {ok, Anonymous} = nova_test:post("/api/posts", Post, Config),
    ?assertStatus(401, Anonymous),
    ?assertJson(#{~"error" := ~"missing bearer token"}, Anonymous),
    Token = blog_data_fixtures:token(?config(alice, Config)),
    Authorized = Post#{headers => [{~"authorization", <<"Bearer ", Token/binary>>}]},
    {ok, Created} = nova_test:post("/api/posts", Authorized, Config),
    ?assertStatus(201, Created),
    ?assertJson(#{~"title" := ~"Via nova_test", ~"status" := ~"draft"}, Created).
```

`nova_test:start/2` is called after `blog_data_fixtures:start/0` here, because the fixture adds the listener check and the sandbox; on its own it would also start the app. `nova_test:json/1` and `?assertJson` decode with the app's `json_lib`, so the keys are binaries, as a client sees them. The request options are `json` (encoded and sent as `application/json`), `body` with `content_type`, and `headers` as binary pairs. Response headers are strings, as `httpc` returns them.

| Function or macro | Purpose |
|---|---|
| `nova_test:get/2,3`, `post/2,3`, `put/3`, `patch/3`, `delete/2,3` | `{ok, Resp}` or `{error, Reason}` |
| `nova_test:request(Method, Path, Opts, Config)` | any method |
| `nova_test:status/1`, `body/1`, `json/1`, `headers/1`, `header/2` | read a response |
| `?assertStatus(Code, Resp)` | status code |
| `?assertJson(Pattern, Resp)` | pattern-match the decoded body |
| `?assertBody(Binary, Resp)` | exact body |
| `?assertHeader(Name, Value, Resp)` | one header, case-insensitive name |

The Part II suites use two small helpers of their own in `blog_data_fixtures` that return `{Status, DecodedBody}`, which reads well in a match:

```erlang
request_as(Token, Method, Path, Json) ->
    Body = iolist_to_binary(json:encode(Json)),
    {ok, {{_, Status, _}, _, RespBody}} = httpc:request(
        Method, {url(Path), [bearer(Token)], "application/json", Body}, [], [{body_format, binary}]
    ),
    {Status, decode(RespBody)}.
```

```erlang
{201, #{~"id" := Id, ~"status" := ~"draft", ~"user_id" := AliceId}} =
    blog_data_fixtures:request_as(Token, post, "/api/posts", #{
        title => ~"My First Post", body => ~"Hello from Nova!"
    }),
```

Both styles are plain `httpc` underneath. Use whichever reads better.

## HTML forms: cookies and CSRF

The HTML side needs what `nova_test` does not do: keep cookies between requests and send the CSRF token from the form. `test/blog_web.erl` is a few functions for that. Every call takes a cookie jar (a map) and returns the updated one:

```erlang
request(Method, Jar, Path, Headers, Body) ->
    AllHeaders = cookie_header(Jar) ++ Headers,
    Request =
        case Body of
            none -> {blog_data_fixtures:url(Path), AllHeaders};
            {ContentType, Data} -> {blog_data_fixtures:url(Path), AllHeaders, ContentType, Data}
        end,
    {ok, {{_, Status, _}, RespHeaders, RespBody}} =
        httpc:request(Method, Request, [{autoredirect, false}], [{body_format, binary}]),
    {Status, RespHeaders, RespBody, update_jar(Jar, RespHeaders)}.

csrf_token(Html) ->
    {match, [Token]} = re:run(
        Html, ~"name=\"_csrf_token\" value=\"([^\"]+)\"", [{capture, all_but_first, binary}]
    ),
    Token.
```

`{autoredirect, false}` matters: a login answers `302`, and the test wants to see that redirect and the cookie set with it, not the page after it. Logging in is then a GET for the form, and a POST with the token from it:

```erlang
log_in(Email, Password) ->
    {200, _, Form, Jar} = get(#{}, "/login"),
    {302, Headers, _, Jar1} = post_form(Jar, "/login", [
        {"_csrf_token", csrf_token(Form)}, {"email", Email}, {"password", Password}
    ]),
    "/posts" = header("location", Headers),
    Jar1.
```

With it, tests read like the steps a person takes. From `blog_auth_login_SUITE`:

```erlang
login_without_csrf_token_is_403(_Config) ->
    {200, _, _, Jar} = blog_web:get(#{}, "/login"),
    {403, _, <<"Forbidden - CSRF token invalid">>, _} =
        blog_web:post_form(Jar, "/login", [{"email", "alice@example.com"}, {"password", ?PASSWORD}]).

logout_requires_csrf_token(_Config) ->
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {403, _, _, _} = blog_web:post_form(Jar, "/logout", []),
    {200, _, _, _} = blog_web:get(Jar, "/posts/new").
```

The negative tests are the important ones: the form without a token is refused, and the session survives a forged logout.

### Shared state: the rate limiter

The login limiter from [Authentication](../auth-sessions/authentication.md) counts attempts per IP, and every test comes from `127.0.0.1`. Its counters live in memory and are not part of the sandbox, so suites that log in reset it in `init_per_testcase/2`:

```erlang
init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    blog_web:reset_rate_limit(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].
```

```erlang
reset_rate_limit() ->
    try
        seki:reset(blog_login_limiter, {127, 0, 0, 1})
    catch
        error:{limiter_not_found, blog_login_limiter} -> ok
    end.
```

Without it, the sixth login of a run gets `429 Too Many Requests` and an unrelated test fails depending on the order the suites run in. `login_is_rate_limited` tests the limiter itself, and relies on the reset to start from zero.

### Logged-in sessions without a form

Live views and WebSockets need a logged-in session too, but logging in through the form on every test is slow and tests the login again. The realtime suites write the session directly, the way `nova_auth` stores it:

```erlang
login(User) ->
    {ok, SessionId} = nova_session:generate_session_id(),
    ok = nova_session_ets:set_value(
        SessionId, nova_auth_actor:session_key(), term_to_binary(actor(User))
    ),
    SessionId.

cookie(User) ->
    [{"cookie", "session_id=" ++ binary_to_list(login(User))}].
```

This depends on `nova_session_ets` being the session manager, which is Nova's default and what the test config uses. Keep at least one test that logs in through the real form.

## Running part of the suite

The whole run takes a few minutes. While working on one area, run one suite, one group or one case:

```shell
$ rebar3 ct --suite=blog_auth_login_SUITE
===> Running Common Test suites...
%%% blog_auth_login_SUITE: ............
All 12 tests passed.
$ rebar3 ct --suite=blog_realtime_ws_SUITE --group=pubsub
%%% blog_realtime_ws_SUITE: ......
All 6 tests passed.
$ rebar3 ct --suite=blog_auth_login_SUITE --case=login_is_rate_limited
%%% blog_auth_login_SUITE: .
All 1 tests passed.
```

`--suite` takes the module name or the path (`test/blog_auth_login_SUITE`). Several can be given separated by commas. When a run fails, rebar3 writes the failing cases to `_build/test/logs/retry.spec`, and `rebar3 ct --retry` runs only those. The HTML logs in `_build/test/logs/index.html` have every case's output, including log lines from the app at `warning` and above, so a crash in a controller shows up next to the test that caused it.

```admonish tip title="Two test runs on one machine"
The test config uses fixed ports, so two runs at once (two checkouts, or a run while another project's tests use 8081) collide. Copy the config, change the ports, and pass it on the command line. `--sys_config` replaces the one in `ct_opts`:

    sed 's/port => 5432/port => 55432/; s/port => 8081/port => 58081/' \
      config/test_sys.config > /tmp/blog_test.config
    rebar3 ct --sys_config=/tmp/blog_test.config

This book's examples were tested that way, with several suites running side by side against separate PostgreSQL containers. Remember the listener check above when a port is taken anyway.
```

## Coverage

`--cover` records which lines the tests ran. Collect it for both EUnit and Common Test, then print the report:

```shell
rebar3 eunit --cover
rebar3 ct --cover
rebar3 cover --verbose
```

```
  |--------------------------------------|------------|
  |                              module  |  coverage  |
  |--------------------------------------|------------|
  |             blog_account_controller  |      100%  |
  |           blog_account_email_worker  |      100%  |
  |                       blog_accounts  |       76%  |
  ...
  |            blog_fallback_controller  |       40%  |
  ...
  |                 blog_live_dashboard  |       55%  |
  ...
  |       m20261103072001_update_schema  |        0%  |
  ...
  |--------------------------------------|------------|
  |                               total  |       91%  |
  |--------------------------------------|------------|
  coverage calculated from:
    .../_build/test/cover/ct.coverdata
    .../_build/test/cover/eunit.coverdata
  cover summary written to: .../_build/test/cover/index.html
```

The HTML version marks every line. Two things in the output are expected. `rebar3 cover` prints `Cover compilation failed: {no_file_attribute, ...}` for each `*_dtl` module, because ErlyDTL's compiled templates have no source file to map lines to; templates are covered by the page tests instead. Migrations show 0%, because the test database is already migrated when the run starts. Read the low numbers as questions: `blog_fallback_controller` at 40% means the `not_found` and catch-all clauses never ran, which is worth a test; a 100% module can still be missing the case that matters.

## Static checks

Tests only check what someone thought to test. Four more checks find mistakes nobody wrote a test for, and they are fast enough to run before every push:

```shell
$ rebar3 fmt --check
Checking formatting...
All matched files use erlfmt code style!
$ rebar3 xref
===> Running cross reference analysis...
$ rebar3 kura check
===> kura: no schema drift
$ rebar3 kura lint_migrations
===> kura: no unsafe migration operations
```

- `fmt --check` fails on any file erlfmt would change (run `rebar3 fmt` to fix it). Generated migrations and generator output need it once, see [Schemas & Migrations](../data-layer/schemas-migrations.md).
- `xref` reports calls to functions that do not exist and unused local functions, with the checks listed in `xref_checks`. `rebar3 as test xref` includes the test modules too.
- `kura check` compares the schemas with the migrations and exits non-zero when a schema changed without a migration.
- `kura lint_migrations` flags drops, renames and column type changes that break a running old version during a deploy ([Schemas & Migrations](../data-layer/schemas-migrations.md#rebar3-kura-lint_migrations)).

```admonish warning title="Run kura check before compiling"
The blog runs `kura compile` as a pre-compile hook, so `rebar3 compile` (and `ct`, `eunit`, anything that compiles) *writes* the missing migration before `kura check` gets to look. After a compile, `kura check` always says "no schema drift". In CI, run `rebar3 kura check` first (it works on a fresh checkout), and fail the build if compiling left new or changed files behind.
```

### Dialyzer

`rebar3 dialyzer` checks types across modules and dependencies. The first run builds a PLT (a cache of type information) for OTP and every dependency, so it is the slow one; later runs only analyse what changed. On the blog it reports eight warnings today:

```
src/blog_stats.erl
Line 48 Column 1: Function latest_post_per_author/0 has no local return
Line 58 Column 45: The call kura_query:from(<<114,97,110,107,101,100,95,112,111,115,116,115>>) breaks the contract (atom() | module()) -> #kura_query{}
Line 62 Column 1: Function activity_feed/1 has no local return
Line 70 Column 25: The call kura_query:from(<<102,101,101,100>>) breaks the contract (atom() | module()) -> #kura_query{}
Line 120 Column 1: Function run/1 will never be called

src/controllers/blog_posts_controller.erl
Line 47 Column 17: The pattern {'error', 'stale'} can never match the type {'error','read_only' | #kura_changeset{...}}

src/controllers/blog_posts_html_controller.erl
Line 68 Column 9: The pattern {'error', 'stale'} can never match the type {'error','read_only' | #kura_changeset{...}}

src/live/blog_live_editor.erl
Line 111 Column 9: The pattern {'error', 'stale'} can never match the type {'error','read_only' | #kura_changeset{...}}
===> Warnings occurred running dialyzer: 8
```

All eight come from type specs in Kura 2.25 that are narrower than Kura's code. `kura_repo_worker:update/2` returns `{error, stale}` when optimistic locking finds a newer row ([Data Safety](../data-layer/data-safety.md)), and the tests prove it does, but its spec leaves it out, so Dialyzer concludes the blog's `{error, stale}` clauses are dead. `kura_query:from/1` is specced for schema modules, while a CTE (in [Advanced Queries](../data-layer/advanced-queries.md)) needs a binary name, which the query record accepts. Dialyzer trusts the specs and so decides those functions can never return.

That is the usual way to read Dialyzer output: each warning is a disagreement between code and a spec, and the question is which one is wrong. Here it is the library's specs, and the fix belongs upstream. Silencing the warnings in the blog with `-dialyzer` attributes would also hide the next real one, so the blog leaves them visible. Run Dialyzer locally after changing types or upgrading Kura, and compare the list. It is not a CI gate until the count is zero.

## Test your production configuration

Every suite above runs with `config/test_sys.config`, so none of them notices a typo in `config/prod_sys.config.src` or a production setting that was lost (`use_stacktrace` back on, a debug log level, a missing cookie flag). Treat the production config as code with its own small suite: read the file, check the settings that matter for security, and fail the build when one changes. [Part XI](../production/configuration.md) adds such a guard suite for the blog.

## Continuous integration

`.github/workflows/examples.yml` in the book's repository runs all of the above on every pull request that touches the companion app:

```yaml
jobs:
  blog:
    runs-on: ubuntu-24.04
    timeout-minutes: 30

    services:
      postgres:
        image: postgres:17
        env:
          POSTGRES_PASSWORD: postgres
        ports:
          - 5432:5432
        options: >-
          --health-cmd "pg_isready -U postgres"
          --health-interval 5s
          --health-timeout 5s
          --health-retries 10

    defaults:
      run:
        working-directory: examples/blog

    steps:
      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1

      - uses: erlef/setup-beam@54075bcc5e249e4758d363f27d099f55d843f124 # v1.24.1
        with:
          otp-version: "29"
          rebar3-version: "3.27"

      - name: Schemas match migrations
        run: rebar3 kura check

      - name: Compile
        run: rebar3 compile

      - name: Nothing generated by the build
        run: |
          git status --porcelain -- .
          test -z "$(git status --porcelain -- .)"

      - name: Formatting
        run: rebar3 fmt --check

      - name: Cross reference
        run: rebar3 xref

      - name: Migration safety
        run: rebar3 kura lint_migrations

      - name: EUnit
        run: rebar3 eunit

      - name: Common Test
        run: |
          epmd -daemon
          rebar3 ct
```

Notes on the recipe:

- The PostgreSQL service listens on 5432 with the password from `test_sys.config`, so the suites run unchanged. The health check makes the job wait until the database accepts connections.
- OTP 29, because Arizona requires it.
- Actions are pinned to commit SHAs with the version as a comment. A tag can be moved to other code; a SHA cannot. Dependabot understands this form and still proposes upgrades.
- `kura check` runs before anything compiles, and the step after `compile` fails if the build generated a migration or changed a tracked file.
- `epmd -daemon` is for `broadcast_reaches_other_nodes`, which starts a second node with `peer` ([Testing Real-Time](testing-realtime.md#across-nodes)).
- A last step (not shown) uploads `_build/test/logs` when the job fails, so the CT logs can be read from the run page.

To add Dialyzer once its warnings are gone, add a step with `rebar3 dialyzer`, and cache `_build/default/*_plt` keyed on `rebar.lock` so the PLT is not rebuilt on every run.

---

Next: [Testing Real-Time](testing-realtime.md), WebSockets, pub/sub and live views.
