# Operations

The last chapter of this part is about living with jobs: testing them, watching them, and keeping the jobs table from growing forever.

## Testing jobs

The previous chapters already used Shigoto's test helpers. Here they are in one place. The [Testing](../testing/unit-testing.md) part of the book builds on these suites.

### Three ways to run a job in a test

| Tool | Database row | Runs the worker | Use it for |
|---|---|---|---|
| `shigoto:perform_job/2,3` | no | yes, now, in a fresh process | Testing a worker's logic |
| `manual` testing mode | no | no | Testing that code enqueues the right job |
| `inline` testing mode | no | yes, inside `shigoto:insert/1` | End-to-end tests that should not care about queues |
| no testing mode + `shigoto:drain_queue/1` | yes | yes, when the test drains | Retries, scheduling, uniqueness, dependencies |

`shigoto:perform_job(Worker, Args)` runs `perform/1` through the same middleware path production uses and returns its result. `perform_job/3` takes job context as a third argument: `attempt`, `max_attempts`, `deps_results`, `queue`, `id`, `meta`, `tags`, `priority`, `timeout`. Retries, rate limits and circuit breakers are not involved.

`manual` and `inline` are testing modes for `shigoto:insert/1,2` and `insert_all/1,2`. Because they stop jobs from being stored, switching one on takes two settings. The blog's fixture, in `test/blog_jobs_fixtures.erl`:

```erlang
testing(disabled) ->
    ok = application:unset_env(shigoto, testing),
    ok = application:unset_env(shigoto, testing_confirm_no_persistence),
    shigoto_testing:reset();
testing(Mode) when Mode =:= inline; Mode =:= manual ->
    ok = application:set_env(shigoto, testing, Mode),
    ok = application:set_env(shigoto, testing_confirm_no_persistence, true),
    shigoto_testing:reset().
```

The second key is a safety catch. A `testing` value that leaks into a production config without its confirmation is ignored (with an error in the log), and jobs are stored as usual:

```erlang
testing_mode_needs_the_confirmation_key(_Config) ->
    ok = application:set_env(shigoto, testing, manual),
    disabled = shigoto_config:testing_mode(),
    {ok, #{id := Id}} = blog_jobs:schedule_publish(#{id => 0}, {{2030, 1, 1}, {0, 0, 0}}),
    {ok, #{state := ~"available"}} = shigoto:get_job(Id),
    ok = application:set_env(shigoto, testing_confirm_no_persistence, true),
    manual = shigoto_config:testing_mode().
```

`shigoto_testing:reset/0` empties the `manual` buffer. The buffer belongs to the process that inserted: a job enqueued by a controller process (an HTTP request in a test) is not in the test's buffer. In `inline` mode a worker that returns `{error, Reason}` raises `{shigoto_inline_job_failed, Worker, Reason}` out of the insert, so failures are loud.

### Assertions

`shigoto_testing:assert_enqueued/1`, `refute_enqueued/1` and `all_enqueued/0,1` take a filter of `worker`, `queue`, `args` (the job's args contain these), `tags` and `state`; an unknown key raises, so a typo cannot match everything. In `manual` mode they look at the buffer. With no testing mode they query the jobs table, which is how `api_comment_is_mailed_by_the_queue` ([Transactional Email](../email/transactional-email.md#testing-mail-sent-from-jobs)) checks a job enqueued by a request. In `inline` mode nothing is kept, and they always see an empty list.

### The database and the sandbox

The blog's tests run inside Kura's sandbox ([CRUD and the Query Builder](../data-layer/crud.md#testing-against-the-database)), which rolls back everything at the end of each test. Shigoto does not go through Kura, so its rows are **not** in the sandbox: a job inserted during a test is committed for real. Two consequences:

- Each jobs suite empties the table before checking out the sandbox (a query outside the sandbox goes straight to the pool):

  ```erlang
  reset_jobs() ->
      {ok, _} = blog_repo:query(~"DELETE FROM shigoto_jobs", []),
      ok.
  ```

  ```erlang
  init_per_testcase(_Case, Config) ->
      ok = blog_jobs_fixtures:reset_jobs(),
      blog_jobs_fixtures:capture_mail(),
      blog_data_fixtures:checkout(),
      ...
  ```

- A worker run by `drain_queue/1` reads blog data through Kura, and the sandbox is checked out in `shared` mode, so the worker sees the posts and comments the test created even though they are never committed.

Because the test config runs no queues, jobs only run when a test calls `shigoto:drain_queue(Queue)`. It claims due jobs in that queue one at a time and runs them in the calling process, including jobs that become claimable along the way (a dependent whose predecessor just finished).

```admonish bug title="drain_queue/1 can crash with two due jobs in the queue"
Shigoto 1.9.12 claims jobs with `UPDATE ... WHERE id IN (SELECT ... LIMIT n FOR UPDATE SKIP LOCKED)`. Depending on the query plan PostgreSQL picks, that can claim more than `n` jobs. On the blog's test database, with two due jobs in a queue, asking for one claimed both every time. `drain_queue/1` only expects one, so it crashes with `case_clause` and leaves both jobs `executing`. (Queue processes are affected too: they can run more jobs at once than their concurrency.)

Until that is fixed, arrange for one due job per queue when you drain. That is why `job_is_discarded_after_max_attempts` creates its comment in `manual` mode: otherwise `add_comment/3` would put a second notification next to the one the test inserts.
```

To see a real queue process at work, a test can start one on the fly. From `test/blog_jobs_ops_SUITE.erl`:

```erlang
queue_process_runs_jobs_in_the_background(_Config) ->
    Alice = blog_data_fixtures:user(~"alice"),
    Bob = blog_data_fixtures:user(~"bob"),
    Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Hello"}),
    ok = application:set_env(shigoto, poll_interval, 100),
    {ok, _Pid} = shigoto:add_queue(~"mailers", 2),
    try
        {ok, _} = blog_posts:add_comment(Post, Bob, #{~"body" => ~"Hi"}),
        #hikyaku_email{to = [#hikyaku_address{address = ~"alice@example.com"}]} =
            blog_jobs_fixtures:receive_email()
    after
        ok = shigoto:remove_queue(~"mailers"),
        application:unset_env(shigoto, poll_interval)
    end.
```

The four jobs suites:

```shell
rebar3 ct --suite=test/blog_jobs_workers_SUITE,test/blog_jobs_ops_SUITE,test/blog_email_SUITE,test/blog_email_notifications_SUITE
```

```
%%% blog_jobs_workers_SUITE: ...............
%%% blog_jobs_ops_SUITE: ........
%%% blog_email_SUITE: .......
%%% blog_email_notifications_SUITE: ...
All 33 tests passed.
```

Failing jobs log `job failed` and `job discarded` warnings during the run; that is the retry tests doing their job.

## Health

`shigoto:health/0` counts jobs by state, looks for stale jobs (executing, but silent for longer than twice the heartbeat interval) and checks that every configured queue process is alive. In the dev shell:

```erlang
1> shigoto:health().
{ok,#{status => ok,pool => blog_repo,
      queues =>
          #{<<"default">> => healthy,<<"mailers">> => healthy,
            <<"maintenance">> => healthy},
      stale_jobs => 0,
      counts =>
          #{total => 2,cancelled => 0,discarded => 0,available => 1,
            executing => 0,completed => 1,retryable => 0}}}
```

`status` is `degraded` when there are stale jobs or a queue is down, and the answer is `{error, #{status => unhealthy, ...}}` when the database cannot be reached.

The blog serves it under the token-protected `/ops` prefix from [Plugins & Middleware](../foundations/plugins.md). A group in `src/blog_router.erl`, before the last (static files and error pages) group:

```erlang
#{
    prefix => "/ops",
    security => fun blog_ops_security:require_token/1,
    routes => [
        {"/jobs", fun blog_jobs_controller:health/1, #{methods => [get]}}
    ]
},
```

and `src/controllers/blog_jobs_controller.erl`:

```erlang
-module(blog_jobs_controller).

-export([health/1]).

health(_Req) ->
    case shigoto:health() of
        {ok, #{status := Status, counts := Counts, queues := Queues}} ->
            {json, #{status => Status, counts => Counts, queues => Queues}};
        {error, #{status := Status}} ->
            {json, 503, #{}, #{status => Status}}
    end.
```

The error answer carries only the status, not the reason, which can contain connection details.

```shell
curl -s localhost:8080/ops/jobs -H 'x-ops-token: dev-ops-token'
```

```
{"status":"ok","queues":{"default":"healthy","mailers":"healthy","maintenance":"healthy"},"counts":{"total":2,"cancelled":0,"discarded":0,"available":1,"executing":0,"completed":1,"retryable":0}}
```

Without the header the answer is `401 Unauthorized`. `test/blog_jobs_ops_SUITE.erl` checks both.

For more than counts, `shigoto_dashboard` has the queries an admin page needs: `queue_stats/0`, `worker_stats/0`, `search_jobs/1`, `recent_failures/1`, `stale_jobs/0`, `batch_stats/0`.

## Telemetry and logs

Shigoto emits [telemetry](https://hex.pm/packages/telemetry) events for everything that happens to a job. The ones you are most likely to want:

| Event | Measurements | Metadata |
|---|---|---|
| `[shigoto, job, inserted]` | `count` | `job_id`, `worker`, `queue`, `attempt`, `priority` |
| `[shigoto, job, completed]` | `count`, `duration`, `duration_ms`, `queue_wait_ms` | same |
| `[shigoto, job, failed]` | `count`, `duration`, `duration_ms`, `queue_wait_ms` | same, plus `reason` |
| `[shigoto, job, snoozed]` | `count` | same, plus `snooze_reason` |
| `[shigoto, job, discarded]` | `count` | same |
| `[shigoto, queue, poll]` | `count`, `claimed` | `queue` |
| `[shigoto, testing, mode_armed]` | `count` | `mode` |

`duration` is in native time units; the `_ms` measurements are milliseconds. `worker` and `queue` are binaries. `[shigoto, testing, mode_armed]` fires at boot when a testing mode is armed, which should never happen on a production node: it is worth an alert. The full list is in the `shigoto_telemetry` module docs.

Handlers are attached with `telemetry:attach_many/4`. The test attaches one that forwards events to the test process:

```erlang
telemetry_reports_failures(_Config) ->
    blog_jobs_fixtures:mailer(#{adapter => blog_test_mail_adapter, reply => {error, smtp_down}}),
    Events = [[shigoto, job, failed], [shigoto, job, discarded]],
    ok = telemetry:attach_many(~"test-job-events", Events, fun ?MODULE:forward_event/4, self()),
    try
        Alice = blog_data_fixtures:user(~"alice"),
        Bob = blog_data_fixtures:user(~"bob"),
        Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Hello"}),
        {ok, #{id := CommentId}} = blog_posts:add_comment(Post, Bob, #{~"body" => ~"Hi"}),
        ok = shigoto:drain_queue(~"mailers"),
        receive
            {[shigoto, job, failed], #{count := 1}, #{
                worker := ~"blog_comment_notification_worker", reason := Reason
            }} ->
                {_, _} = binary:match(Reason, ~"smtp_down")
        after 1000 -> error(no_failed_event)
        end,
        shigoto_testing:assert_enqueued(#{
            args => #{~"comment_id" => CommentId}, state => ~"retryable"
        })
    after
        telemetry:detach(~"test-job-events")
    end.

forward_event(Event, Measurements, Metadata, Pid) ->
    Pid ! {Event, Measurements, Metadata}.
```

The handler is `fun ?MODULE:forward_event/4`, a reference to an exported function rather than a closure, which is what telemetry recommends (it is faster, and survives code reloads). [Observability](../production/opentelemetry.md) in Part XI connects telemetry to metrics and traces.

Shigoto also logs those lifecycle events (`job inserted`, `job completed`, `job failed`, ...). It tags them with the logger domain `[shigoto]`, and OTP's default log handler only shows events without a domain or from OTP itself, so by default you never see them. To show them, add a filter to the handler, here in the shell:

```erlang
1> logger:add_handler_filter(default, shigoto, {fun logger_filters:domain/2, {log, sub, [shigoto]}}).
ok
```

After that the dev log shows them, around the email the job sent:

```
[info] [2026-11-03T08:51:50.200643+00:00] unstructured_log="job inserted"  ()
[notice] [2026-11-03T08:51:53.089315+00:00] bcc= cc= html_body="<p>Hi alice,</p>\
...
[info] [2026-11-03T08:51:53.093435+00:00] unstructured_log="job completed"  ()
```

Shigoto sets logger metadata (`shigoto_job_id`, `shigoto_worker`, `shigoto_queue`, `shigoto_attempt`) on the process that manages a job, but `perform/1` runs in a process spawned from it, which does not inherit process metadata. To tie your worker's own log lines to a job, log the id from the args.

## Pruning and archiving

Finished jobs (completed, discarded, cancelled) stay in `shigoto_jobs` for `prune_after_days` (14). Then an hourly process moves them to `shigoto_jobs_archive`, and deletes archived jobs older than `archive_after_days` (90). Change the windows in the config if you need failed jobs for longer, or less history:

```erlang
{shigoto, [
    {repo, blog_repo},
    {prune_after_days, 7},
    {archive_after_days, 30}
]}
```

## Encrypting job arguments

Shigoto can encrypt `args` and `meta` with AES-256-GCM before storing them: set `encryption_key` to a 32-byte key, or `encryption_keys` to a list (newest first) to rotate keys. Workers see plaintext. The blog does not need it, because its jobs carry only ids; that is the better first line of defence. If a job must carry personal data, turn encryption on, and keep the key out of the code (see [Security](../production/security.md)).

## Resilience

Shigoto includes [seki](https://hex.pm/packages/seki) for per-worker protection of the things workers call. Each is an optional worker callback; a variant (compiled and run in a scratch module, not part of the blog):

```erlang
-module(blog_webhook_worker).
-behaviour(shigoto_worker).

-export([perform/1, rate_limit/0, concurrency/0, circuit_breaker/0]).

rate_limit() -> #{limit => 10, window => 1000, algorithm => token_bucket}.

concurrency() -> 3.

circuit_breaker() -> #{failure_threshold => 50, window_size => 20, wait_duration => 30000}.

perform(#{~"url" := _Url}) ->
    ok.
```

- `rate_limit/0`: at most `limit` jobs per `window` milliseconds on this node (`token_bucket`, `sliding_window`, `gcra` or `leaky_bucket`).
- `concurrency/0`: at most this many running at once on this node, across queues. `global_concurrency/0` does the same across all nodes, through PostgreSQL.
- `circuit_breaker/0`: after enough failures, stop calling a dependency that is clearly down for `wait_duration` milliseconds.

A job that hits one of these limits is snoozed, not failed, so remember that [snoozes use up attempts](background-jobs.md#snoozing). `perform_job/2` skips all of them.

## Dashboards

Two companion projects exist, neither on Hex. Both are optional git dependencies, and neither is used by the blog:

- [shigoto_board](https://github.com/Taure/shigoto_board): a live web dashboard (Nova and Datastar) with queue, worker, job and batch pages, retry and cancel. It currently pins Nova to an older git tag, so check it against your Nova version before adding it.
- [shigoto_metrics](https://github.com/Taure/shigoto_metrics): Prometheus collectors built on the telemetry events above, with a Grafana dashboard. Tag `v0.1.0`; it depends on `telemetry` and `prometheus`.

---

With email and background jobs in place, it is time to look at testing the whole application: [Unit Testing](../testing/unit-testing.md).
