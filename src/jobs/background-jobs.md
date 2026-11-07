# Background Jobs

The [previous part](../email/transactional-email.md) moved email out of the request and into background jobs. This part looks at the job queue itself, [Shigoto](https://hex.pm/packages/shigoto) 1.9.12: how it is set up, what a worker can declare, what happens when a job fails, and how jobs are made unique, scheduled and tested.

Shigoto stores jobs in PostgreSQL, in the blog's own database. There is no separate broker to run. Queue processes on every node poll the jobs table and claim work with `SELECT ... FOR UPDATE SKIP LOCKED`, so two nodes never take the same job. A job runs **at least once**: if a node dies halfway through a job, the job is picked up again later. Workers should therefore be safe to run twice, which is why the blog's workers load their data by id and check its state before acting.

## Setup

### Dependency and start order

`rebar.config` has `{shigoto, "~> 1.9"}`, and `src/blog.app.src` starts it after Kura:

```erlang
{applications, [
    kernel,
    stdlib,
    nova,
    kura,
    kura_postgres,
    nova_auth,
    nova_json_schemas,
    arizona_nova,
    shigoto,
    hikyaku
]},
```

The order matters. Kura starts the `blog_repo` connection pool when the `kura` application starts, and Shigoto uses that pool from the moment it starts: its application start creates its tables and its queue processes begin polling. Listing `shigoto` after `kura` and `kura_postgres` makes sure the pool exists first. Shigoto also starts its own dependencies, `minato` (the PostgreSQL driver Kura uses) and `seki` (resilience primitives, see [Operations](operations.md#resilience)).

### Configuration

`config/dev_sys.config.src`:

```erlang
{shigoto, [
    {repo, blog_repo},
    {queues, [{~"default", 10}, {~"mailers", 5}, {~"maintenance", 1}]}
]},
```

`repo` is the name of the minato pool to use. kura_postgres names the pool after the repo, so `{repo, blog_repo}` makes Shigoto share the blog's pool and database.

```admonish note title="repo, not pool"
Shigoto's README and guides still show `{pool, my_app_db}` and setting up a pgo pool. Those are from before Shigoto moved to minato. With Kura, set `repo`; `shigoto_config:pool/0` reads `repo` first and falls back to `pool`.
```

The other settings, with their defaults (from `shigoto_config`):

| Key | Default | Meaning |
|---|---|---|
| `queues` | `[{~"default", 10}]` | Queues this node works on, with how many jobs of each run at once |
| `poll_interval` | `5000` | Milliseconds between polls of each queue |
| `shutdown_timeout` | `15000` | Milliseconds to let running jobs finish on shutdown |
| `heartbeat_interval` | `30000` | How often running jobs report they are alive; a job silent for twice this is rescued |
| `prune_after_days` | `14` | Age at which finished jobs move to the archive table |
| `archive_after_days` | `90` | Age at which archived jobs are deleted |
| `cron` | `[]` | Recurring jobs, see [Scheduling](scheduling.md#recurring-jobs) |
| `middleware` | `[]` | Functions wrapped around every job |
| `encryption_key`, `encryption_keys` | none | Encrypt job arguments at rest, see [Operations](operations.md#encrypting-job-arguments) |

The test config, `config/test_sys.config`, runs no queues at all:

```erlang
{shigoto, [
    {repo, blog_repo},
    {queues, []}
]},
```

Nothing runs in the background during tests, so a test decides exactly when jobs run (with `shigoto:drain_queue/1`, see [Operations](operations.md#testing-jobs)).

### Shigoto's tables

Shigoto creates its tables itself: `shigoto_app` calls `shigoto_migration:up/1` on the pool every time it starts. The migration is idempotent (`CREATE TABLE IF NOT EXISTS`, `ADD COLUMN IF NOT EXISTS`), and a newer Shigoto adds its new columns the same way, so there is nothing to keep in step with Kura's migrations.

There is one catch, on the very first boot. Shigoto starts before the blog, and the blog is where Kura's migrator runs and creates the database if it is missing. On a brand new database Shigoto's migration therefore runs against a database that does not exist yet. Shigoto tries 5 times, 1 to 5 seconds apart, then starts without its queues and logs `shigoto_migration_pending` at error level. From then on it retries every 30 seconds; meanwhile `shigoto:health/0` reports `migration_pending` and no jobs run.

The blog runs the migration itself once Kura's has created the database, then tells Shigoto to retry at once, in `src/blog_app.erl`:

```erlang
start(_StartType, _StartArgs) ->
    {ok, _} = kura_migrator:migrate(blog_repo),
    ok = shigoto_migration:up(blog_repo),
    _ = shigoto_migrator:retry_now(),
    ok = blog_jobs:schedule_maintenance(),
    ok = nova_handlers:register_handler(text, fun blog_text_handler:handle/3),
    blog_sup:start_link().
```

`shigoto_migration:up/1` returns `{error, Reason}` when a statement fails, so the `ok =` match stops the boot if the tables cannot be created. `shigoto_migrator:retry_now/0` returns `not_running` when nothing is pending, which is every boot after the first, so its result is ignored. On the first boot the queues start a moment after `blog_app:start/2` returns. The five attempts before that still cost a few seconds, and the error report is expected; the `shigoto_migration_completed` notice follows it. (`blog_jobs:schedule_maintenance/0` is the [recurring purge](scheduling.md#recurring-jobs).)

Why not a Kura migration that runs Shigoto's DDL? It would freeze the schema of whichever Shigoto version you had when you wrote it, while Shigoto expects to upgrade its own tables on boot. Running Shigoto's own migration at boot keeps that working.

A test checks the tables exist, in `test/blog_jobs_ops_SUITE.erl`:

```erlang
tables_are_created_at_boot(_Config) ->
    {ok, Rows} = blog_repo:query(
        ~"SELECT tablename FROM pg_tables WHERE tablename LIKE 'shigoto_%' ORDER BY tablename", []
    ),
    [~"shigoto_batches", ~"shigoto_cron", ~"shigoto_jobs", ~"shigoto_jobs_archive"] =
        [Name || #{tablename := Name} <- Rows].
```

## Workers

A worker is a module with the `shigoto_worker` behaviour and a `perform/1` callback. `perform/1` receives the job's arguments, decoded from JSON, so keys are binaries. It returns:

| Return | Meaning |
|---|---|
| `ok` | Done |
| `{ok, Result}` | Done; `Result` (JSON-encodable) is stored on the job and passed to dependent jobs |
| `{error, Reason}` | This attempt failed; retry later, or discard after the last attempt |
| `{snooze, Seconds}` | Not now; run again in `Seconds` |

An exception in `perform/1` counts as `{error, _}`, and so does running longer than the worker's timeout.

The blog's simplest worker publishes a post, in `src/workers/blog_publish_worker.erl`:

```erlang
-module(blog_publish_worker).
-behaviour(shigoto_worker).

-export([perform/1]).

perform(#{~"post_id" := PostId}) ->
    case blog_repo:get(blog_post, PostId) of
        {ok, #{status := draft} = Post} ->
            case blog_repo:update(blog_post:changeset(Post, #{status => published})) of
                {ok, _Published} -> ok;
                {error, _Changeset} -> {error, invalid_post}
            end;
        {ok, _AlreadyPublishedOrArchived} ->
            ok;
        {error, not_found} ->
            ok
    end.
```

It is safe to run twice: a post that is already published, archived or deleted is left alone.

### Optional callbacks

A worker can declare defaults for its jobs. Values given when a job is inserted take precedence.

| Callback | Default | Meaning |
|---|---|---|
| `queue/0` | `~"default"` | Queue the job goes to |
| `max_attempts/0` | `3` | Attempts before the job is discarded |
| `priority/0` | `0` | Higher priorities are claimed first within a queue |
| `timeout/0` | `300000` | Milliseconds `perform/1` may run before it is killed and the attempt fails |
| `backoff/2` | see below | Seconds to wait before the next attempt |
| `unique/0` | none | Refuse duplicates, see [Unique jobs](#unique-jobs) |
| `tags/0` | `[]` | Tags stored on the job, for filtering |
| `on_discard/2` | none | Called with the args and the error list when the job is discarded |
| `middleware/0` | `[]` | Worker-specific middleware |
| `rate_limit/0`, `concurrency/0`, `global_concurrency/0`, `circuit_breaker/0` | none | See [Resilience](operations.md#resilience) |

The comment notification worker from the previous part uses four of them:

```erlang
queue() -> ~"mailers".

max_attempts() -> 5.

backoff(Attempt, _Reason) -> 30 * Attempt.

unique() -> #{keys => [worker, args], period => infinity}.
```

## Enqueueing

`shigoto:insert/1` takes a map. Only `worker` is required:

| Key | Meaning |
|---|---|
| `worker` | The worker module |
| `args` | A JSON-encodable map; use binary keys, since that is what `perform/1` gets back |
| `queue`, `priority`, `max_attempts`, `tags` | Override the worker's defaults |
| `scheduled_at` | Do not run before this UTC datetime, see [Scheduling](scheduling.md) |
| `meta` | A map stored with the job, not passed to `perform/1` |
| `depends_on` | Job ids that must finish first, see [Scheduling](scheduling.md#dependencies) |
| `batch` | A batch id from `shigoto:new_batch/1` |

`shigoto:insert/2` takes options as well; `#{unique => ...}` overrides the worker's `unique/0`. `shigoto:insert_all/1` inserts a list of jobs in one statement.

The answer is the stored row:

```erlang
1> blog_jobs:schedule_publish(Post, {{2026, 11, 4}, {9, 0, 0}}).
{ok,#{args => <<"{\"post_id\": 1}">>,id => 3,meta => <<"{}">>,
      priority => 0,state => <<"available">>,progress => 0,
      max_attempts => 3,queue => <<"default">>,
      worker => <<"blog_publish_worker">>,result => null,
      errors => <<"[]">>,
      inserted_at => {{2026,11,3},{8,51,48}},
      tags => [],
      scheduled_at => {{2026,11,4},{9,0,0}},
      depends_on => [],batch_id => null,partition_key => null,
      attempt => 0,attempted_at => null,completed_at => null,
      discarded_at => null,unique_key => null,
      cancelled_at => null,heartbeat_at => null,
      deps_results => <<"{}">>}}
```

Note that `args`, `errors` and `meta` come back as JSON text, not as maps. `shigoto:get_job/1` returns the same shape; decode with `json:decode/1` when you need them.

What goes in `args` matters. Keep them small and put ids in them, not data: the worker should load what it needs when it runs, so it sees current data, and the jobs table does not become a second copy of your users' personal data. The blog's jobs carry `comment_id`, `post_id` and `user_id` and nothing else.

## Queues

Each queue in the `queues` config is a process on every node that lists it. It polls every `poll_interval`, claims as many available jobs as it has free slots (its concurrency minus running jobs), highest `priority` first, oldest first within a priority, and runs each in its own process.

The blog has three queues so that one kind of work cannot starve another: `default` (publishing), `mailers` (all email) and `maintenance` (one slot, for housekeeping). A burst of a thousand notifications fills `mailers` and leaves publishing alone.

Queues can be changed at runtime, on the local node:

```erlang
{ok, _Pid} = shigoto:add_queue(~"mailers", 2),
ok = shigoto:pause_queue(~"mailers"),
ok = shigoto:resume_queue(~"mailers"),
ok = shigoto:remove_queue(~"mailers").
```

A paused queue claims nothing new and lets running jobs finish; `remove_queue/1` waits for them. One test in `test/blog_jobs_ops_SUITE.erl` uses `add_queue/2` to watch a real queue process deliver a notification, see [Operations](operations.md#testing-jobs).

## When a job fails

A job moves through these states:

```
available -> executing -> completed
                       -> retryable -> (backoff) -> executing ...
                       -> discarded   (after max_attempts)
available or retryable -> cancelled   (shigoto:cancel/1)
```

When `perform/1` returns `{error, Reason}`, raises, or times out, Shigoto records the error on the job and either schedules it again (`retryable`) or, if this was the last attempt, gives up (`discarded`). The delay before the next attempt comes from the worker's `backoff(Attempt, Reason)`, or by default `Attempt^4` seconds plus up to 30 s of random jitter, capped at 30 minutes: about 2-31 s, 17-46 s, 82-111 s and so on.

The tests use `blog_test_mail_adapter` (from the [previous chapter](../email/transactional-email.md#testing-mail-sent-from-jobs)) to make delivery fail. From `test/blog_jobs_workers_SUITE.erl`:

```erlang
failed_delivery_is_retried_with_backoff(Config) ->
    blog_jobs_fixtures:mailer(#{adapter => blog_test_mail_adapter, reply => {error, smtp_down}}),
    {ok, _} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    ok = shigoto:drain_queue(~"mailers"),
    [#{id := JobId}] = shigoto_testing:all_enqueued(#{worker => blog_comment_notification_worker}),
    {ok, #{state := ~"retryable", attempt := 1, max_attempts := 5, scheduled_at := RetryAt}} =
        shigoto:get_job(JobId),
    Delay = seconds_from_now(RetryAt),
    true = Delay > 20 andalso Delay =< 30.
```

The worker's `backoff(1, _)` is 30, so the retry is due 30 seconds after the failure. With one attempt allowed, the first failure discards the job, and the error is kept:

```erlang
job_is_discarded_after_max_attempts(Config) ->
    blog_jobs_fixtures:mailer(#{adapter => blog_test_mail_adapter, reply => {error, smtp_down}}),
    blog_jobs_fixtures:testing(manual),
    {ok, #{id := CommentId}} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    blog_jobs_fixtures:testing(disabled),
    {ok, #{id := JobId}} = shigoto:insert(#{
        worker => blog_comment_notification_worker,
        args => #{~"comment_id" => CommentId},
        max_attempts => 1
    }),
    ok = shigoto:drain_queue(~"mailers"),
    {ok, #{state := ~"discarded", errors := Errors}} = shigoto:get_job(JobId),
    [#{~"error" := ~"smtp_down", ~"attempt" := 1}] = json:decode(Errors).
```

Discarded and cancelled jobs can be brought back with `shigoto:retry/1` (one job) or `shigoto:retry_by/1`, which takes a filter map of `worker`, `queue`, `tags` or `args` (containment); `shigoto:cancel_by/1` cancels waiting jobs with the same filters. Despite its documentation, `retry_by/1` does not accept `state`: `shigoto:retry_by(#{state => ~"discarded"})` crashes with `function_clause`. It only ever touches discarded and cancelled jobs anyway.

### Snoozing

`{snooze, Seconds}` puts the job back to `available`, due in `Seconds`, and records no error. The notification worker snoozes on a 429:

```erlang
snoozed_job_is_rescheduled(Config) ->
    blog_jobs_fixtures:mailer(#{
        adapter => blog_test_mail_adapter, reply => {error, #{status_code => 429}}
    }),
    {ok, _} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    ok = shigoto:drain_queue(~"mailers"),
    [#{id := JobId}] = shigoto_testing:all_enqueued(#{worker => blog_comment_notification_worker}),
    {ok, #{state := ~"available", attempt := 1, errors := ~"[]", scheduled_at := At}} =
        shigoto:get_job(JobId),
    Delay = seconds_from_now(At),
    true = Delay > 50 andalso Delay =< 60.
```

```admonish warning title="A snooze still uses up an attempt"
Look at `attempt := 1` above. Shigoto increments `attempt` every time a job is claimed, and a snooze does not give it back (the README says otherwise). A job that snoozes four times and then fails once, with `max_attempts` 5, is discarded without a retry. Give workers that snooze a generous `max_attempts`, or count snoozes yourself.
```

## Unique jobs

The notification worker declares `unique/0`, so a second notification for the same comment is refused while the first is waiting:

```erlang
duplicate_notification_is_a_conflict(Config) ->
    {ok, #{id := CommentId}} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    Job = #{worker => blog_comment_notification_worker, args => #{~"comment_id" => CommentId}},
    [#{id := JobId}] = shigoto_testing:all_enqueued(#{worker => blog_comment_notification_worker}),
    {ok, {conflict, #{id := JobId}}} = shigoto:insert(Job),
    [_] = shigoto_testing:all_enqueued(#{worker => blog_comment_notification_worker}).
```

The insert answers `{ok, {conflict, Existing}}` with the job that is already there. The options:

| Option | Default | Meaning |
|---|---|---|
| `keys` | `[worker, args]` | What makes two jobs "the same": any of `worker`, `args`, `queue` |
| `states` | `[available, executing, retryable]` | Only jobs in these states count |
| `period` | `infinity` | Only jobs inserted within this many seconds count |
| `replace` | `[]` | On conflict, update these fields of the existing job (`args`, `priority`, `max_attempts`, `scheduled_at`) |
| `debounce` | none | On conflict, push the existing job's `scheduled_at` this many seconds into the future |

The check runs under a PostgreSQL advisory lock, so two nodes inserting the same job at the same moment still produce one. `states` is the option to think about: the [recurring purge](scheduling.md#recurring-jobs) leaves `executing` out, so that a running purge can schedule its own next run.

## Transactions

`shigoto:transaction/1` runs a function in a transaction on Shigoto's pool. Every job inserted inside it commits together, or not at all:

```erlang
shigoto_transaction_groups_job_inserts(_Config) ->
    rolled_back =
        try
            shigoto:transaction(fun() ->
                {ok, _} = shigoto:insert(#{worker => blog_publish_worker, args => #{~"post_id" => 1}}),
                {ok, _} = shigoto:insert(#{worker => blog_publish_worker, args => #{~"post_id" => 2}}),
                throw(rolled_back)
            end)
        catch
            throw:rolled_back -> rolled_back
        end,
    shigoto_testing:refute_enqueued(#{worker => blog_publish_worker}).
```

What you would really like is to commit a job together with the database change it is about: the comment and its notification, in one transaction. Shigoto's documentation promises exactly that ("transactional enqueue"), but that promise dates from when Shigoto and Kura both ran on pgo. With minato, Kura and Shigoto each keep their transaction on their own connection, and neither sees the other's. Two tests pin it, in `test/blog_jobs_workers_SUITE.erl`. A job inserted inside a Kura transaction survives the rollback:

```erlang
enqueue_inside_kura_transaction_is_not_rolled_back(_Config) ->
    {aborted, _Job} =
        try
            blog_repo:transaction(fun() ->
                {ok, Job} = shigoto:insert(#{
                    worker => blog_publish_worker, args => #{~"post_id" => 1}
                }),
                throw({aborted, Job})
            end)
        catch
            throw:Aborted -> Aborted
        end,
    shigoto_testing:assert_enqueued(#{worker => blog_publish_worker, state => ~"available"}).
```

and a Kura write inside a Shigoto transaction survives its rollback (here, soft-deleting a post):

```erlang
shigoto_transaction_does_not_cover_kura_writes(Config) ->
    #{id := PostId} = ?config(post, Config),
    rolled_back =
        try
            shigoto:transaction(fun() ->
                {ok, _} = blog_posts:delete_post(?config(post, Config)),
                throw(rolled_back)
            end)
        catch
            throw:rolled_back -> rolled_back
        end,
    {error, not_found} = blog_repo:get(blog_post, PostId).
```

```admonish bug title="No transactional enqueue with Kura in Shigoto 1.9.12"
Do not rely on `shigoto:insert/1` inside `blog_repo:transaction/1` or `kura_multi`, nor on `shigoto:transaction/1` to cover Kura writes. If the test above starts failing after an upgrade, the two have learned to share a transaction.
```

The blog works with what is there:

- **Enqueue after the commit.** `add_comment/3` inserts the comment, and only then the job. If the node dies between the two, one notification is lost, which is acceptable for a notification.
- **Make the worker tolerate a missing row.** If you enqueue before the change commits, or the change is later undone, the job finds nothing and succeeds without doing anything. All the blog's workers do this.
- **For work that must not be lost**, write a row describing it in the same Kura transaction as the change (an "outbox" table), and have a recurring job turn outbox rows into jobs.

---

Jobs can also wait for a time, repeat, or wait for each other. Next: [Scheduling](scheduling.md).
