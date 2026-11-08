# Background Jobs

The [previous part](../email/transactional-email.md) moved email out of the request and into background jobs. This part looks at the job queue itself, [Shigoto](https://hex.pm/packages/shigoto) 1.10: how it is set up, what a worker can declare, what happens when a job fails, and how jobs are made unique, scheduled and tested.

Shigoto stores jobs in PostgreSQL, in the blog's own database. There is no separate broker to run. Queue processes on every node poll the jobs table and claim work with `SELECT ... FOR UPDATE SKIP LOCKED`, so two nodes never take the same job. A job runs **at least once**: if a node dies halfway through a job, the job is picked up again later. Workers should therefore be safe to run twice, which is why the blog's workers load their data by id and check its state before acting.

## Setup

### Dependency and start order

`rebar.config` has `{shigoto, "~> 1.10"}`, and `src/blog.app.src` starts it after Kura:

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

The order matters. Kura starts the `blog_repo` connection pool when the `kura` application starts, and Shigoto uses that pool from the moment it starts: its application start migrates its tables and its queue processes begin polling. Listing `shigoto` after `kura` and `kura_postgres` makes sure the pool exists first. Shigoto also starts its own dependencies, `minato` (the PostgreSQL driver Kura uses) and `seki` (resilience primitives, see [Operations](operations.md#resilience)).

### Configuration

`config/dev_sys.config.src`:

```erlang
{shigoto, [
    {repo, blog_repo},
    {queues, [{~"default", 10}, {~"mailers", 5}, {~"maintenance", 1}]},
    {cron, [
        {~"purge-deleted-posts", ~"0 3 * * *", blog_purge_worker, #{~"days" => 30}}
    ]}
]},
```

`repo` is the name of the minato pool to use. kura_postgres names the pool after the repo, so `{repo, blog_repo}` makes Shigoto share the blog's pool and database. It also tells Shigoto that the pool belongs to a Kura repo, which is what lets the two share a transaction ([Transactions](#transactions)). The alternative, `{pool, Name}`, is for a minato pool Shigoto has to itself. The `cron` entry is the nightly purge from [Scheduling](scheduling.md#cron).

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

Shigoto creates and upgrades its tables itself: `shigoto_app` runs `shigoto_migration:up/1` on the pool every time it starts. The migration takes an advisory lock first, so nodes that boot together migrate one after another, and it does nothing when the schema is already current (Shigoto records its schema version in a comment on `shigoto_jobs`). A newer Shigoto upgrades its own tables the same way, so there is nothing to keep in step with Kura's migrations.

There is one catch, on the very first boot. Shigoto starts before the blog, and the blog is where Kura's migrator runs and creates the database if it is missing. On a brand new database Shigoto's migration therefore runs against a database that does not exist yet. Shigoto tries 5 times, 1 to 5 seconds apart, logging a `shigoto_migration_failed` warning each time. Then it starts without its queues and logs `shigoto_migration_pending` at error level. From then on it retries every 30 seconds. Meanwhile no jobs run, `shigoto:health/0` reports `migration_pending` and `shigoto:add_queue/2` answers `{error, migration_pending}`.

The blog does not wait for that retry. In `src/blog_app.erl` it runs Shigoto's migration itself once Kura has created the database, and makes sure Shigoto's queues run before the blog starts:

```erlang
start(_StartType, _StartArgs) ->
    ok = require_encryption_key(),
    {ok, _} = kura_migrator:migrate(blog_repo),
    ok = migrate_jobs(),
    ok = nova_handlers:register_handler(text, fun blog_text_handler:handle/3),
    ok = opentelemetry_nova:setup(),
    ok = opentelemetry_kura:setup(),
    blog_sup:start_link().
```

```erlang
migrate_jobs() ->
    ok = shigoto_migration:up(blog_repo),
    case whereis(shigoto_migrator) of
        undefined ->
            ok;
        Migrator ->
            Ref = monitor(process, Migrator),
            _ = shigoto_migrator:retry_now(),
            receive
                {'DOWN', Ref, process, Migrator, _Reason} -> ok
            after 10000 ->
                error(shigoto_queues_not_started)
            end
    end.
```

- `shigoto_migration:up/1` returns `ok`, or `{error, Reason}` when a statement fails, so the `ok =` match stops the boot if the tables cannot be created. On every boot after the first it finds the schema current and changes nothing.
- `shigoto_migrator` is the process that holds the queues back. It only runs while a migration is pending, so on a normal boot `whereis/1` finds nothing and `migrate_jobs/0` is done.
- When it is there, `shigoto_migrator:retry_now/0` tells it to retry at once. That is a message, so it returns before the retry has run. The retry finds the schema current, starts the configured queues and stops the process normally. Waiting for the `'DOWN'` makes `blog_app:start/2` return only once the queues are running, so no request and no test ever sees `migration_pending`. The timeout turns a retry that keeps failing into a boot failure instead of a silent node with no queues.

The first boot is still slow: Shigoto's five attempts take a few seconds, and the reports above are expected. There is a second wait. Kura's migrator creates the database and then waits for the `blog_repo` pool to answer. The pool has been failing to connect to the missing database for as long as Shigoto retried, and minato waits longer after each failure (up to 30 seconds), so the pool's next attempt can be well over Kura's default 5 second wait away, and `kura_migrator:migrate/1` then fails with `{error, pool_unavailable}`. `config/test_sys.config` gives Kura more time, because `blog_test` is created by the first test run (on CI, by every run):

```erlang
{kura, [
    {migration_pool_ready_timeout, 35000},
    {repos, #{
```

Only a missing database needs this. In development, Docker Compose creates `blog_dev` when the container starts ([Database Setup](../data-layer/setup.md)). In production the database exists before the release starts (you create it when you provision it, see [Deployment](../production/deployment.md#migrations-at-boot)). In both, Shigoto migrates at its own start and `migrate_jobs/0` finds nothing to do.

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

The comment is created in `manual` mode so that `add_comment/3`'s own notification only goes to the test buffer. Otherwise the table would already hold a notification for the comment, with the worker's five attempts, and the test's insert would be a unique conflict with it.

Discarded and cancelled jobs can be brought back with `shigoto:retry/1` (one job) or `shigoto:retry_by/1`, which takes a filter map of `worker`, `queue`, `tags`, `args` (containment) and `state`; `shigoto:cancel_by/1` cancels waiting jobs with the same filters. `state` narrows the states the call already acts on: `shigoto:retry_by(#{worker => blog_comment_notification_worker, state => ~"discarded"})` brings back the notifications that ran out of attempts and leaves the cancelled ones alone.

### Snoozing

`{snooze, Seconds}` puts the job back to `available`, due in `Seconds`, and records no error. It does not use up an attempt. The notification worker snoozes on a 429:

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
    {ok, #{
        state := ~"available",
        attempt := 1,
        max_attempts := 6,
        errors := ~"[]",
        scheduled_at := At
    }} = shigoto:get_job(JobId),
    Delay = seconds_from_now(At),
    true = Delay > 50 andalso Delay =< 60.
```

Look at the numbers. `attempt` counts runs, so it is 1 after the first run, snooze or not. To keep the snooze from counting against the worker's five attempts, Shigoto raises the job's `max_attempts` by one instead: 6 after one snooze. A job that snoozes four times and then fails still has all its retries left. A job that only ever snoozes is never discarded, so snooze for conditions that clear by themselves, such as a provider's rate limit, and return `{error, Reason}` for the rest.

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

The check runs under a PostgreSQL advisory lock, so two nodes inserting the same job at the same moment still produce one. `states` is the option to think about. The default leaves `completed` out, so a notification can be sent again once the first one has gone; the [account email worker](../email/transactional-email.md#confirming-addresses-and-resetting-passwords) adds `completed` and a `period` of 300 seconds, so each user gets at most one email of each kind per five minutes.

## Transactions

Shigoto's jobs live in the blog's database, on the blog's pool, so a job can commit together with the change it is about. With `{repo, blog_repo}` this works in both directions:

- A job inserted inside `blog_repo:transaction/1` (or a `kura_multi`) runs on that transaction's connection. It commits with the transaction, or disappears with it.
- Kura writes inside `shigoto:transaction/1` join Shigoto's transaction, because Shigoto opens it through Kura's driver.

The tests are in `test/blog_jobs_workers_SUITE.erl`. A job inserted in a Kura transaction that rolls back is gone:

```erlang
job_rolls_back_with_a_kura_transaction(_Config) ->
    rolled_back =
        try
            blog_repo:transaction(fun() ->
                {ok, _} = shigoto:insert(#{
                    worker => blog_publish_worker, args => #{~"post_id" => 1}
                }),
                throw(rolled_back)
            end)
        catch
            throw:rolled_back -> rolled_back
        end,
    shigoto_testing:refute_enqueued(#{worker => blog_publish_worker}).
```

and a user inserted through Kura inside a Shigoto transaction that rolls back was never there:

```erlang
kura_write_rolls_back_with_a_shigoto_transaction(_Config) ->
    rolled_back =
        try
            shigoto:transaction(fun() ->
                _ = blog_data_fixtures:user(~"carol"),
                throw(rolled_back)
            end)
        catch
            throw:rolled_back -> rolled_back
        end,
    {error, not_found} = blog_repo:get_by(blog_user, [{username, ~"carol"}]).
```

Both run inside Kura's test sandbox. Shigoto runs its statements on the sandbox's connection, and a transaction there becomes a savepoint, so a rollback behaves as it does in production ([Operations](operations.md#the-database-and-the-sandbox)).

`shigoto:transaction/1` also groups job inserts on their own. From `test/blog_jobs_workers_SUITE.erl`:

```erlang
shigoto_transaction_groups_job_inserts(_Config) ->
    rolled_back =
        try
            shigoto:transaction(fun() ->
                {ok, _} = shigoto:insert(#{
                    worker => blog_publish_worker, args => #{~"post_id" => 1}
                }),
                {ok, _} = shigoto:insert(#{
                    worker => blog_publish_worker, args => #{~"post_id" => 2}
                }),
                throw(rolled_back)
            end)
        catch
            throw:rolled_back -> rolled_back
        end,
    shigoto_testing:refute_enqueued(#{worker => blog_publish_worker}).
```

The blog uses this for the comment and its notification: `blog_posts:add_comment/3` inserts both in one `kura_multi` ([Transactional Email](../email/transactional-email.md#calling-it)). Two things to know:

- **Which transaction is on the outside.** Either works, but `[shigoto, job, inserted]` telemetry and the `job inserted` log fire when the insert runs. Inside `blog_repo:transaction/1` that is before the commit, so a rolled-back job still shows up as inserted. Inside `shigoto:transaction/1` they fire after the commit. If your metrics must only count committed jobs, make `shigoto:transaction/1` the outer one.
- **The worker should still be idempotent.** A shared transaction means no job for a comment that was never saved, and no saved comment without its job. It does not make a job run exactly once: jobs run at least once, and a comment can disappear between the commit and the run (deleting a post deletes its comments). So the notification worker still loads the comment by id and does nothing when it is gone.

---

Jobs can also wait for a time, repeat, or wait for each other. Next: [Scheduling](scheduling.md).
