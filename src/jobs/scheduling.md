# Scheduling

Not every job should run as soon as possible. A post can be written now and published tomorrow morning; deleted posts should be purged every night; a job can need another job's result. This chapter covers jobs that wait for a time, jobs that repeat, and jobs that wait for each other.

## Scheduled jobs

`scheduled_at` holds a job back until a UTC datetime. Queues only claim jobs whose `scheduled_at` has passed. `blog_jobs` schedules a publication:

```erlang
schedule_publish(#{id := PostId}, At) ->
    shigoto:insert(#{
        worker => blog_publish_worker,
        args => #{~"post_id" => PostId},
        scheduled_at => At
    }).
```

`At` is a `calendar:datetime()` in UTC, the same shape Kura uses for `utc_datetime` fields:

```erlang
1> blog_jobs:schedule_publish(Post, {{2026, 11, 4}, {9, 0, 0}}).
{ok,#{args => <<"{\"post_id\": 1}">>,id => 3,
      ...
      state => <<"available">>,
      scheduled_at => {{2026,11,4},{9,0,0}},
      ...}}
```

The job is `available` at once, but no queue will touch it before 09:00 UTC on 4 November. `blog_publish_worker` (from [Background Jobs](background-jobs.md#workers)) checks that the post is still a draft when it runs, so an author who publishes by hand in the meantime gets no surprise.

The test schedules one post in the future and one in the past, drains the queue, and only the second is published. From `test/blog_jobs_workers_SUITE.erl`:

```erlang
scheduled_publish_waits_until_due(Config) ->
    Alice = ?config(alice, Config),
    #{id := LaterId} = Later = ?config(post, Config),
    #{id := NowId} = Now = blog_data_fixtures:post(Alice, #{~"title" => ~"Right away"}),
    {ok, _} = blog_jobs:schedule_publish(Later, hours_from_now(2)),
    {ok, _} = blog_jobs:schedule_publish(Now, hours_from_now(-1)),
    ok = shigoto:drain_queue(~"default"),
    {ok, #{status := draft}} = blog_repo:get(blog_post, LaterId),
    {ok, #{status := published}} = blog_repo:get(blog_post, NowId),
    shigoto_testing:assert_enqueued(#{
        worker => blog_publish_worker, args => #{~"post_id" => LaterId}, state => ~"available"
    }).

hours_from_now(Hours) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    calendar:gregorian_seconds_to_datetime(Now + Hours * 3600).
```

To call off a scheduled publication, cancel the job: `shigoto:cancel(JobId)`, or `shigoto:cancel_by(#{worker => blog_publish_worker, args => #{~"post_id" => PostId}})` when you only know the post.

## Recurring jobs

The blog soft-deletes posts ([Data Safety](../data-layer/data-safety.md)) and `blog_posts:purge_deleted/1` removes the ones deleted before a given time. That should happen every night.

### Cron

Shigoto has a cron scheduler. Entries go in the `cron` setting as `{Name, Schedule, Worker, Args}`. `config/dev_sys.config.src` and `config/prod_sys.config.src`:

```erlang
{shigoto, [
    {repo, blog_repo},
    {queues, [{~"default", 10}, {~"mailers", 5}, {~"maintenance", 1}]},
    {cron, [
        {~"purge-deleted-posts", ~"0 3 * * *", blog_purge_worker, #{~"days" => 30}}
    ]}
]},
```

The schedule is the usual five fields (minute, hour, day of month, month, day of week), so this entry inserts a `blog_purge_worker` job with `#{~"days" => 30}` at 03:00 UTC every day. Aliases such as `@daily` and `@hourly` work too. The test config has no `cron` key, so nothing is scheduled behind a test's back.

How it works, because it matters once you run more than one node:

- **One node schedules.** Every node runs a scheduler that ticks once a minute. Each pass runs in a transaction that first takes a PostgreSQL advisory lock without waiting; the node that gets it schedules, the others skip that minute. Run as many nodes as you like and each entry still gets one job per due minute.
- **The minute comes from PostgreSQL**, not from the node's clock, so a node whose clock is off cannot make a minute due twice.
- **Shigoto remembers the last minute it scheduled** for each entry, in the `shigoto_cron` table, in the same transaction as the jobs it inserted. A pass only looks at the minutes after that one. A tick that runs late, a restart within the minute, or two nodes racing never schedule a minute twice.
- **Catch-up is limited to 60 minutes.** After downtime, an entry that was due in the last 60 minutes is scheduled once on the first pass. A deploy that takes the blog down from 02:55 to 03:10 still purges; a night with the database down from 01:00 to 05:00 skips that purge, and the next one is the following night.
- **A new entry is not backfilled.** The first time Shigoto sees an entry's name, it starts from the current minute. Adding the purge at 14:00 does not run it for 03:00 that morning.
- **The job goes to the worker's queue.** `blog_purge_worker:queue/0` says `maintenance`, so the purge runs in the one-slot `maintenance` queue, not in `default`.

An entry can take a fifth element, `#{timezone => Zone}`, where `Zone` is `utc` (the default), an hour offset such as `1` or `~"+1"`, or an IANA name such as `~"Europe/Stockholm"`. The schedule is then read in that zone, with daylight saving time: `{~"morning-digest", ~"0 7 * * *", Worker, Args, #{timezone => ~"Europe/Stockholm"}}` runs at 07:00 Stockholm time, summer and winter.

An entry that cannot be scheduled (a schedule that does not parse, args that are not JSON, a worker whose `queue/0` is not a binary) is skipped with a `shigoto_cron_entry_skipped` warning, and the other entries still run. `blog_prod_config_SUITE` parses the production entry, so a typo fails the build rather than the night.

### The purge worker

`src/workers/blog_purge_worker.erl`:

```erlang
-module(blog_purge_worker).
-behaviour(shigoto_worker).

-include_lib("kernel/include/logger.hrl").

-export([perform/1, queue/0, cutoff/2]).

queue() -> ~"maintenance".

perform(#{~"days" := Days}) ->
    {ok, Purged} = blog_posts:purge_deleted(cutoff(calendar:universal_time(), Days)),
    ?LOG_INFO(#{msg => ~"purged soft-deleted posts", count => Purged}),
    {ok, #{~"purged" => Purged}}.

cutoff(Now, Days) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) - Days * 86400
    ).
```

The retention period is in the cron entry's args, not in the worker, so changing it is a config change. `cutoff/2` is a pure function of the time, which makes it easy to test ([Unit Testing](../testing/unit-testing.md#workers)). The worker returns `{ok, #{~"purged" => N}}`, which is stored on the job as its result. Running it twice is harmless: the second run finds nothing older than the cutoff.

### Testing cron

The test config runs no queues and has no cron entries, and you do not want a test to wait for 03:00. `shigoto_cron:schedule_once/0` runs one scheduling pass in the calling process, right now, and inserts the jobs that are due. The tests set an entry, run a pass and look at the jobs table.

They live in `test/blog_jobs_commit_SUITE.erl`, which runs outside Kura's sandbox: a pass writes the `shigoto_cron` table on its own connection and commits, and the second test changes that row afterwards ([Operations](operations.md#the-database-and-the-sandbox) explains why that cannot happen inside the sandbox). The suite deletes its jobs and cron rows itself, before and after every test.

An entry due every minute is scheduled once, however many passes run in that minute, and goes to the worker's queue:

```erlang
cron_schedules_the_purge_on_its_queue(_Config) ->
    Entry = {~"purge-every-minute", ~"* * * * *", blog_purge_worker, #{~"days" => 30}},
    ok = application:set_env(shigoto, cron, [Entry]),
    ok = shigoto_cron:schedule_once(),
    ok = shigoto_cron:schedule_once(),
    [#{queue := ~"maintenance", args := Args}] =
        shigoto_testing:all_enqueued(#{worker => blog_purge_worker}),
    #{~"days" := 30} = json:decode(Args).
```

Catch-up, and no backfill, with an entry that was due ten minutes ago:

```erlang
cron_catches_up_a_missed_run(_Config) ->
    {ok, Now} = shigoto_repo:current_minute(blog_repo),
    {_Date, {Hour, Minute, _}} = minutes_before(Now, 10),
    Schedule = iolist_to_binary(io_lib:format("~b ~b * * *", [Minute, Hour])),
    Entry = {~"purge-catch-up", Schedule, blog_purge_worker, #{~"days" => 30}},
    ok = application:set_env(shigoto, cron, [Entry]),
    ok = shigoto_cron:schedule_once(),
    shigoto_testing:refute_enqueued(#{worker => blog_purge_worker}),
    {ok, _} = blog_repo:query(
        ~"UPDATE shigoto_cron SET last_scheduled_at = last_scheduled_at - interval '20 minutes'",
        []
    ),
    ok = shigoto_cron:schedule_once(),
    shigoto_testing:assert_enqueued(#{worker => blog_purge_worker}).
```

The first pass sees a new entry and starts from the current minute, so the run ten minutes ago is not made up. Moving `last_scheduled_at` back 20 minutes makes it look as if the node had been down since then; the next pass finds the missed minute within the last 60 and schedules it. `shigoto_repo:current_minute/1` is the database's minute that cron itself uses, so the test and the scheduler agree on the time.

`schedule_once/0` answers `{error, in_transaction}` when called inside a Shigoto or Kura transaction, since the pass would join it and hold the cron lock until it ended.

## Dependencies

A job can wait for others with `depends_on`, a list of job ids. It is not claimed until every one of them has finished (completed, discarded or cancelled). From `test/blog_jobs_workers_SUITE.erl`, publishing part two of a series only after part one:

```erlang
dependent_job_waits_for_its_predecessor(Config) ->
    #{id := FirstId} = ?config(post, Config),
    #{id := SecondId} = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Part two"}),
    {ok, #{id := FirstJob}} = shigoto:insert(#{
        worker => blog_publish_worker, args => #{~"post_id" => FirstId}
    }),
    {ok, #{id := SecondJob}} = shigoto:insert(#{
        worker => blog_publish_worker,
        args => #{~"post_id" => SecondId},
        depends_on => [FirstJob]
    }),
    {ok, #{depends_on := [FirstJob]}} = shigoto:get_job(SecondJob),
    ok = shigoto:drain_queue(~"default"),
    {ok, #{state := ~"completed", depends_on := []}} = shigoto:get_job(SecondJob),
    {ok, #{status := published}} = blog_repo:get(blog_post, SecondId).
```

When a predecessor finishes, Shigoto removes it from the dependent's `depends_on` and records its result. The dependent's `perform/1` then finds the predecessors' results in its args under the atom key `deps_results`, keyed by job id: a predecessor that returned `{ok, #{~"purged" => 3}}` shows up as `#{deps_results := #{41 => #{~"purged" => 3}}}`. Plain `ok`, a discard or a cancel shows up as `null`. Inserting a job whose dependencies do not exist fails with `{error, {missing_dependencies, Ids}}`.

**Batches** group jobs under one callback: `shigoto:new_batch(#{callback_worker => W, callback_args => Args})` returns a batch whose `id` you pass as `batch` when inserting jobs, and `W` runs once every job in the batch has finished (completed or discarded; when the last one to finish was discarded, its args get an extra `~"_batch_event" => ~"discard"`). The blog has no use for them, so they are only mentioned here; see Shigoto's `shigoto_batch` module docs.

```admonish note
The `inline` and `manual` testing modes from the [next chapter](operations.md#testing-jobs) ignore `depends_on`, `batch` and `unique` (Shigoto logs a warning). Test those against the database with `drain_queue/1`, as above.
```

---

Next: [Operations](operations.md), testing jobs and keeping an eye on them in production.
