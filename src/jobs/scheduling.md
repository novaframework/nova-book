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

### Cron, and why the blog does not use it

Shigoto has a cron scheduler. Entries go in the `cron` setting as `{Name, Schedule, Worker, Args}`, or with a fifth element `#{timezone => Zone}` where `Zone` is `utc`, an hour offset, or an IANA name such as `~"Europe/Stockholm"`:

```erlang
{shigoto, [
    {repo, blog_repo},
    {cron, [
        {~"purge-deleted-posts", ~"0 3 * * *", blog_purge_worker, #{~"days" => 30}}
    ]}
]}
```

The schedule is the usual five fields (minute, hour, day of month, month, day of week). Every minute, one node (chosen with a PostgreSQL advisory lock) inserts a job for each entry that is due.

````admonish bug title="Cron does not fire in Shigoto 1.9.12"
The leader check in `shigoto_cron` expects the advisory-lock query to answer with a tuple row, but Shigoto's database layer returns rows as maps, so the check never passes and no cron job is ever inserted. A `* * * * *` entry inserts nothing, however long you wait. Cron jobs would also always go to the `default` queue, ignoring the worker's `queue/0`.

`test/blog_jobs_ops_SUITE.erl` pins the behaviour. It triggers a check by hand rather than waiting a minute:

```erlang
cron_entries_do_not_fire(_Config) ->
    Entry = {~"publish-every-minute", ~"* * * * *", blog_publish_worker, #{~"post_id" => 0}},
    ok = application:set_env(shigoto, cron, [Entry]),
    try
        shigoto_cron ! check,
        timer:sleep(500),
        shigoto_testing:refute_enqueued(#{worker => blog_publish_worker})
    after
        application:unset_env(shigoto, cron)
    end.
```

When that test fails after an upgrade, cron works, and the cron entry above can replace the self-scheduling job below.
````

### A job that schedules its next run

Until then, the purge reschedules itself. `src/workers/blog_purge_worker.erl`:

```erlang
-module(blog_purge_worker).
-behaviour(shigoto_worker).

-include_lib("kernel/include/logger.hrl").

-export([perform/1, queue/0, unique/0, schedule_next/0, next_run/1]).

-define(RETENTION_DAYS, 30).

queue() -> ~"maintenance".

unique() -> #{keys => [worker], states => [available, retryable], period => infinity}.

perform(#{~"days" := Days}) ->
    {ok, _} = schedule_next(),
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Before = calendar:gregorian_seconds_to_datetime(Now - Days * 86400),
    {ok, Purged} = blog_posts:purge_deleted(Before),
    ?LOG_INFO(#{msg => ~"purged soft-deleted posts", count => Purged}),
    {ok, #{~"purged" => Purged}}.

schedule_next() ->
    shigoto:insert(#{
        worker => ?MODULE,
        args => #{~"days" => ?RETENTION_DAYS},
        scheduled_at => next_run(calendar:universal_time())
    }).

next_run({Date, Time}) when Time < {3, 0, 0} ->
    {Date, {3, 0, 0}};
next_run({Date, _Time}) ->
    Tomorrow = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + 1),
    {Tomorrow, {3, 0, 0}}.
```

The pieces:

- `schedule_next/0` inserts a purge for the next 03:00 UTC.
- `unique/0` allows only one purge waiting (`available`) or waiting for a retry (`retryable`), whatever its args. `executing` is left out on purpose: when the purge runs, its call to `schedule_next/0` must not conflict with itself.
- `perform/1` schedules the next run first. If the purge then crashes, the job is retried, the retry's `schedule_next/0` finds tomorrow's job already there and gets `{ok, {conflict, _}}`, and there is still exactly one purge tomorrow.
- It returns `{ok, #{~"purged" => N}}`, which is stored on the job as its result.

Something has to insert the first one. `blog_app:start/2` calls `blog_jobs:schedule_maintenance/0` on every boot:

```erlang
schedule_maintenance() ->
    case blog_purge_worker:schedule_next() of
        {ok, _JobOrConflict} -> ok;
        {error, Reason} -> ?LOG_WARNING(#{msg => ~"could not schedule purge", reason => Reason})
    end.
```

On every boot after the first, and on every other node, the insert is a conflict and changes nothing. The tests, from `test/blog_jobs_workers_SUITE.erl`:

```erlang
purge_worker_schedules_its_next_run(_Config) ->
    {ok, #{~"purged" := 0}} = shigoto:perform_job(blog_purge_worker, #{~"days" => 30}),
    [#{scheduled_at := At}] = shigoto_testing:all_enqueued(#{
        worker => blog_purge_worker, state => ~"available"
    }),
    {_Date, {3, 0, 0}} = truncate(At),
    {ok, {conflict, _}} = blog_purge_worker:schedule_next().

next_purge_is_at_three_in_the_morning(_Config) ->
    {{2026, 11, 3}, {3, 0, 0}} = blog_purge_worker:next_run({{2026, 11, 3}, {1, 30, 0}}),
    {{2026, 11, 4}, {3, 0, 0}} = blog_purge_worker:next_run({{2026, 11, 3}, {3, 0, 0}}),
    {{2027, 1, 1}, {3, 0, 0}} = blog_purge_worker:next_run({{2026, 12, 31}, {22, 0, 0}}).
```

`next_run/1` is a pure function of the current time, so its edge cases (just before 03:00, exactly 03:00, New Year's Eve) are tested without a clock.

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
