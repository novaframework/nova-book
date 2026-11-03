-module(blog_jobs_workers_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("hikyaku/include/hikyaku.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    perform_job_sends_the_notification/1,
    own_comment_sends_nothing/1,
    deleted_comment_is_ignored/1,
    rate_limited_delivery_snoozes/1,
    failed_delivery_is_retried_with_backoff/1,
    job_is_discarded_after_max_attempts/1,
    snoozed_job_is_rescheduled/1,
    duplicate_notification_is_a_conflict/1,
    scheduled_publish_waits_until_due/1,
    enqueue_inside_kura_transaction_is_not_rolled_back/1,
    shigoto_transaction_groups_job_inserts/1,
    shigoto_transaction_does_not_cover_kura_writes/1,
    purge_worker_schedules_its_next_run/1,
    next_purge_is_at_three_in_the_morning/1
]).

all() ->
    [
        perform_job_sends_the_notification,
        own_comment_sends_nothing,
        deleted_comment_is_ignored,
        rate_limited_delivery_snoozes,
        failed_delivery_is_retried_with_backoff,
        job_is_discarded_after_max_attempts,
        snoozed_job_is_rescheduled,
        duplicate_notification_is_a_conflict,
        scheduled_publish_waits_until_due,
        enqueue_inside_kura_transaction_is_not_rolled_back,
        shigoto_transaction_groups_job_inserts,
        shigoto_transaction_does_not_cover_kura_writes,
        purge_worker_schedules_its_next_run,
        next_purge_is_at_three_in_the_morning
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    ok = blog_jobs_fixtures:reset_jobs(),
    blog_jobs_fixtures:capture_mail(),
    blog_data_fixtures:checkout(),
    Alice = blog_data_fixtures:user(~"alice"),
    Bob = blog_data_fixtures:user(~"bob"),
    Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Hello jobs"}),
    [{alice, Alice}, {bob, Bob}, {post, Post} | Config].

end_per_testcase(_Case, _Config) ->
    blog_jobs_fixtures:testing(disabled),
    blog_data_fixtures:checkin().

perform_job_sends_the_notification(Config) ->
    blog_jobs_fixtures:testing(manual),
    {ok, #{id := CommentId}} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    ok = shigoto:perform_job(blog_comment_notification_worker, #{~"comment_id" => CommentId}),
    #hikyaku_email{
        to = [#hikyaku_address{address = ~"alice@example.com"}],
        subject = ~"bob commented on \"Hello jobs\""
    } = blog_jobs_fixtures:receive_email().

own_comment_sends_nothing(Config) ->
    blog_jobs_fixtures:testing(manual),
    {ok, #{id := CommentId}} = blog_posts:add_comment(
        ?config(post, Config), ?config(alice, Config), #{~"body" => ~"Replying to myself"}
    ),
    ok = shigoto:perform_job(blog_comment_notification_worker, #{~"comment_id" => CommentId}),
    ok = blog_jobs_fixtures:refute_email().

deleted_comment_is_ignored(_Config) ->
    ok = shigoto:perform_job(blog_comment_notification_worker, #{~"comment_id" => 0}),
    ok = blog_jobs_fixtures:refute_email().

rate_limited_delivery_snoozes(Config) ->
    blog_jobs_fixtures:testing(manual),
    blog_jobs_fixtures:mailer(#{
        adapter => blog_test_mail_adapter, reply => {error, #{status_code => 429}}
    }),
    {ok, #{id := CommentId}} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    {snooze, 60} =
        shigoto:perform_job(blog_comment_notification_worker, #{~"comment_id" => CommentId}).

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

duplicate_notification_is_a_conflict(Config) ->
    {ok, #{id := CommentId}} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    Job = #{worker => blog_comment_notification_worker, args => #{~"comment_id" => CommentId}},
    [#{id := JobId}] = shigoto_testing:all_enqueued(#{worker => blog_comment_notification_worker}),
    {ok, {conflict, #{id := JobId}}} = shigoto:insert(Job),
    [_] = shigoto_testing:all_enqueued(#{worker => blog_comment_notification_worker}).

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

hours_from_now(Hours) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    calendar:gregorian_seconds_to_datetime(Now + Hours * 3600).

seconds_from_now(DateTime) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    calendar:datetime_to_gregorian_seconds(truncate(DateTime)) - Now.

truncate({Date, {H, M, S}}) -> {Date, {H, M, trunc(S)}}.
