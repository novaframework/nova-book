-module(blog_jobs_commit_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    job_rolls_back_with_a_kura_transaction/1,
    kura_write_rolls_back_with_a_shigoto_transaction/1,
    comment_and_notification_commit_together/1,
    rolled_back_comment_takes_its_notification_along/1,
    cron_schedules_the_purge_on_its_queue/1,
    cron_catches_up_a_missed_run/1
]).

all() ->
    [
        job_rolls_back_with_a_kura_transaction,
        kura_write_rolls_back_with_a_shigoto_transaction,
        comment_and_notification_commit_together,
        rolled_back_comment_takes_its_notification_along,
        cron_schedules_the_purge_on_its_queue,
        cron_catches_up_a_missed_run
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    ok = clean_up(),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = application:unset_env(shigoto, cron),
    ok = clean_up().

clean_up() ->
    ok = blog_jobs_fixtures:reset_jobs(),
    Queries = [
        ~"DELETE FROM shigoto_cron",
        ~"""
        DELETE FROM audit_log WHERE table_name = 'posts' AND record_id IN (
            SELECT id::text FROM posts WHERE user_id IN (
                SELECT id FROM users WHERE username IN ('commit_alice', 'commit_bob')))
        """,
        ~"DELETE FROM users WHERE username IN ('commit_alice', 'commit_bob')"
    ],
    [{ok, _} = blog_repo:query(Query, []) || Query <- Queries],
    ok.

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

kura_write_rolls_back_with_a_shigoto_transaction(_Config) ->
    rolled_back =
        try
            shigoto:transaction(fun() ->
                _ = blog_data_fixtures:user(~"commit_alice"),
                throw(rolled_back)
            end)
        catch
            throw:rolled_back -> rolled_back
        end,
    {error, not_found} = blog_repo:get_by(blog_user, [{username, ~"commit_alice"}]).

comment_and_notification_commit_together(_Config) ->
    Post = blog_data_fixtures:post(blog_data_fixtures:user(~"commit_alice"), #{~"title" => ~"Hello"}),
    Bob = blog_data_fixtures:user(~"commit_bob"),
    {ok, #{id := CommentId}} = blog_posts:add_comment(Post, Bob, #{~"body" => ~"Nice post"}),
    {ok, _} = blog_repo:get(blog_comment, CommentId),
    shigoto_testing:assert_enqueued(#{
        worker => blog_comment_notification_worker, args => #{~"comment_id" => CommentId}
    }).

rolled_back_comment_takes_its_notification_along(_Config) ->
    Post = blog_data_fixtures:post(blog_data_fixtures:user(~"commit_alice"), #{~"title" => ~"Hello"}),
    Bob = blog_data_fixtures:user(~"commit_bob"),
    {rolled_back, CommentId} =
        try
            blog_repo:transaction(fun() ->
                {ok, #{id := Id}} = blog_posts:add_comment(Post, Bob, #{~"body" => ~"Nice post"}),
                throw({rolled_back, Id})
            end)
        catch
            throw:RolledBack -> RolledBack
        end,
    {error, not_found} = blog_repo:get(blog_comment, CommentId),
    shigoto_testing:refute_enqueued(#{worker => blog_comment_notification_worker}).

cron_schedules_the_purge_on_its_queue(_Config) ->
    Entry = {~"purge-every-minute", ~"* * * * *", blog_purge_worker, #{~"days" => 30}},
    ok = application:set_env(shigoto, cron, [Entry]),
    ok = shigoto_cron:schedule_once(),
    ok = shigoto_cron:schedule_once(),
    [#{queue := ~"maintenance", args := Args}] =
        shigoto_testing:all_enqueued(#{worker => blog_purge_worker}),
    #{~"days" := 30} = json:decode(Args).

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

minutes_before({Date, {H, M, _S}}, Minutes) ->
    Seconds = calendar:datetime_to_gregorian_seconds({Date, {H, M, 0}}),
    calendar:gregorian_seconds_to_datetime(Seconds - Minutes * 60).
