-module(blog_jobs_ops_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("hikyaku/include/hikyaku.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    health_reports_job_counts/1,
    ops_jobs_endpoint_requires_the_token/1,
    telemetry_reports_failures/1,
    testing_mode_needs_the_confirmation_key/1,
    queues_can_be_paused_and_resumed/1,
    tables_are_created_at_boot/1,
    cron_schedules_the_purge_on_its_queue/1,
    cron_catches_up_a_missed_run/1
]).
-export([forward_event/4]).

all() ->
    [
        health_reports_job_counts,
        ops_jobs_endpoint_requires_the_token,
        telemetry_reports_failures,
        testing_mode_needs_the_confirmation_key,
        queues_can_be_paused_and_resumed,
        tables_are_created_at_boot,
        cron_schedules_the_purge_on_its_queue,
        cron_catches_up_a_missed_run
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_jobs_fixtures:capture_mail(),
    blog_data_fixtures:checkout(),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = application:unset_env(shigoto, cron),
    blog_jobs_fixtures:testing(disabled),
    blog_data_fixtures:checkin().

health_reports_job_counts(_Config) ->
    {ok, _} = blog_jobs:schedule_publish(#{id => 0}, {{2030, 1, 1}, {0, 0, 0}}),
    {ok, #{status := ok, pool := blog_repo, counts := #{available := 1, total := 1}}} =
        shigoto:health().

ops_jobs_endpoint_requires_the_token(_Config) ->
    {ok, _} = blog_jobs:schedule_publish(#{id => 0}, {{2030, 1, 1}, {0, 0, 0}}),
    Url = blog_data_fixtures:url("/ops/jobs"),
    {ok, {{_, 401, _}, _, _}} = httpc:request(get, {Url, []}, [], []),
    {ok, {{_, 200, _}, _, Body}} =
        httpc:request(get, {Url, [{"x-ops-token", "test-ops-token"}]}, [], [
            {body_format, binary}
        ]),
    #{~"status" := ~"ok", ~"counts" := #{~"available" := 1, ~"discarded" := 0}} = json:decode(Body).

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

testing_mode_needs_the_confirmation_key(_Config) ->
    ok = application:set_env(shigoto, testing, manual),
    disabled = shigoto_config:testing_mode(),
    {ok, #{id := Id}} = blog_jobs:schedule_publish(#{id => 0}, {{2030, 1, 1}, {0, 0, 0}}),
    {ok, #{state := ~"available"}} = shigoto:get_job(Id),
    ok = application:set_env(shigoto, testing_confirm_no_persistence, true),
    manual = shigoto_config:testing_mode().

queues_can_be_paused_and_resumed(_Config) ->
    {ok, _Pid} = shigoto:add_queue(~"mailers", 2),
    try
        ok = shigoto:pause_queue(~"mailers"),
        ok = shigoto:resume_queue(~"mailers"),
        {error, not_found} = shigoto:pause_queue(~"nonexistent")
    after
        ok = shigoto:remove_queue(~"mailers")
    end.

tables_are_created_at_boot(_Config) ->
    {ok, Rows} = blog_repo:query(
        ~"SELECT tablename FROM pg_tables WHERE tablename LIKE 'shigoto_%' ORDER BY tablename", []
    ),
    [~"shigoto_batches", ~"shigoto_cron", ~"shigoto_jobs", ~"shigoto_jobs_archive"] =
        [Name || #{tablename := Name} <- Rows].

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
