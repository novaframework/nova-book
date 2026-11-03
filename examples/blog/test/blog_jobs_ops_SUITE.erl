-module(blog_jobs_ops_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("hikyaku/include/hikyaku.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    health_reports_job_counts/1,
    ops_jobs_endpoint_requires_the_token/1,
    telemetry_reports_failures/1,
    testing_mode_needs_the_confirmation_key/1,
    queue_process_runs_jobs_in_the_background/1,
    queues_can_be_paused_and_resumed/1,
    cron_entries_do_not_fire/1,
    tables_are_created_at_boot/1
]).
-export([forward_event/4]).

all() ->
    [
        health_reports_job_counts,
        ops_jobs_endpoint_requires_the_token,
        telemetry_reports_failures,
        testing_mode_needs_the_confirmation_key,
        queue_process_runs_jobs_in_the_background,
        queues_can_be_paused_and_resumed,
        cron_entries_do_not_fire,
        tables_are_created_at_boot
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
    Config.

end_per_testcase(_Case, _Config) ->
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

queues_can_be_paused_and_resumed(_Config) ->
    {ok, _Pid} = shigoto:add_queue(~"mailers", 2),
    try
        ok = shigoto:pause_queue(~"mailers"),
        ok = shigoto:resume_queue(~"mailers"),
        {error, not_found} = shigoto:pause_queue(~"nonexistent")
    after
        ok = shigoto:remove_queue(~"mailers")
    end.

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

tables_are_created_at_boot(_Config) ->
    {ok, Rows} = blog_repo:query(
        ~"SELECT tablename FROM pg_tables WHERE tablename LIKE 'shigoto_%' ORDER BY tablename", []
    ),
    [~"shigoto_batches", ~"shigoto_cron", ~"shigoto_jobs", ~"shigoto_jobs_archive"] =
        [Name || #{tablename := Name} <- Rows].
