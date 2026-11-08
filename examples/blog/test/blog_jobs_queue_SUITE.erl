-module(blog_jobs_queue_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("hikyaku/include/hikyaku.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([queue_process_runs_jobs_in_the_background/1]).

all() ->
    [queue_process_runs_jobs_in_the_background].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    ok = clean_up(),
    blog_jobs_fixtures:capture_mail(),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = clean_up().

clean_up() ->
    ok = blog_jobs_fixtures:reset_jobs(),
    Queries = [
        ~"""
        DELETE FROM audit_log WHERE table_name = 'posts' AND record_id IN (
            SELECT id::text FROM posts WHERE user_id IN (
                SELECT id FROM users WHERE username IN ('queue_alice', 'queue_bob')))
        """,
        ~"DELETE FROM users WHERE username IN ('queue_alice', 'queue_bob')"
    ],
    [{ok, _} = blog_repo:query(Query, []) || Query <- Queries],
    ok.

queue_process_runs_jobs_in_the_background(_Config) ->
    Alice = blog_data_fixtures:user(~"queue_alice"),
    Bob = blog_data_fixtures:user(~"queue_bob"),
    Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Hello"}),
    ok = application:set_env(shigoto, poll_interval, 100),
    {ok, _Pid} = shigoto:add_queue(~"mailers", 2),
    try
        {ok, _} = blog_posts:add_comment(Post, Bob, #{~"body" => ~"Hi"}),
        #hikyaku_email{to = [#hikyaku_address{address = ~"queue_alice@example.com"}]} =
            blog_jobs_fixtures:receive_email()
    after
        ok = shigoto:remove_queue(~"mailers"),
        application:unset_env(shigoto, poll_interval)
    end.
