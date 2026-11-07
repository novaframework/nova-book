-module(blog_jobs_commit_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    job_rolls_back_with_a_kura_transaction/1,
    kura_write_rolls_back_with_a_shigoto_transaction/1,
    comment_and_notification_commit_together/1,
    rolled_back_comment_takes_its_notification_along/1
]).

all() ->
    [
        job_rolls_back_with_a_kura_transaction,
        kura_write_rolls_back_with_a_shigoto_transaction,
        comment_and_notification_commit_together,
        rolled_back_comment_takes_its_notification_along
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
    ok = clean_up().

clean_up() ->
    ok = blog_jobs_fixtures:reset_jobs(),
    Queries = [
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
