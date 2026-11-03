-module(blog_email_notifications_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("hikyaku/include/hikyaku.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    comment_enqueues_a_notification/1,
    inline_mode_delivers_on_insert/1,
    api_comment_is_mailed_by_the_queue/1
]).

all() ->
    [
        comment_enqueues_a_notification,
        inline_mode_delivers_on_insert,
        api_comment_is_mailed_by_the_queue
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
    [{bob, Bob}, {post, Post} | Config].

end_per_testcase(_Case, _Config) ->
    blog_jobs_fixtures:testing(disabled),
    blog_data_fixtures:checkin().

comment_enqueues_a_notification(Config) ->
    blog_jobs_fixtures:testing(manual),
    {ok, #{id := CommentId}} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    shigoto_testing:assert_enqueued(#{
        worker => blog_comment_notification_worker,
        queue => ~"mailers",
        args => #{~"comment_id" => CommentId}
    }),
    shigoto_testing:refute_enqueued(#{worker => blog_export_worker}),
    ok = blog_jobs_fixtures:refute_email().

inline_mode_delivers_on_insert(Config) ->
    blog_jobs_fixtures:testing(inline),
    {ok, _} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    #hikyaku_email{to = [#hikyaku_address{address = ~"alice@example.com"}]} =
        blog_jobs_fixtures:receive_email().

api_comment_is_mailed_by_the_queue(Config) ->
    #{id := PostId} = ?config(post, Config),
    #{id := BobId} = ?config(bob, Config),
    Path = "/api/posts/" ++ integer_to_list(PostId) ++ "/comments",
    {201, #{~"id" := CommentId}} =
        blog_data_fixtures:request(post, Path, #{body => ~"Lovely", user_id => BobId}),
    shigoto_testing:assert_enqueued(#{
        worker => blog_comment_notification_worker,
        args => #{~"comment_id" => CommentId},
        state => ~"available"
    }),
    ok = blog_jobs_fixtures:refute_email(),
    ok = shigoto:drain_queue(~"mailers"),
    #hikyaku_email{text_body = Text} = blog_jobs_fixtures:receive_email(),
    {_, _} = binary:match(Text, ~"Lovely"),
    shigoto_testing:assert_enqueued(#{
        worker => blog_comment_notification_worker, state => ~"completed"
    }).
