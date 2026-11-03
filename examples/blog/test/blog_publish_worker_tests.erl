-module(blog_publish_worker_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

perform_test_() ->
    {foreach, fun mock_repo/0, fun unmock_repo/1, [
        fun publishes_a_draft/0,
        fun leaves_published_posts_alone/0,
        fun ignores_a_deleted_post/0,
        fun reports_a_post_that_no_longer_validates/0
    ]}.

mock_repo() ->
    ok = meck:new(blog_repo).

unmock_repo(_) ->
    ?assert(meck:validate(blog_repo)),
    meck:unload(blog_repo).

publishes_a_draft() ->
    meck:expect(blog_repo, get, [blog_post, 7], {ok, draft()}),
    meck:expect(blog_repo, update, 1, {ok, #{}}),
    ?assertEqual(ok, shigoto:perform_job(blog_publish_worker, #{~"post_id" => 7})),
    CS = meck:capture(first, blog_repo, update, '_', 1),
    ?assertEqual(published, kura_changeset:get_change(CS, status)).

leaves_published_posts_alone() ->
    meck:expect(blog_repo, get, [blog_post, 7], {ok, (draft())#{status => published}}),
    ?assertEqual(ok, shigoto:perform_job(blog_publish_worker, #{~"post_id" => 7})),
    ?assertNot(meck:called(blog_repo, update, '_')).

ignores_a_deleted_post() ->
    meck:expect(blog_repo, get, [blog_post, 7], {error, not_found}),
    ?assertEqual(ok, shigoto:perform_job(blog_publish_worker, #{~"post_id" => 7})).

reports_a_post_that_no_longer_validates() ->
    meck:expect(blog_repo, get, [blog_post, 7], {ok, draft()}),
    meck:expect(blog_repo, update, 1, {error, #kura_changeset{valid = false}}),
    ?assertEqual(
        {error, invalid_post}, shigoto:perform_job(blog_publish_worker, #{~"post_id" => 7})
    ).

draft() ->
    #{id => 7, title => ~"Hello", body => ~"x", status => draft, published_at => undefined}.
