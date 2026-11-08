-module(blog_data_tx_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kura/include/kura.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    transaction_returns_the_fun_result/1,
    transaction_rolls_back_on_exception/1,
    returning_an_error_does_not_roll_back/1,
    multi_publishes_and_tags/1,
    multi_failure_rolls_back_every_step/1,
    insert_all_bulk_inserts/1,
    insert_all_upsert_skips_duplicates/1,
    insert_upsert_with_replace/1,
    update_all_archives_stale_drafts/1,
    delete_all_purges_soft_deleted/1
]).

all() ->
    [
        transaction_returns_the_fun_result,
        transaction_rolls_back_on_exception,
        returning_an_error_does_not_roll_back,
        multi_publishes_and_tags,
        multi_failure_rolls_back_every_step,
        insert_all_bulk_inserts,
        insert_all_upsert_skips_duplicates,
        insert_upsert_with_replace,
        update_all_archives_stale_drafts,
        delete_all_purges_soft_deleted
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

transaction_returns_the_fun_result(Config) ->
    {ok, [#{title := ~"One"}, #{title := ~"Two"}]} = blog_posts:import_posts(
        ?config(alice, Config), [
            #{~"title" => ~"One", ~"body" => ~"1"}, #{~"title" => ~"Two", ~"body" => ~"2"}
        ]
    ),
    {ok, 2} = blog_repo:count(kura_query:from(blog_post)).

transaction_rolls_back_on_exception(Config) ->
    {error, #kura_changeset{} = CS} = blog_posts:import_posts(?config(alice, Config), [
        #{~"title" => ~"One", ~"body" => ~"1"}, #{~"title" => ~"Two"}
    ]),
    #{body := [~"can't be blank"]} = blog_api_json:errors(CS),
    {ok, 0} = blog_repo:count(kura_query:from(blog_post)).

returning_an_error_does_not_roll_back(Config) ->
    Alice = ?config(alice, Config),
    {error, kept} = blog_repo:transaction(fun() ->
        {ok, _} = blog_posts:create_post(Alice, #{~"title" => ~"Committed", ~"body" => ~"x"}),
        {error, kept}
    end),
    {ok, 1} = blog_repo:count(kura_query:from(blog_post)).

multi_publishes_and_tags(Config) ->
    Post = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Ready"}),
    {ok, #{status := published, tags := [_, _]}} = blog_posts:publish(Post, [~"erlang", ~"nova"]),
    {ok, #{status := published}} = blog_repo:get(blog_post, maps:get(id, Post)).

multi_failure_rolls_back_every_step(Config) ->
    Post = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Ready"}),
    {error, {tagged, #kura_changeset{schema = blog_tag}}} = blog_posts:publish(Post, [~"Bad Tag"]),
    {ok, #{status := draft, published_at := undefined}} = blog_repo:get(
        blog_post, maps:get(id, Post)
    ).

insert_all_bulk_inserts(_Config) ->
    Rows = [#{name => N} || N <- [~"erlang", ~"elixir", ~"gleam"]],
    {ok, 3} = blog_repo:insert_all(blog_tag, Rows),
    {ok, [#{inserted_at := {{_, _, _}, _}} | _]} = blog_repo:all(kura_query:from(blog_tag)).

insert_all_upsert_skips_duplicates(_Config) ->
    {ok, [_, _]} = blog_posts:ensure_tags([~"erlang", ~"nova"]),
    {ok, 1} = blog_repo:insert_all(
        blog_tag, [#{name => ~"nova"}, #{name => ~"otp"}], #{on_conflict => {name, nothing}}
    ),
    {ok, 3} = blog_repo:count(kura_query:from(blog_tag)).

insert_upsert_with_replace(_Config) ->
    {ok, #{id := Id}} = blog_repo:insert(blog_tag:changeset(#{}, #{name => ~"erlang"})),
    CS = blog_tag:changeset(#{}, #{name => ~"erlang"}),
    {ok, #{id := Id}} = blog_repo:insert(CS, #{
        on_conflict => {name, {replace, [inserted_at]}}, returning => true
    }),
    {ok, 1} = blog_repo:count(kura_query:from(blog_tag)).

update_all_archives_stale_drafts(Config) ->
    Alice = ?config(alice, Config),
    Old = blog_data_fixtures:post(Alice, #{~"title" => ~"Forgotten"}),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Live", ~"status" => ~"published"}),
    {ok, 0} = blog_posts:archive_stale_drafts({{2000, 1, 1}, {0, 0, 0}}),
    Tomorrow = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + 86400
    ),
    {ok, 1} = blog_posts:archive_stale_drafts(Tomorrow),
    {ok, #{status := archived}} = blog_repo:get(blog_post, maps:get(id, Old)).

delete_all_purges_soft_deleted(Config) ->
    Alice = ?config(alice, Config),
    Gone = blog_data_fixtures:post(Alice, #{~"title" => ~"Gone"}),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Kept"}),
    {ok, _} = blog_posts:delete_post(Gone),
    Tomorrow = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + 86400
    ),
    {ok, 1} = blog_posts:purge_deleted(Tomorrow),
    {ok, 1} = blog_repo:count(kura_query:with_deleted(kura_query:from(blog_post))).
