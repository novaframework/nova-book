-module(blog_data_query_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    aggregates/1,
    group_by_and_having/1,
    join_selects_from_both_tables/1,
    subquery_in/1,
    not_exists_with_correlated_fragment/1,
    cte_with_window_function/1,
    union_all_feed/1,
    distinct_commenters/1,
    fragment_in_where/1,
    full_text_search/1,
    cursor_pagination/1,
    streaming_in_batches/1,
    raw_sql/1
]).

all() ->
    [
        aggregates,
        group_by_and_having,
        join_selects_from_both_tables,
        subquery_in,
        not_exists_with_correlated_fragment,
        cte_with_window_function,
        union_all_feed,
        distinct_commenters,
        fragment_in_where,
        full_text_search,
        cursor_pagination,
        streaming_in_batches,
        raw_sql
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    Alice = blog_data_fixtures:user(~"alice"),
    Bob = blog_data_fixtures:user(~"bob"),
    {ok, Admin} = blog_repo:update(blog_user:role_changeset(Bob, #{role => ~"admin"})),
    Posts = [
        blog_data_fixtures:post(Alice, #{
            ~"title" => ~"Supervision trees",
            ~"body" => ~"Erlang processes are supervised and restarted.",
            ~"status" => ~"published"
        }),
        blog_data_fixtures:post(Alice, #{
            ~"title" => ~"Routing in Nova",
            ~"body" => ~"Routes map paths to controllers.",
            ~"status" => ~"published"
        }),
        blog_data_fixtures:post(Alice, #{
            ~"title" => ~"Unfinished", ~"body" => ~"Draft about processes."
        }),
        blog_data_fixtures:post(Admin, #{
            ~"title" => ~"Site news", ~"body" => ~"We moved to Kura.", ~"status" => ~"published"
        })
    ],
    [{alice, Alice}, {admin, Admin}, {posts, Posts} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

aggregates(_Config) ->
    {ok, 3} = blog_stats:published_count(),
    {ok, {{_, _, _}, {_, _, _}}} = blog_stats:latest_publication().

group_by_and_having(Config) ->
    #{id := AliceId} = ?config(alice, Config),
    #{id := AdminId} = ?config(admin, Config),
    {ok, [#{user_id := AliceId, count := 3}, #{user_id := AdminId, count := 1}]} =
        blog_stats:posts_per_author(1),
    {ok, [#{user_id := AliceId, count := 3}]} = blog_stats:posts_per_author(2).

join_selects_from_both_tables(_Config) ->
    {ok, [#{title := ~"Routing in Nova", username := ~"alice"} | _] = Rows} =
        blog_stats:post_titles_with_authors(),
    4 = length(Rows).

subquery_in(_Config) ->
    {ok, [#{title := ~"Site news"}]} = blog_stats:posts_by_admins().

not_exists_with_correlated_fragment(Config) ->
    [First | _] = ?config(posts, Config),
    {ok, _} = blog_posts:add_comment(First, ?config(admin, Config), #{~"body" => ~"Nice"}),
    {ok, Uncommented} = blog_stats:posts_without_comments(),
    3 = length(Uncommented),
    false = lists:member(maps:get(id, First), [maps:get(id, P) || P <- Uncommented]).

cte_with_window_function(Config) ->
    #{id := AliceId} = ?config(alice, Config),
    #{id := AdminId} = ?config(admin, Config),
    {ok, [
        #{user_id := AliceId, position := 1, title := _},
        #{user_id := AdminId, title := ~"Site news"}
    ]} =
        blog_stats:latest_post_per_author().

union_all_feed(Config) ->
    [First | _] = ?config(posts, Config),
    {ok, _} = blog_posts:add_comment(First, ?config(admin, Config), #{~"body" => ~"Nice"}),
    {ok, Feed} = blog_stats:activity_feed(10),
    5 = length(Feed),
    [~"comment"] = [K || #{kind := K, text := ~"Nice"} <- Feed].

distinct_commenters(Config) ->
    [First | _] = ?config(posts, Config),
    Admin = ?config(admin, Config),
    {ok, _} = blog_posts:add_comment(First, Admin, #{~"body" => ~"One"}),
    {ok, _} = blog_posts:add_comment(First, Admin, #{~"body" => ~"Two"}),
    {ok, [#{user_id := AdminId}]} = blog_stats:commenters(First),
    AdminId = maps:get(id, Admin).

fragment_in_where(_Config) ->
    {ok, Recent} = blog_stats:published_since(7),
    3 = length(Recent).

full_text_search(_Config) ->
    {ok, [#{title := ~"Supervision trees"}]} = blog_stats:search(~"process"),
    {ok, [#{title := ~"Routing in Nova"}]} = blog_stats:search(~"routing controllers"),
    {ok, []} = blog_stats:search(~"haskell").

cursor_pagination(Config) ->
    [First | _] = ?config(posts, Config),
    Admin = ?config(admin, Config),
    [
        {ok, _} = blog_posts:add_comment(First, Admin, #{~"body" => integer_to_binary(N)})
     || N <- lists:seq(1, 5)
    ],
    {ok, #{entries := [#{body := ~"1"}, #{body := ~"2"}], has_next := true, end_cursor := Cursor}} =
        blog_stats:comments_page(First, #{limit => 2}),
    {ok, #{entries := [#{body := ~"3"}, #{body := ~"4"}], has_prev := true}} =
        blog_stats:comments_page(First, #{limit => 2, 'after' => Cursor}).

streaming_in_batches(Config) ->
    Path = filename:join(?config(priv_dir, Config), "titles.txt"),
    {ok, Io} = file:open(Path, [write]),
    ok = blog_stats:export_titles(Io),
    ok = file:close(Io),
    {ok, Bin} = file:read_file(Path),
    [~"Routing in Nova", ~"Site news", ~"Supervision trees"] =
        lists:sort(binary:split(Bin, ~"\n", [global, trim])).

raw_sql(Config) ->
    [First | _] = ?config(posts, Config),
    {ok, _} = blog_posts:add_comment(First, ?config(admin, Config), #{~"body" => ~"Nice"}),
    {ok, [#{title := ~"Supervision trees", comments := 1}, #{comments := 0}]} =
        blog_stats:top_commented(2).
