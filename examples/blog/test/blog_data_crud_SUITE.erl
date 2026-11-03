-module(blog_data_crud_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kura/include/kura.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    insert_returns_the_stored_row/1,
    get_and_get_by/1,
    one_and_exists/1,
    query_builder_filters/1,
    update_writes_only_changes/1,
    delete_removes_the_row/1,
    paginate_lists_newest_first/1,
    api_create_show_update_delete/1,
    api_validation_errors/1,
    api_list_filters_and_paginates/1
]).

all() ->
    [
        insert_returns_the_stored_row,
        get_and_get_by,
        one_and_exists,
        query_builder_filters,
        update_writes_only_changes,
        delete_removes_the_row,
        paginate_lists_newest_first,
        api_create_show_update_delete,
        api_validation_errors,
        api_list_filters_and_paginates
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

insert_returns_the_stored_row(Config) ->
    Alice = ?config(alice, Config),
    {ok, Post} = blog_posts:create_post(Alice, #{~"title" => ~"My First Post", ~"body" => ~"Hello"}),
    #{id := Id, status := draft, user_id := AliceId, inserted_at := {{_, _, _}, {_, _, _}}} = Post,
    AliceId = maps:get(id, Alice),
    true = is_integer(Id),
    {error, #kura_changeset{valid = false}} = blog_posts:create_post(Alice, #{}).

get_and_get_by(Config) ->
    Alice = ?config(alice, Config),
    #{id := Id} = blog_data_fixtures:post(Alice, #{~"title" => ~"Findable"}),
    {ok, #{title := ~"Findable"}} = blog_repo:get(blog_post, Id),
    {error, not_found} = blog_repo:get(blog_post, Id + 1000),
    {ok, #{username := ~"alice"}} = blog_repo:get_by(blog_user, [{email, ~"alice@example.com"}]),
    {error, not_found} = blog_repo:get_by(blog_user, [{username, ~"nobody"}]),
    {error, not_found} = blog_posts:get_post(~"not-a-number").

one_and_exists(Config) ->
    Alice = ?config(alice, Config),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Old", ~"status" => ~"published"}),
    timer:sleep(1100),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"New", ~"status" => ~"published"}),
    Q = kura_query:from(blog_post),
    Q1 = kura_query:where(Q, {status, ~"published"}),
    Q2 = kura_query:order_by(Q1, [{inserted_at, desc}]),
    {ok, #{title := ~"New"}} = blog_repo:one(Q2),
    {ok, true} = blog_repo:exists(Q1),
    {ok, false} = blog_repo:exists(kura_query:where(Q, {status, ~"archived"})).

query_builder_filters(Config) ->
    Alice = ?config(alice, Config),
    [
        blog_data_fixtures:post(Alice, #{~"title" => T, ~"status" => S})
     || {T, S} <- [
            {~"Nova tips", ~"published"}, {~"Kura tricks", ~"draft"}, {~"Old news", ~"archived"}
        ]
    ],
    Titles = fun(Q) ->
        {ok, Rows} = blog_repo:all(kura_query:order_by(Q, [{title, asc}])),
        [maps:get(title, R) || R <- Rows]
    end,
    Q = kura_query:from(blog_post),
    [~"Kura tricks", ~"Nova tips"] = Titles(
        kura_query:where(Q, {status, in, [~"draft", ~"published"]})
    ),
    [~"Kura tricks", ~"Nova tips"] = Titles(kura_query:where(Q, {status, '!=', ~"archived"})),
    [~"Nova tips"] = Titles(kura_query:where(Q, {title, ilike, ~"%nova%"})),
    [~"Kura tricks", ~"Old news"] =
        Titles(kura_query:where(Q, {'or', [{status, ~"draft"}, {status, ~"archived"}]})),
    [~"Kura tricks", ~"Old news"] = Titles(kura_query:where(Q, {'not', {status, ~"published"}})),
    [~"Kura tricks", ~"Old news"] = Titles(kura_query:where(Q, {published_at, is_nil})),
    {ok, [#{id := _, title := _} = Row | _]} =
        blog_repo:all(kura_query:limit(kura_query:select(Q, [id, title]), 1)),
    false = maps:is_key(body, Row).

update_writes_only_changes(Config) ->
    Alice = ?config(alice, Config),
    Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Draft title"}),
    {ok, Updated} = blog_posts:update_post(Post, #{
        ~"title" => ~"Final title", ~"status" => ~"published"
    }),
    #{title := ~"Final title", status := published, published_at := {{_, _, _}, _}} = Updated,
    CS = blog_post:changeset(Updated, #{~"title" => ~"Final title"}),
    0 = map_size(CS#kura_changeset.changes),
    {ok, Updated} = blog_repo:update(CS),
    {error, CS1} = blog_posts:update_post(Updated, #{~"title" => ~""}),
    #{title := [~"can't be blank", ~"should be at least 3 character(s)"]} = blog_api_json:errors(
        CS1
    ).

delete_removes_the_row(Config) ->
    Alice = ?config(alice, Config),
    #{id := Id} = Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Short lived"}),
    {ok, #{id := Id}} = blog_posts:delete_post(Post),
    {error, not_found} = blog_repo:get(blog_post, Id).

paginate_lists_newest_first(Config) ->
    Alice = ?config(alice, Config),
    [
        blog_data_fixtures:post(Alice, #{~"title" => <<"Post ", (integer_to_binary(N))/binary>>})
     || N <- lists:seq(1, 12)
    ],
    {ok, #{entries := Page1, total_entries := 12, total_pages := 2, page := 1}} =
        blog_posts:list_posts(#{}),
    10 = length(Page1),
    ~"Post 12" = maps:get(title, hd(Page1)),
    {ok, #{entries := [#{title := ~"Post 2"}, #{title := ~"Post 1"}]}} =
        blog_posts:list_posts(#{~"page" => ~"2"}).

api_create_show_update_delete(Config) ->
    #{id := AliceId} = ?config(alice, Config),
    {201, #{~"id" := Id, ~"status" := ~"draft", ~"user_id" := AliceId}} =
        blog_data_fixtures:request(post, "/api/posts", #{
            title => ~"My First Post", body => ~"Hello from Nova!", user_id => AliceId
        }),
    Path = "/api/posts/" ++ integer_to_list(Id),
    {200, #{~"title" := ~"My First Post", ~"published_at" := null}} =
        blog_data_fixtures:request(get, Path),
    {200, #{~"status" := ~"published", ~"published_at" := <<_/binary>>}} =
        blog_data_fixtures:request(put, Path, #{status => ~"published"}),
    {204, <<>>} = blog_data_fixtures:request(delete, Path),
    {404, #{~"error" := ~"post not found"}} = blog_data_fixtures:request(get, Path),
    {404, _} = blog_data_fixtures:request(get, "/api/posts/abc").

api_validation_errors(Config) ->
    #{id := AliceId} = ?config(alice, Config),
    {422, #{
        ~"errors" := #{
            ~"title" := [~"should be at least 3 character(s)"], ~"body" := [~"can't be blank"]
        }
    }} =
        blog_data_fixtures:request(post, "/api/posts", #{title => ~"Hi", user_id => AliceId}),
    {422, #{~"errors" := #{~"user_id" := [~"does not exist"]}}} =
        blog_data_fixtures:request(post, "/api/posts", #{
            title => ~"Hello", body => ~"x", user_id => 0
        }).

api_list_filters_and_paginates(Config) ->
    Alice = ?config(alice, Config),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Visible", ~"status" => ~"published"}),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Hidden"}),
    {200, #{~"posts" := [#{~"title" := ~"Visible"}], ~"total_entries" := 1, ~"page" := 1}} =
        blog_data_fixtures:request(get, "/api/posts?status=published"),
    {400, #{~"errors" := #{~"status" := [~"is not a valid enum value"]}}} =
        blog_data_fixtures:request(get, "/api/posts?status=nope").
