-module(blog_data_assoc_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kura/include/kura.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    foreign_key_violation_is_a_changeset_error/1,
    preload_in_query/1,
    nested_and_standalone_preload/1,
    has_many_from_user/1,
    cast_assoc_inserts_children_atomically/1,
    deleting_a_user_cascades/1,
    api_show_includes_author_and_comments/1,
    api_create_comment/1
]).

all() ->
    [
        foreign_key_violation_is_a_changeset_error,
        preload_in_query,
        nested_and_standalone_preload,
        has_many_from_user,
        cast_assoc_inserts_children_atomically,
        deleting_a_user_cascades,
        api_show_includes_author_and_comments,
        api_create_comment
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    [{alice, blog_data_fixtures:user(~"alice")}, {bob, blog_data_fixtures:user(~"bob")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

foreign_key_violation_is_a_changeset_error(_Config) ->
    {error, CS} = blog_posts:create_post(#{id => 0}, #{~"title" => ~"Orphan", ~"body" => ~"x"}),
    [{user_id, ~"does not exist"}] = CS#kura_changeset.errors.

preload_in_query(Config) ->
    {Post, _} = post_with_comments(Config),
    Q = kura_query:preload(kura_query:from(blog_post), [author, comments]),
    {ok, [Loaded]} = blog_repo:all(Q),
    #{author := #{username := ~"alice"}, comments := [_, _]} = Loaded,
    true = maps:get(id, Loaded) =:= maps:get(id, Post).

nested_and_standalone_preload(Config) ->
    {Post, _} = post_with_comments(Config),
    {ok, Fetched} = blog_repo:get(blog_post, maps:get(id, Post)),
    false = maps:is_key(comments, Fetched),
    #{comments := Comments} = blog_repo:preload(blog_post, Fetched, [{comments, [author]}]),
    [~"bob", ~"alice"] = [maps:get(username, maps:get(author, C)) || C <- Comments].

has_many_from_user(Config) ->
    Alice = ?config(alice, Config),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"One"}),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Two"}),
    #{posts := Posts} = blog_repo:preload(blog_user, Alice, [posts]),
    2 = length(Posts),
    [#{posts := [_, _]}, #{posts := []}] =
        blog_repo:preload(blog_user, [Alice, ?config(bob, Config)], [posts]).

cast_assoc_inserts_children_atomically(Config) ->
    Alice = ?config(alice, Config),
    #{id := BobId} = ?config(bob, Config),
    Params = #{
        ~"title" => ~"Imported post",
        ~"body" => ~"From the old blog",
        ~"comments" => [
            #{~"body" => ~"First!", ~"user_id" => BobId},
            #{~"body" => ~"Welcome back", ~"user_id" => BobId}
        ]
    },
    {ok, #{id := PostId, comments := [#{post_id := PostId}, #{post_id := PostId}]}} =
        blog_posts:import_post(Alice, Params),
    Bad = Params#{~"comments" => [#{~"body" => ~"ok", ~"user_id" => BobId}, #{~"body" => ~""}]},
    {error, #kura_changeset{valid = false, assoc_changes = #{comments := [_, Failed]}}} =
        blog_posts:import_post(Alice, Bad#{~"title" => ~"Second import"}),
    #{body := [~"can't be blank"], user_id := [~"can't be blank"]} = blog_json:errors(Failed),
    {ok, [_]} = blog_repo:all(kura_query:from(blog_post)),
    {ok, 2} = kura_repo_worker:count(blog_repo, kura_query:from(blog_comment)).

deleting_a_user_cascades(Config) ->
    {_, Alice} = post_with_comments(Config),
    {ok, _} = blog_repo:delete(kura_changeset:cast(blog_user, Alice, #{}, [])),
    {ok, []} = blog_repo:all(kura_query:from(blog_post)),
    {ok, []} = blog_repo:all(kura_query:from(blog_comment)).

api_show_includes_author_and_comments(Config) ->
    {#{id := Id}, _} = post_with_comments(Config),
    {200, #{
        ~"author" := #{~"username" := ~"alice", ~"role" := ~"author"} = Author,
        ~"comments" := [#{~"body" := ~"Great post!", ~"author" := #{~"username" := ~"bob"}}, _]
    }} = blog_data_fixtures:request(get, "/api/posts/" ++ integer_to_list(Id)),
    false = maps:is_key(~"hashed_password", Author).

api_create_comment(Config) ->
    #{id := BobId} = ?config(bob, Config),
    #{id := Id} = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Hello"}),
    Path = "/api/posts/" ++ integer_to_list(Id) ++ "/comments",
    {201, #{~"body" := ~"Nice one", ~"user_id" := BobId}} =
        blog_data_fixtures:request(post, Path, #{body => ~"Nice one", user_id => BobId}),
    {422, #{~"errors" := #{~"body" := [~"can't be blank"]}}} =
        blog_data_fixtures:request(post, Path, #{body => ~"", user_id => BobId}),
    {404, _} = blog_data_fixtures:request(post, "/api/posts/0/comments", #{
        body => ~"x", user_id => BobId
    }).

post_with_comments(Config) ->
    Alice = ?config(alice, Config),
    Bob = ?config(bob, Config),
    Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Commented"}),
    {ok, _} = blog_posts:add_comment(Post, Bob, #{~"body" => ~"Great post!"}),
    {ok, _} = blog_posts:add_comment(Post, Alice, #{~"body" => ~"Thanks!"}),
    {Post, Alice}.
