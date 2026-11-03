-module(blog_data_advanced_data_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kura/include/kura.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    ensure_tags_is_idempotent/1,
    put_assoc_replaces_tags/1,
    preload_many_to_many_both_ways/1,
    filter_posts_by_tag/1,
    invalid_tag_is_rejected/1,
    embed_round_trips_as_jsonb/1,
    embed_validation_errors_reach_the_parent/1,
    api_tags_and_metadata/1
]).

all() ->
    [
        ensure_tags_is_idempotent,
        put_assoc_replaces_tags,
        preload_many_to_many_both_ways,
        filter_posts_by_tag,
        invalid_tag_is_rejected,
        embed_round_trips_as_jsonb,
        embed_validation_errors_reach_the_parent,
        api_tags_and_metadata
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

ensure_tags_is_idempotent(_Config) ->
    {ok, [#{id := A}, #{id := B}]} = sorted(blog_posts:ensure_tags([~"erlang", ~"nova"])),
    {ok, [#{id := A}, #{id := B}, #{name := ~"otp"}]} =
        sorted(blog_posts:ensure_tags([~"nova", ~"erlang", ~"otp"])),
    {ok, 3} = kura_repo_worker:count(blog_repo, kura_query:from(blog_tag)).

put_assoc_replaces_tags(Config) ->
    Post = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Tagged"}),
    {ok, #{tags := [_, _]}} = blog_posts:tag_post(Post, [~"erlang", ~"nova"]),
    {ok, #{tags := [#{name := ~"kura"}]}} = blog_posts:tag_post(Post, [~"kura"]),
    #{tags := [#{name := ~"kura"}]} = blog_repo:preload(blog_post, Post, [tags]),
    {ok, 1} = kura_repo_worker:count(blog_repo, kura_query:from(blog_post_tag)).

preload_many_to_many_both_ways(Config) ->
    Alice = ?config(alice, Config),
    P1 = blog_data_fixtures:post(Alice, #{~"title" => ~"First"}),
    P2 = blog_data_fixtures:post(Alice, #{~"title" => ~"Second"}),
    {ok, _} = blog_posts:tag_post(P1, [~"erlang", ~"nova"]),
    {ok, _} = blog_posts:tag_post(P2, [~"erlang"]),
    {ok, [Erlang]} = blog_repo:all(
        kura_query:preload(kura_query:where(kura_query:from(blog_tag), {name, ~"erlang"}), [posts])
    ),
    [~"First", ~"Second"] = lists:sort([maps:get(title, P) || P <- maps:get(posts, Erlang)]).

filter_posts_by_tag(Config) ->
    Alice = ?config(alice, Config),
    P1 = blog_data_fixtures:post(Alice, #{~"title" => ~"About Nova"}),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Untagged"}),
    {ok, _} = blog_posts:tag_post(P1, [~"nova"]),
    {ok, [#{title := ~"About Nova", tags := [#{name := ~"nova"}]}]} = blog_posts:tagged(~"nova"),
    {ok, #{entries := [#{title := ~"About Nova"}]}} = blog_posts:list_posts(#{~"tag" => ~"nova"}),
    {ok, []} = blog_posts:tagged(~"missing").

invalid_tag_is_rejected(Config) ->
    Post = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Tagged"}),
    {error, CS} = blog_posts:tag_post(Post, [~"Not Valid"]),
    #{name := [~"has invalid format"]} = blog_api_json:errors(CS).

embed_round_trips_as_jsonb(Config) ->
    Metadata = #{
        ~"seo_title" => ~"Nova and Kura",
        ~"cover_image_url" => ~"https://example.com/cover.png",
        ~"reading_minutes" => 4
    },
    #{id := Id, metadata := Stored} = blog_data_fixtures:post(?config(alice, Config), #{
        ~"title" => ~"With metadata", ~"metadata" => Metadata
    }),
    #{seo_title := ~"Nova and Kura", reading_minutes := 4} = Stored,
    {ok, #{metadata := #{cover_image_url := ~"https://example.com/cover.png"}}} =
        blog_repo:get(blog_post, Id),
    {ok, [#{seo_title := ~"Nova and Kura"}]} =
        blog_repo:query(~"SELECT metadata->>'seo_title' AS seo_title FROM posts WHERE id = $1", [Id]).

embed_validation_errors_reach_the_parent(_Config) ->
    CS = blog_post:changeset(#{}, #{
        ~"title" => ~"Hello",
        ~"body" => ~"x",
        ~"metadata" => #{
            ~"cover_image_url" => ~"http://insecure.example.com", ~"reading_minutes" => 0
        }
    }),
    #{cover_image_url := [~"has invalid format"], reading_minutes := [~"must be greater than 0"]} =
        blog_api_json:errors(CS),
    Bad = blog_post:changeset(#{}, #{~"title" => ~"Hello", ~"body" => ~"x", ~"metadata" => [1]}),
    #{metadata := [~"expected a map"]} = blog_api_json:errors(Bad).

api_tags_and_metadata(Config) ->
    Alice = ?config(alice, Config),
    #{id := Id} = blog_data_fixtures:post(Alice, #{
        ~"title" => ~"Tag me", ~"metadata" => #{~"seo_title" => ~"Tag me please"}
    }),
    Path = "/api/posts/" ++ integer_to_list(Id),
    {200, #{~"tags" := [~"erlang", ~"nova"]}} =
        blog_data_fixtures:request_as(
            blog_data_fixtures:token(Alice), put, Path ++ "/tags", #{tags => [~"nova", ~"erlang"]}
        ),
    {200, #{
        ~"tags" := [~"erlang", ~"nova"],
        ~"metadata" := #{~"seo_title" := ~"Tag me please", ~"reading_minutes" := null}
    }} = blog_data_fixtures:request(get, Path),
    {200, #{~"posts" := [#{~"id" := Id}]}} = blog_data_fixtures:request(get, "/api/posts?tag=nova").

sorted({ok, Tags}) ->
    {ok, lists:sort(fun(A, B) -> maps:get(name, A) =< maps:get(name, B) end, Tags)}.
