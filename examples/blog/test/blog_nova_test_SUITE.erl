-module(blog_nova_test_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    heartbeat_answers/1,
    posts_api_lists_published_posts/1,
    creating_a_post_needs_a_token/1,
    unknown_routes_are_json_404s/1
]).

all() ->
    [
        heartbeat_answers,
        posts_api_lists_published_posts,
        creating_a_post_needs_a_token,
        unknown_routes_are_json_404s
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    nova_test:start(blog, Config).

end_per_suite(Config) ->
    nova_test:stop(Config).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

heartbeat_answers(Config) ->
    {ok, Resp} = nova_test:get("/heartbeat", Config),
    ?assertStatus(200, Resp).

posts_api_lists_published_posts(Config) ->
    Alice = ?config(alice, Config),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Out now", ~"status" => ~"published"}),
    {ok, Resp} = nova_test:get("/api/posts?status=published", Config),
    ?assertStatus(200, Resp),
    ?assertHeader("content-type", "application/json", Resp),
    ?assertJson(#{~"posts" := [#{~"title" := ~"Out now"}], ~"total_entries" := 1}, Resp).

creating_a_post_needs_a_token(Config) ->
    Post = #{json => #{~"title" => ~"Via nova_test", ~"body" => ~"Hello"}},
    {ok, Anonymous} = nova_test:post("/api/posts", Post, Config),
    ?assertStatus(401, Anonymous),
    ?assertJson(#{~"error" := ~"missing bearer token"}, Anonymous),
    Token = blog_data_fixtures:token(?config(alice, Config)),
    Authorized = Post#{headers => [{~"authorization", <<"Bearer ", Token/binary>>}]},
    {ok, Created} = nova_test:post("/api/posts", Authorized, Config),
    ?assertStatus(201, Created),
    ?assertJson(#{~"title" := ~"Via nova_test", ~"status" := ~"draft"}, Created).

unknown_routes_are_json_404s(Config) ->
    {ok, Resp} = nova_test:get("/no/such/page", Config),
    ?assertStatus(404, Resp),
    ?assertJson(#{~"error" := ~"not_found"}, Resp).
