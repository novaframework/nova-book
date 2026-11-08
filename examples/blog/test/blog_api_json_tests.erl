-module(blog_api_json_tests).
-include_lib("eunit/include/eunit.hrl").

errors_group_messages_by_field_test() ->
    CS = blog_post:changeset(#{}, #{~"title" => ~"Hi"}),
    ?assertEqual(
        #{title => [~"should be at least 3 character(s)"], body => [~"can't be blank"]},
        blog_api_json:errors(CS)
    ).

datetimes_are_rfc3339_utc_test_() ->
    [
        ?_assertEqual(~"2026-11-03T08:30:00Z", blog_api_json:datetime({{2026, 11, 3}, {8, 30, 0}})),
        ?_assertEqual(null, blog_api_json:datetime(undefined))
    ].

post_includes_only_loaded_associations_test() ->
    Post = #{
        id => 1,
        title => ~"Hello",
        body => ~"World",
        status => published,
        user_id => 7,
        published_at => {{2026, 11, 3}, {8, 30, 0}},
        metadata => undefined,
        lock_version => 1,
        inserted_at => {{2026, 11, 3}, {8, 0, 0}},
        tags => [#{name => ~"otp"}, #{name => ~"erlang"}]
    },
    Json = blog_api_json:post(Post),
    ?assertMatch(#{id := 1, metadata := null, tags := [~"erlang", ~"otp"]}, Json),
    ?assertNot(maps:is_key(author, Json)),
    ?assertNot(maps:is_key(comments, Json)),
    ?assertMatch(
        #{
            ~"status" := ~"published",
            ~"published_at" := ~"2026-11-03T08:30:00Z",
            ~"metadata" := null
        },
        json:decode(iolist_to_binary(json:encode(Json)))
    ).
