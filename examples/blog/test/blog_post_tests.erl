-module(blog_post_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

valid_post_test() ->
    CS = blog_post:changeset(#{}, #{~"title" => ~"Hello", ~"body" => ~"World"}),
    ?assert(CS#kura_changeset.valid),
    ?assertEqual(~"Hello", kura_changeset:get_change(CS, title)).

title_and_body_are_required_test() ->
    CS = blog_post:changeset(#{}, #{}),
    ?assertNot(CS#kura_changeset.valid),
    ?assertEqual(
        [{title, ~"can't be blank"}, {body, ~"can't be blank"}], CS#kura_changeset.errors
    ).

title_length_test_() ->
    Errors = fun(Title) ->
        CS = blog_post:changeset(#{}, #{~"title" => Title, ~"body" => ~"x"}),
        CS#kura_changeset.errors
    end,
    [
        ?_assertEqual([{title, ~"should be at least 3 character(s)"}], Errors(~"Hi")),
        ?_assertEqual([], Errors(~"Hey")),
        ?_assertEqual([], Errors(binary:copy(~"a", 200))),
        ?_assertEqual(
            [{title, ~"should be at most 200 character(s)"}], Errors(binary:copy(~"a", 201))
        )
    ].

metadata_is_validated_as_an_embed_test() ->
    Params = #{
        ~"title" => ~"Hello",
        ~"body" => ~"World",
        ~"metadata" => #{~"cover_image_url" => ~"http://example.com/a.png", ~"reading_minutes" => 0}
    },
    CS = blog_post:changeset(#{}, Params),
    ?assertNot(CS#kura_changeset.valid),
    ?assertEqual(
        #{
            cover_image_url => [~"has invalid format"],
            reading_minutes => [~"must be greater than 0"]
        },
        blog_api_json:errors(CS)
    ).

publishing_stamps_published_at_test() ->
    Draft = #{id => 1, title => ~"Hello", body => ~"x", status => draft, published_at => undefined},
    CS = blog_post:changeset(Draft, #{~"status" => ~"published"}),
    ?assertMatch({{_, _, _}, {_, _, _}}, kura_changeset:get_change(CS, published_at)).
