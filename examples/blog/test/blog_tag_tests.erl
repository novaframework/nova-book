-module(blog_tag_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

valid_name_test() ->
    ?assertMatch(#kura_changeset{valid = true}, blog_tag:changeset(#{}, #{~"name" => ~"erlang"})).

name_with_a_trailing_newline_is_invalid_test() ->
    ?assertMatch(
        #kura_changeset{valid = false}, blog_tag:changeset(#{}, #{~"name" => ~"erlang\n"})
    ).

live_input_trims_and_validates_test() ->
    ?assertEqual({ok, ~"erlang"}, blog_live_tag_input:normalize(~" Erlang\n")),
    ?assertMatch({error, _}, blog_live_tag_input:normalize(~"erl\nang")).
