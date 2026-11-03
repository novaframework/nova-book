-module(blog_hello_controller_tests).
-include_lib("nova_test/include/nova_test.hrl").

greet_test() ->
    Req = greet_req(~"Ada", #{}),
    ?assertJsonResponse(#{greeting := ~"Hello, Ada!"}, blog_hello_controller:greet(Req)).

greet_can_shout_test() ->
    Req = greet_req(~"Ada", #{~"shout" => ~"true"}),
    ?assertJsonResponse(#{greeting := ~"HELLO, ADA!"}, blog_hello_controller:greet(Req)).

greet_rejects_bad_names_test() ->
    Req = greet_req(~"<script>", #{}),
    ?assertEqual({error, invalid_name}, blog_hello_controller:greet(Req)).

create_from_json_test() ->
    Req = nova_test_req:with_json(#{~"name" => ~"Ada"}, nova_test_req:new(post, "/hello")),
    ?assertJsonResponse(201, #{name := ~"Ada"}, blog_hello_controller:create(Req)),
    {json, 201, Headers, _} = blog_hello_controller:create(Req),
    ?assertEqual(#{~"location" => ~"/hello/Ada"}, Headers).

create_without_a_name_test() ->
    Req = nova_test_req:with_json(#{}, nova_test_req:new(post, "/hello")),
    ?assertEqual({error, missing_name}, blog_hello_controller:create(Req)).

fallback_turns_errors_into_responses_test() ->
    Req = nova_test_req:new(get, "/hello/x"),
    ?assertJsonResponse(
        422,
        #{error := ~"invalid_name"},
        blog_fallback_controller:resolve(Req, {error, invalid_name})
    ),
    ?assertJsonResponse(
        400,
        #{error := ~"missing_name"},
        blog_fallback_controller:resolve(Req, {error, missing_name})
    ).

greet_req(Name, Query) ->
    Req = nova_test_req:new(get, <<"/hello/", Name/binary>>),
    (nova_test_req:with_bindings(#{~"name" => Name}, Req))#{parsed_qs => Query}.
