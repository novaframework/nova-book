-module(my_first_nova_api_controller_tests).
-include_lib("eunit/include/eunit.hrl").

index_returns_users_test() ->
    Req = #{},
    Result = my_first_nova_api_controller:index(Req),
    ?assertMatch({json, #{users := [_|_]}}, Result).

show_existing_user_test() ->
    Req = #{bindings => #{<<"id">> => <<"1">>}},
    Result = my_first_nova_api_controller:show(Req),
    ?assertMatch({json, #{id := 1, name := _, email := _}}, Result).

create_with_valid_params_test() ->
    Req = #{json => #{<<"name">> => <<"Charlie">>, <<"email">> => <<"charlie@example.com">>}},
    Result = my_first_nova_api_controller:create(Req),
    ?assertMatch({json, 201, #{}, #{id := 3, name := <<"Charlie">>, email := <<"charlie@example.com">>}}, Result).

create_missing_params_test() ->
    Req = #{},
    Result = my_first_nova_api_controller:create(Req),
    ?assertMatch({status, 422, _, _}, Result).
