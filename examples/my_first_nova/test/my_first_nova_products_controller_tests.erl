-module(my_first_nova_products_controller_tests).
-include_lib("eunit/include/eunit.hrl").

list_returns_products_test() ->
    Req = #{},
    Result = my_first_nova_products_controller:list(Req),
    ?assertMatch({json, #{products := [_|_]}}, Result).

show_existing_product_test() ->
    Req = #{bindings => #{<<"id">> => <<"1">>}},
    Result = my_first_nova_products_controller:show(Req),
    ?assertMatch({json, #{id := 1, name := _, price := _}}, Result).

create_with_valid_params_test() ->
    Req = #{params => #{<<"name">> => <<"Widget">>, <<"price">> => 999}},
    Result = my_first_nova_products_controller:create(Req),
    ?assertMatch({json, 201, #{}, #{id := 3, name := <<"Widget">>, price := 999}}, Result).

create_missing_params_test() ->
    Req = #{},
    Result = my_first_nova_products_controller:create(Req),
    ?assertMatch({status, 422, _, _}, Result).

update_product_test() ->
    Req = #{bindings => #{<<"id">> => <<"1">>},
            params => #{<<"name">> => <<"Updated">>, <<"price">> => 1500}},
    Result = my_first_nova_products_controller:update(Req),
    ?assertMatch({json, #{id := 1, name := <<"Updated">>, price := 1500}}, Result).

delete_product_test() ->
    Req = #{bindings => #{<<"id">> => <<"1">>}},
    Result = my_first_nova_products_controller:delete(Req),
    ?assertMatch({status, 204}, Result).
