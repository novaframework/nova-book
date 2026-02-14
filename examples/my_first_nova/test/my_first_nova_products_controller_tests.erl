-module(my_first_nova_products_controller_tests).
-include_lib("nova_test/include/nova_test.hrl").
-include_lib("kura/include/kura.hrl").

-define(PRODUCT, #{id => 1, name => <<"Widget">>, price => 999, description => <<"A fine widget">>}).

setup() ->
    meck:new(my_first_nova_repo, [non_strict]),
    meck:new(kura_query, [non_strict]),
    meck:new(kura_changeset, [non_strict]),
    meck:expect(kura_query, from, fun(product) -> {query, product} end),
    ok.

cleanup(_) ->
    meck:unload().

products_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun list_returns_products/0,
        fun show_existing_product/0,
        fun show_missing_product/0,
        fun create_with_valid_params/0,
        fun create_missing_params/0,
        fun update_product/0,
        fun update_missing_product/0,
        fun delete_product/0,
        fun delete_missing_product/0
    ]}.

list_returns_products() ->
    Products = [?PRODUCT],
    meck:expect(my_first_nova_repo, all, fun({query, product}) -> {ok, Products} end),
    Req = nova_test_req:new(get, "/api/products"),
    Result = my_first_nova_products_controller:list(Req),
    ?assertJsonResponse(#{products := [_ | _]}, Result).

show_existing_product() ->
    meck:expect(my_first_nova_repo, get, fun(product, 1) -> {ok, ?PRODUCT} end),
    Req = nova_test_req:with_bindings(
        #{<<"id">> => <<"1">>}, nova_test_req:new(get, "/api/products/1")
    ),
    Result = my_first_nova_products_controller:show(Req),
    ?assertJsonResponse(#{id := 1, name := <<"Widget">>}, Result).

show_missing_product() ->
    meck:expect(my_first_nova_repo, get, fun(product, 99) -> {error, not_found} end),
    Req = nova_test_req:with_bindings(
        #{<<"id">> => <<"99">>}, nova_test_req:new(get, "/api/products/99")
    ),
    Result = my_first_nova_products_controller:show(Req),
    ?assertMatch({status, 404, _, _}, Result).

create_with_valid_params() ->
    CS = #kura_changeset{valid = true},
    meck:expect(kura_changeset, cast, fun(product, #{}, _, [name, price, description]) -> CS end),
    meck:expect(kura_changeset, validate_required, fun(C, [name, price]) -> C end),
    meck:expect(my_first_nova_repo, insert, fun(_) -> {ok, ?PRODUCT} end),
    Req = nova_test_req:with_json(
        #{<<"name">> => <<"Widget">>, <<"price">> => 999},
        nova_test_req:new(post, "/api/products")
    ),
    Result = my_first_nova_products_controller:create(Req),
    ?assertJsonResponse(201, #{id := 1, name := <<"Widget">>}, Result).

create_missing_params() ->
    Req = nova_test_req:new(post, "/api/products"),
    Result = my_first_nova_products_controller:create(Req),
    ?assertMatch({status, 422, _, _}, Result).

update_product() ->
    Existing = ?PRODUCT,
    Updated = Existing#{name => <<"Updated">>},
    CS = #kura_changeset{valid = true},
    meck:expect(my_first_nova_repo, get, fun(product, 1) -> {ok, Existing} end),
    meck:expect(kura_changeset, cast, fun(product, _, _, [name, price, description]) -> CS end),
    meck:expect(my_first_nova_repo, update, fun(_) -> {ok, Updated} end),
    Req = nova_test_req:with_json(
        #{<<"name">> => <<"Updated">>},
        nova_test_req:with_bindings(
            #{<<"id">> => <<"1">>}, nova_test_req:new(put, "/api/products/1")
        )
    ),
    Result = my_first_nova_products_controller:update(Req),
    ?assertJsonResponse(#{name := <<"Updated">>}, Result).

update_missing_product() ->
    meck:expect(my_first_nova_repo, get, fun(product, 99) -> {error, not_found} end),
    Req = nova_test_req:with_json(
        #{<<"name">> => <<"Nope">>},
        nova_test_req:with_bindings(
            #{<<"id">> => <<"99">>}, nova_test_req:new(put, "/api/products/99")
        )
    ),
    Result = my_first_nova_products_controller:update(Req),
    ?assertMatch({status, 404, _, _}, Result).

delete_product() ->
    CS = #kura_changeset{valid = true},
    meck:expect(my_first_nova_repo, get, fun(product, 1) -> {ok, ?PRODUCT} end),
    meck:expect(kura_changeset, cast, fun(product, _, #{}, []) -> CS end),
    meck:expect(my_first_nova_repo, delete, fun(_) -> {ok, ?PRODUCT} end),
    Req = nova_test_req:with_bindings(
        #{<<"id">> => <<"1">>},
        nova_test_req:new(delete, "/api/products/1")
    ),
    Result = my_first_nova_products_controller:delete(Req),
    ?assertStatusResponse(204, Result).

delete_missing_product() ->
    meck:expect(my_first_nova_repo, get, fun(product, 99) -> {error, not_found} end),
    Req = nova_test_req:with_bindings(
        #{<<"id">> => <<"99">>},
        nova_test_req:new(delete, "/api/products/99")
    ),
    Result = my_first_nova_products_controller:delete(Req),
    ?assertMatch({status, 404, _, _}, Result).
