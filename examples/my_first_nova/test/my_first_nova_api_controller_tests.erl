-module(my_first_nova_api_controller_tests).
-include_lib("nova_test/include/nova_test.hrl").
-include_lib("kura/include/kura.hrl").

-define(USER, #{id => 1, name => <<"Alice">>, email => <<"alice@example.com">>}).

setup() ->
    meck:new(my_first_nova_repo, [non_strict]),
    meck:new(kura_query, [non_strict]),
    meck:new(kura_changeset, [non_strict]),
    meck:expect(kura_query, from, fun(user) -> {query, user} end),
    ok.

cleanup(_) ->
    meck:unload().

users_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun index_returns_users/0,
        fun show_existing_user/0,
        fun show_missing_user/0,
        fun create_with_valid_params/0,
        fun create_missing_params/0
    ]}.

index_returns_users() ->
    Users = [?USER],
    meck:expect(my_first_nova_repo, all, fun({query, user}) -> {ok, Users} end),
    Req = nova_test_req:new(get, "/api/users"),
    Result = my_first_nova_api_controller:index(Req),
    ?assertJsonResponse(#{users := [_ | _]}, Result).

show_existing_user() ->
    meck:expect(my_first_nova_repo, get, fun(user, 1) -> {ok, ?USER} end),
    Req = nova_test_req:with_bindings(
        #{<<"id">> => <<"1">>}, nova_test_req:new(get, "/api/users/1")
    ),
    Result = my_first_nova_api_controller:show(Req),
    ?assertJsonResponse(#{id := 1, name := <<"Alice">>}, Result).

show_missing_user() ->
    meck:expect(my_first_nova_repo, get, fun(user, 99) -> {error, not_found} end),
    Req = nova_test_req:with_bindings(
        #{<<"id">> => <<"99">>}, nova_test_req:new(get, "/api/users/99")
    ),
    Result = my_first_nova_api_controller:show(Req),
    ?assertMatch({status, 404, _, _}, Result).

create_with_valid_params() ->
    CS = #kura_changeset{valid = true},
    meck:expect(kura_changeset, cast, fun(user, #{}, _, [name, email]) -> CS end),
    meck:expect(kura_changeset, validate_required, fun(C, [name, email]) -> C end),
    meck:expect(my_first_nova_repo, insert, fun(_) -> {ok, ?USER} end),
    Req = nova_test_req:with_json(
        #{<<"name">> => <<"Alice">>, <<"email">> => <<"alice@example.com">>},
        nova_test_req:new(post, "/api/users")
    ),
    Result = my_first_nova_api_controller:create(Req),
    ?assertJsonResponse(201, #{id := 1, name := <<"Alice">>}, Result).

create_missing_params() ->
    Req = nova_test_req:new(post, "/api/users"),
    Result = my_first_nova_api_controller:create(Req),
    ?assertMatch({status, 422, _, _}, Result).
