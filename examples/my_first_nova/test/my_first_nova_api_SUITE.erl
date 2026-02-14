-module(my_first_nova_api_SUITE).
-include_lib("nova_test/include/nova_test.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    products_crud/1,
    users_crud/1
]).

all() -> [products_crud, users_crud].

init_per_suite(Config) ->
    nova_test:start(my_first_nova, Config).

end_per_suite(Config) ->
    nova_test:stop(Config).

init_per_testcase(_TC, Config) ->
    my_first_nova_repo:query(<<"TRUNCATE products, users RESTART IDENTITY">>, []),
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% Products

products_crud(Config) ->
    %% List (empty)
    {ok, R1} = nova_test:get("/api/products", Config),
    ?assertStatus(200, R1),
    ?assertJson(#{<<"products">> := []}, R1),

    %% Create
    {ok, R2} = nova_test:post(
        "/api/products",
        #{
            json => #{
                <<"name">> => <<"Widget">>,
                <<"price">> => 999,
                <<"description">> => <<"A fine widget">>
            }
        },
        Config
    ),
    ?assertStatus(201, R2),
    #{<<"id">> := ProductId} = nova_test:json(R2),

    %% Show
    {ok, R3} = nova_test:get("/api/products/" ++ integer_to_list(ProductId), Config),
    ?assertStatus(200, R3),
    ?assertJson(#{<<"name">> := <<"Widget">>, <<"price">> := 999}, R3),

    %% Update
    {ok, R4} = nova_test:put(
        "/api/products/" ++ integer_to_list(ProductId),
        #{
            json => #{
                <<"name">> => <<"Super Widget">>,
                <<"price">> => 1999
            }
        },
        Config
    ),
    ?assertStatus(200, R4),
    ?assertJson(#{<<"name">> := <<"Super Widget">>, <<"price">> := 1999}, R4),

    %% List (has one)
    {ok, R5} = nova_test:get("/api/products", Config),
    ?assertStatus(200, R5),
    #{<<"products">> := [_]} = nova_test:json(R5),

    %% Delete
    {ok, R6} = nova_test:delete("/api/products/" ++ integer_to_list(ProductId), Config),
    ?assertStatus(204, R6),

    %% Show after delete (404)
    {ok, R7} = nova_test:get("/api/products/" ++ integer_to_list(ProductId), Config),
    ?assertStatus(404, R7),

    ok.

%% Users

users_crud(Config) ->
    %% List (empty)
    {ok, R1} = nova_test:get("/api/users", Config),
    ?assertStatus(200, R1),
    ?assertJson(#{<<"users">> := []}, R1),

    %% Create
    {ok, R2} = nova_test:post(
        "/api/users",
        #{
            json => #{
                <<"name">> => <<"Alice">>,
                <<"email">> => <<"alice@example.com">>
            }
        },
        Config
    ),
    ?assertStatus(201, R2),
    #{<<"id">> := UserId} = nova_test:json(R2),

    %% Show
    {ok, R3} = nova_test:get("/api/users/" ++ integer_to_list(UserId), Config),
    ?assertStatus(200, R3),
    ?assertJson(#{<<"name">> := <<"Alice">>, <<"email">> := <<"alice@example.com">>}, R3),

    %% List (has one)
    {ok, R4} = nova_test:get("/api/users", Config),
    ?assertStatus(200, R4),
    #{<<"users">> := [_]} = nova_test:json(R4),

    ok.
