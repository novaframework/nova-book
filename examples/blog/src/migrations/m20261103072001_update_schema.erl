-module(m20261103072001_update_schema).
-moduledoc false.
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").
-export([up/0, down/0]).

-spec up() -> [kura_migration:operation()].
up() ->
    [
        {create_table, ~"posts", [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{name = title, type = string, nullable = false},
            #kura_column{name = body, type = text, nullable = false},
            #kura_column{
                name = status,
                type = {enum, [draft, published, archived]},
                nullable = false,
                default = draft
            },
            #kura_column{name = user_id, type = bigint, nullable = false},
            #kura_column{name = inserted_at, type = utc_datetime},
            #kura_column{name = updated_at, type = utc_datetime}
        ]},
        {create_table, ~"users", [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{name = email, type = string, nullable = false},
            #kura_column{name = username, type = string, nullable = false},
            #kura_column{name = hashed_password, type = string, nullable = false},
            #kura_column{
                name = role, type = {enum, [author, admin]}, nullable = false, default = author
            },
            #kura_column{name = confirmed_at, type = utc_datetime},
            #kura_column{name = inserted_at, type = utc_datetime},
            #kura_column{name = updated_at, type = utc_datetime}
        ]},
        {create_index, ~"posts", [user_id], #{}},
        {create_index, ~"posts", [status], #{}},
        {create_index, ~"users", [email], #{unique => true}},
        {create_index, ~"users", [username], #{unique => true}}
    ].

-spec down() -> [kura_migration:operation()].
down() ->
    [
        {drop_table, ~"posts"},
        {drop_table, ~"users"}
    ].
