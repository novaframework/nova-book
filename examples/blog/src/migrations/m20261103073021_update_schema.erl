-module(m20261103073021_update_schema).
-moduledoc false.
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").
-export([up/0, down/0]).

-spec up() -> [kura_migration:operation()].
up() ->
    [
        {create_table, ~"tags", [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{name = name, type = string, nullable = false},
            #kura_column{name = inserted_at, type = utc_datetime}
        ]},
        {alter_table, ~"posts", [
            {add_column, #kura_column{name = metadata, type = jsonb}}
        ]},
        {create_index, ~"tags", [name], #{unique => true}}
    ].

-spec down() -> [kura_migration:operation()].
down() ->
    [
        {drop_table, ~"tags"},
        {alter_table, ~"posts", [
            {drop_column, metadata}
        ]}
    ].
