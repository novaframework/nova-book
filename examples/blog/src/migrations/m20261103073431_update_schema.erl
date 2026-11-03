-module(m20261103073431_update_schema).
-moduledoc false.
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").
-export([up/0, down/0]).

-spec up() -> [kura_migration:operation()].
up() ->
    [
        {alter_table, ~"posts", [
            {add_column, #kura_column{name = deleted_at, type = utc_datetime}},
            {add_column, #kura_column{
                name = lock_version, type = integer, nullable = false, default = 1
            }}
        ]},
        {alter_table, ~"users", [
            {add_column, #kura_column{name = full_name, type = {encrypted, string}}}
        ]}
    ].

-spec down() -> [kura_migration:operation()].
down() ->
    [
        {alter_table, ~"posts", [
            {drop_column, deleted_at},
            {drop_column, lock_version}
        ]},
        {alter_table, ~"users", [
            {drop_column, full_name}
        ]}
    ].
