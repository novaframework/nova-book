-module(m20261103072100_alter_posts).
-moduledoc false.
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").
-export([up/0, down/0]).

-spec up() -> [kura_migration:operation()].
up() ->
    [
        {alter_table, ~"posts", [
            {add_column, #kura_column{name = published_at, type = utc_datetime}}
        ]}
    ].

-spec down() -> [kura_migration:operation()].
down() ->
    [
        {alter_table, ~"posts", [
            {drop_column, published_at}
        ]}
    ].
