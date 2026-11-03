-module(m20261103081738_create_user_tokens).
-moduledoc false.
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").
-export([up/0, down/0]).

-spec up() -> [kura_migration:operation()].
up() ->
    [
        {create_table, ~"user_tokens", [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{
                name = user_id,
                type = bigint,
                nullable = false,
                references = {~"users", id},
                on_delete = cascade
            },
            #kura_column{name = token, type = string, nullable = false},
            #kura_column{name = context, type = string, nullable = false},
            #kura_column{name = inserted_at, type = utc_datetime}
        ]},
        {create_index, ~"user_tokens", [context, token], #{unique => true}},
        {create_index, ~"user_tokens", [user_id], #{}}
    ].

-spec down() -> [kura_migration:operation()].
down() ->
    [{drop_table, ~"user_tokens"}].
