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
        {create_table, ~"post_tags",
            [
                #kura_column{
                    name = post_id,
                    type = bigint,
                    nullable = false,
                    references = {~"posts", id},
                    on_delete = cascade
                },
                #kura_column{
                    name = tag_id,
                    type = bigint,
                    nullable = false,
                    references = {~"tags", id},
                    on_delete = cascade
                }
            ],
            [{primary_key, [post_id, tag_id]}]},
        {alter_table, ~"posts", [
            {add_column, #kura_column{name = metadata, type = jsonb}}
        ]},
        {create_index, ~"post_tags", [tag_id], #{}},
        {create_index, ~"tags", [name], #{unique => true}}
    ].

-spec down() -> [kura_migration:operation()].
down() ->
    [
        {alter_table, ~"posts", [
            {drop_column, metadata}
        ]},
        {drop_table, ~"post_tags"},
        {drop_table, ~"tags"}
    ].
