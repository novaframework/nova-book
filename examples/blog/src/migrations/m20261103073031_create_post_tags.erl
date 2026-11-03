-module(m20261103073031_create_post_tags).
-moduledoc false.
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").
-export([up/0, down/0]).

-spec up() -> [kura_migration:operation()].
up() ->
    [
        {create_table, ~"post_tags", [
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
        ]},
        {create_index, ~"post_tags", [post_id, tag_id], #{unique => true}},
        {create_index, ~"post_tags", [tag_id], #{}}
    ].

-spec down() -> [kura_migration:operation()].
down() ->
    [{drop_table, ~"post_tags"}].
