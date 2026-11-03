-module(m20261103072804_update_schema).
-moduledoc false.
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").
-export([up/0, down/0]).

-spec up() -> [kura_migration:operation()].
up() ->
    [
        {create_table, ~"comments", [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{name = body, type = text, nullable = false},
            #kura_column{
                name = post_id,
                type = bigint,
                nullable = false,
                references = {~"posts", id},
                on_delete = cascade
            },
            #kura_column{
                name = user_id,
                type = bigint,
                nullable = false,
                references = {~"users", id},
                on_delete = cascade
            },
            #kura_column{name = inserted_at, type = utc_datetime},
            #kura_column{name = updated_at, type = utc_datetime}
        ]},
        {execute,
            ~"ALTER TABLE \"posts\" ADD CONSTRAINT \"posts_user_id_fkey\" FOREIGN KEY (\"user_id\") REFERENCES \"users\" (\"id\") ON DELETE CASCADE"},
        {create_index, ~"comments", [post_id], #{}},
        {create_index, ~"comments", [user_id], #{}}
    ].

-spec down() -> [kura_migration:operation()].
down() ->
    [
        {drop_table, ~"comments"},
        {execute, ~"ALTER TABLE \"posts\" DROP CONSTRAINT \"posts_user_id_fkey\""}
    ].
