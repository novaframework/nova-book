-module(m20260214180502_update_schema).
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").
-export([up/0, down/0]).

up() ->
    [{create_table, <<"products">>, [
        #kura_column{name = id, type = id, primary_key = true, nullable = false},
        #kura_column{name = name, type = string, nullable = false},
        #kura_column{name = price, type = integer, nullable = false},
        #kura_column{name = description, type = text},
        #kura_column{name = inserted_at, type = utc_datetime},
        #kura_column{name = updated_at, type = utc_datetime}
    ]},
     {create_table, <<"users">>, [
        #kura_column{name = id, type = id, primary_key = true, nullable = false},
        #kura_column{name = name, type = string, nullable = false},
        #kura_column{name = email, type = string, nullable = false},
        #kura_column{name = inserted_at, type = utc_datetime},
        #kura_column{name = updated_at, type = utc_datetime}
    ]}].

down() ->
    [{drop_table, <<"products">>},
     {drop_table, <<"users">>}].
