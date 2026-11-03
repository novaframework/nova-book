-module(blog_post).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0]).

table() -> ~"posts".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = title, type = string, nullable = false},
        #kura_field{name = body, type = text, nullable = false},
        #kura_field{
            name = status,
            type = {enum, [draft, published, archived]},
            nullable = false,
            default = draft
        },
        #kura_field{name = user_id, type = bigint, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

indexes() ->
    [
        {[user_id], #{}},
        {[status], #{}}
    ].
