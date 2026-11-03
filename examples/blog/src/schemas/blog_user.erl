-module(blog_user).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0]).

table() -> ~"users".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = email, type = string, nullable = false},
        #kura_field{name = username, type = string, nullable = false},
        #kura_field{name = hashed_password, type = string, nullable = false},
        #kura_field{
            name = role, type = {enum, [author, admin]}, nullable = false, default = author
        },
        #kura_field{name = confirmed_at, type = utc_datetime},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime},
        #kura_field{name = password, type = string, virtual = true},
        #kura_field{name = password_confirmation, type = string, virtual = true}
    ].

indexes() ->
    [
        {[email], #{unique => true}},
        {[username], #{unique => true}}
    ].
