-module(blog_comment).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0, associations/0]).
-export([changeset/2, import_changeset/2]).

table() -> ~"comments".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = body, type = text, nullable = false},
        #kura_field{name = post_id, type = bigint, nullable = false},
        #kura_field{name = user_id, type = bigint, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

indexes() ->
    [
        {[post_id], #{}},
        {[user_id], #{}}
    ].

associations() ->
    [
        #kura_assoc{
            name = post,
            type = belongs_to,
            schema = blog_post,
            foreign_key = post_id,
            on_delete = cascade
        },
        #kura_assoc{
            name = author,
            type = belongs_to,
            schema = blog_user,
            foreign_key = user_id,
            on_delete = cascade
        }
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [body]),
    validate(CS).

import_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [body, user_id]),
    validate(kura_changeset:validate_required(CS, [user_id])).

validate(CS) ->
    CS1 = kura_changeset:validate_required(CS, [body]),
    CS2 = kura_changeset:validate_length(CS1, body, [{max, 5000}]),
    CS3 = kura_changeset:foreign_key_constraint(CS2, post_id),
    kura_changeset:foreign_key_constraint(CS3, user_id).
