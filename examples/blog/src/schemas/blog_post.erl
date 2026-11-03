-module(blog_post).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0, associations/0]).
-export([changeset/2]).

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
        #kura_field{name = published_at, type = utc_datetime},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

indexes() ->
    [
        {[user_id], #{}},
        {[status], #{}}
    ].

associations() ->
    [
        #kura_assoc{
            name = author,
            type = belongs_to,
            schema = blog_user,
            foreign_key = user_id,
            on_delete = cascade
        },
        #kura_assoc{name = comments, type = has_many, schema = blog_comment, foreign_key = post_id}
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [title, body, status]),
    CS1 = kura_changeset:validate_required(CS, [title, body]),
    CS2 = kura_changeset:validate_length(CS1, title, [{min, 3}, {max, 200}]),
    CS3 = kura_changeset:foreign_key_constraint(CS2, user_id),
    stamp_published_at(CS3).

stamp_published_at(CS) ->
    case {kura_changeset:get_change(CS, status), kura_changeset:get_field(CS, published_at)} of
        {published, undefined} ->
            kura_changeset:put_change(CS, published_at, calendar:universal_time());
        _ ->
            CS
    end.
