-module(blog_tag).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0, associations/0]).
-export([changeset/2]).

table() -> ~"tags".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = name, type = string, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime}
    ].

indexes() ->
    [{[name], #{unique => true}}].

associations() ->
    [
        #kura_assoc{
            name = posts,
            type = many_to_many,
            schema = blog_post,
            join_through = blog_post_tag,
            join_keys = {tag_id, post_id}
        }
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [name]),
    CS1 = kura_changeset:validate_required(CS, [name]),
    CS2 = kura_changeset:validate_length(CS1, name, [{max, 30}]),
    kura_changeset:validate_format(CS2, name, ~"^[a-z0-9-]+\\z").
