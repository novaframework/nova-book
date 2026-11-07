-module(blog_post).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0, associations/0, embeds/0]).
-export([before_update/1, after_insert/1, after_update/1, after_delete/1]).
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
        #kura_field{name = lock_version, type = integer, nullable = false, default = 1},
        #kura_field{name = deleted_at, type = utc_datetime},
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
        #kura_assoc{name = comments, type = has_many, schema = blog_comment, foreign_key = post_id},
        #kura_assoc{
            name = tags,
            type = many_to_many,
            schema = blog_tag,
            join_through = blog_post_tag,
            join_keys = {post_id, tag_id}
        }
    ].

embeds() ->
    [#kura_embed{name = metadata, type = embeds_one, schema = blog_post_metadata}].

changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [title, body, status]),
    CS1 = kura_changeset:validate_required(CS, [title, body]),
    CS2 = kura_changeset:validate_length(CS1, title, [{min, 3}, {max, 200}]),
    CS3 = kura_changeset:foreign_key_constraint(CS2, user_id),
    CS4 = kura_changeset:cast_embed(CS3, metadata, #{with => fun blog_post_metadata:changeset/2}),
    stamp_published_at(CS4).

stamp_published_at(CS) ->
    case {kura_changeset:get_change(CS, status), kura_changeset:get_field(CS, published_at)} of
        {published, undefined} ->
            kura_changeset:put_change(CS, published_at, calendar:universal_time());
        _ ->
            CS
    end.

before_update(CS) ->
    kura_audit:stash(CS),
    {ok, CS}.

after_insert(Post) ->
    kura_audit:log(blog_repo, ?MODULE, insert, Post),
    {ok, Post}.

after_update(Post) ->
    kura_audit:log(blog_repo, ?MODULE, update, Post),
    {ok, Post}.

after_delete(Post) ->
    kura_audit:log(blog_repo, ?MODULE, delete, Post).
