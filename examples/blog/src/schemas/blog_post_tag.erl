-module(blog_post_tag).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, key/0, fields/0, indexes/0, associations/0]).

table() -> ~"post_tags".

key() -> [post_id, tag_id].

fields() ->
    [
        #kura_field{name = post_id, type = bigint, nullable = false},
        #kura_field{name = tag_id, type = bigint, nullable = false}
    ].

indexes() ->
    [{[tag_id], #{}}].

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
            name = tag,
            type = belongs_to,
            schema = blog_tag,
            foreign_key = tag_id,
            on_delete = cascade
        }
    ].
