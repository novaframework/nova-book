-module(blog_tags).

-export([list_tags/0, get_tag/1, get_tag/2, create_tag/1, update_tag/2, delete_tag/1]).

list_tags() ->
    blog_repo:all(kura_query:order_by(kura_query:from(blog_tag), [{name, asc}])).

get_tag(Id) ->
    case kura_types:cast(id, Id) of
        {ok, TagId} when is_integer(TagId) -> blog_repo:get(blog_tag, TagId);
        _ -> {error, not_found}
    end.

get_tag(Id, Preloads) ->
    case get_tag(Id) of
        {ok, Tag} -> {ok, blog_repo:preload(blog_tag, Tag, Preloads)};
        {error, not_found} -> {error, not_found}
    end.

create_tag(Params) ->
    blog_repo:insert(blog_tag:changeset(#{}, Params)).

update_tag(Tag, Params) ->
    blog_repo:update(blog_tag:changeset(Tag, Params)).

delete_tag(Tag) ->
    blog_repo:delete(kura_changeset:cast(blog_tag, Tag, #{}, [])).
