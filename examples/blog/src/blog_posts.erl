-module(blog_posts).
-include_lib("kura/include/kura.hrl").

-export([
    list_posts/1,
    get_post/1,
    get_post/2,
    create_post/2,
    update_post/2,
    delete_post/1,
    add_comment/3,
    import_post/2
]).

list_posts(Params) ->
    Types = #{
        page => integer, page_size => integer, status => {enum, [draft, published, archived]}
    },
    CS = kura_changeset:cast(Types, #{}, Params, [page, page_size, status]),
    CS1 = kura_changeset:validate_number(CS, page, [{greater_than, 0}]),
    CS2 = kura_changeset:validate_number(CS1, page_size, [
        {greater_than, 0}, {less_than_or_equal_to, 50}
    ]),
    case kura_changeset:apply_action(CS2, list) of
        {ok, Filters} ->
            Query = filter_by_status(kura_query:from(blog_post), Filters),
            Newest = kura_query:order_by(Query, [{inserted_at, desc}, {id, desc}]),
            kura_paginator:paginate(blog_repo, Newest, #{
                page => maps:get(page, Filters, 1),
                page_size => maps:get(page_size, Filters, 10)
            });
        {error, _} = Error ->
            Error
    end.

filter_by_status(Query, #{status := Status}) ->
    kura_query:where(Query, {status, atom_to_binary(Status)});
filter_by_status(Query, #{}) ->
    Query.

get_post(Id) ->
    case kura_types:cast(id, Id) of
        {ok, PostId} when is_integer(PostId) -> blog_repo:get(blog_post, PostId);
        _ -> {error, not_found}
    end.

get_post(Id, Preloads) ->
    case get_post(Id) of
        {ok, Post} -> {ok, blog_repo:preload(blog_post, Post, Preloads)};
        {error, not_found} -> {error, not_found}
    end.

create_post(#{id := AuthorId}, Params) ->
    CS = blog_post:changeset(#{}, Params),
    blog_repo:insert(kura_changeset:put_change(CS, user_id, AuthorId)).

update_post(Post, Params) ->
    blog_repo:update(blog_post:changeset(Post, Params)).

delete_post(Post) ->
    blog_repo:delete(kura_changeset:cast(blog_post, Post, #{}, [])).

add_comment(#{id := PostId}, #{id := AuthorId}, Params) ->
    CS = blog_comment:changeset(#{}, Params),
    CS1 = kura_changeset:put_change(CS, post_id, PostId),
    blog_repo:insert(kura_changeset:put_change(CS1, user_id, AuthorId)).

import_post(#{id := AuthorId}, Params) ->
    CS = blog_post:changeset(#{}, Params),
    CS1 = kura_changeset:put_change(CS, user_id, AuthorId),
    CS2 = kura_changeset:cast_assoc(CS1, comments, #{with => fun blog_comment:import_changeset/2}),
    blog_repo:insert(CS2).
