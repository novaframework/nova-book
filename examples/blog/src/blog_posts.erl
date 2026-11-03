-module(blog_posts).
-include_lib("kura/include/kura.hrl").

-export([
    list_posts/1,
    get_post/1,
    get_post/2,
    create_post/2,
    update_post/2,
    delete_post/1,
    restore_post/1,
    purge_deleted/1,
    archive_stale_drafts/1,
    publish/2,
    import_posts/2,
    add_comment/3,
    import_post/2,
    tag_post/2,
    ensure_tags/1,
    tagged/1
]).

list_posts(Params) ->
    Types = #{
        page => integer,
        page_size => integer,
        status => {enum, [draft, published, archived]},
        tag => string
    },
    CS = kura_changeset:cast(Types, #{}, Params, [page, page_size, status, tag]),
    CS1 = kura_changeset:validate_number(CS, page, [{greater_than, 0}]),
    CS2 = kura_changeset:validate_number(CS1, page_size, [
        {greater_than, 0}, {less_than_or_equal_to, 50}
    ]),
    case kura_changeset:apply_action(CS2, list) of
        {ok, Filters} ->
            Query = filter_by_tag(filter_by_status(kura_query:from(blog_post), Filters), Filters),
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

filter_by_tag(Query, #{tag := Tag}) ->
    kura_query:where(Query, {id, in, {subquery, tagged_post_ids(Tag)}});
filter_by_tag(Query, #{}) ->
    Query.

tagged_post_ids(TagName) ->
    TagIds = kura_query:select(kura_query:where(kura_query:from(blog_tag), {name, TagName}), [id]),
    PostTags = kura_query:where(kura_query:from(blog_post_tag), {tag_id, in, {subquery, TagIds}}),
    kura_query:select(PostTags, [post_id]).

tagged(TagName) ->
    Query = kura_query:where(
        kura_query:from(blog_post), {id, in, {subquery, tagged_post_ids(TagName)}}
    ),
    blog_repo:all(kura_query:preload(Query, [tags])).

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
    Expected = expected_version(Post, Params),
    CS = blog_post:changeset(Post#{lock_version => Expected}, Params),
    blog_repo:update(kura_changeset:optimistic_lock(CS, lock_version)).

expected_version(#{lock_version := Current}, Params) ->
    case kura_types:cast(integer, maps:get(~"lock_version", Params, Current)) of
        {ok, Version} when is_integer(Version) -> Version;
        _ -> Current
    end.

delete_post(Post) ->
    blog_repo:soft_delete(kura_changeset:cast(blog_post, Post, #{}, [])).

restore_post(Id) ->
    Query = kura_query:only_deleted(kura_query:where(kura_query:from(blog_post), {id, Id})),
    case blog_repo:one(Query) of
        {ok, Post} -> blog_repo:restore(kura_changeset:cast(blog_post, Post, #{}, []));
        {error, not_found} -> {error, not_found}
    end.

purge_deleted(Before) ->
    Query = kura_query:only_deleted(kura_query:from(blog_post)),
    blog_repo:delete_all(kura_query:where(Query, {deleted_at, '<', Before})).

archive_stale_drafts(Before) ->
    Query = kura_query:where(kura_query:from(blog_post), {status, ~"draft"}),
    Stale = kura_query:where(Query, {updated_at, '<', Before}),
    blog_repo:update_all(Stale, #{status => archived, updated_at => calendar:universal_time()}).

publish(Post, TagNames) ->
    Multi = kura_multi:new(),
    Multi1 = kura_multi:update(Multi, post, blog_post:changeset(Post, #{status => published})),
    Multi2 = kura_multi:run(Multi1, tagged, fun(#{post := Published}) ->
        tag_post(Published, TagNames)
    end),
    case blog_repo:multi(Multi2) of
        {ok, #{tagged := Tagged}} -> {ok, Tagged};
        {error, Step, Reason, _Completed} -> {error, {Step, Reason}}
    end.

import_posts(Author, PostsParams) ->
    try blog_repo:transaction(fun() -> [import_or_throw(Author, P) || P <- PostsParams] end) of
        Imported -> {ok, Imported}
    catch
        throw:{import_failed, CS} -> {error, CS}
    end.

import_or_throw(Author, Params) ->
    case import_post(Author, Params) of
        {ok, Post} -> Post;
        {error, CS} -> throw({import_failed, CS})
    end.

add_comment(#{id := PostId}, #{id := AuthorId}, Params) ->
    CS = blog_comment:changeset(#{}, Params),
    CS1 = kura_changeset:put_change(CS, post_id, PostId),
    case blog_repo:insert(kura_changeset:put_change(CS1, user_id, AuthorId)) of
        {ok, Comment} ->
            ok = blog_jobs:comment_added(Comment),
            {ok, Comment};
        {error, _} = Error ->
            Error
    end.

import_post(#{id := AuthorId}, Params) ->
    CS = blog_post:changeset(#{}, Params),
    CS1 = kura_changeset:put_change(CS, user_id, AuthorId),
    CS2 = kura_changeset:cast_assoc(CS1, comments, #{with => fun blog_comment:import_changeset/2}),
    blog_repo:insert(CS2).

tag_post(Post, Names) ->
    case ensure_tags(Names) of
        {ok, Tags} ->
            CS = kura_changeset:cast(blog_post, Post, #{}, []),
            blog_repo:update(kura_changeset:put_assoc(CS, tags, Tags));
        {error, _} = Error ->
            Error
    end.

ensure_tags([]) ->
    {ok, []};
ensure_tags(Names) ->
    Changesets = [blog_tag:changeset(#{}, #{name => Name}) || Name <- lists:usort(Names)],
    case [CS || #kura_changeset{valid = false} = CS <- Changesets] of
        [Invalid | _] ->
            {error, Invalid};
        [] ->
            Rows = [kura_changeset:apply_changes(CS) || CS <- Changesets],
            {ok, _} = blog_repo:insert_all(blog_tag, Rows, #{on_conflict => {name, nothing}}),
            blog_repo:all(kura_query:where(kura_query:from(blog_tag), {name, in, Names}))
    end.
