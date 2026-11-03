-module(blog_publish_worker).
-behaviour(shigoto_worker).

-export([perform/1]).

perform(#{~"post_id" := PostId}) ->
    case blog_repo:get(blog_post, PostId) of
        {ok, #{status := draft} = Post} ->
            case blog_repo:update(blog_post:changeset(Post, #{status => published})) of
                {ok, _Published} -> ok;
                {error, _Changeset} -> {error, invalid_post}
            end;
        {ok, _AlreadyPublishedOrArchived} ->
            ok;
        {error, not_found} ->
            ok
    end.
