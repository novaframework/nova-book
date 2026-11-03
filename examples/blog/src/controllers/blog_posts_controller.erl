-module(blog_posts_controller).
-include_lib("kura/include/kura.hrl").

-export([
    index/1,
    show/1,
    create/1,
    update/1,
    delete/1,
    create_comment/1,
    update_tags/1
]).

index(Req) ->
    Params = maps:from_list(cowboy_req:parse_qs(Req)),
    case blog_posts:list_posts(Params) of
        {ok, #{entries := Posts} = Page} ->
            {json, #{
                posts => [blog_api_json:post(Post) || Post <- Posts],
                page => maps:get(page, Page),
                total_pages => maps:get(total_pages, Page),
                total_entries => maps:get(total_entries, Page)
            }};
        {error, #kura_changeset{} = CS} ->
            {json, 400, #{}, #{errors => blog_api_json:errors(CS)}}
    end.

show(#{bindings := #{~"id" := Id}}) ->
    case blog_posts:get_post(Id, [author, tags, {comments, [author]}]) of
        {ok, Post} -> {json, blog_api_json:post(Post)};
        {error, not_found} -> not_found()
    end.

create(#{json := Params, auth_data := Actor}) ->
    case blog_posts:create_post(Actor, Params) of
        {ok, Post} -> {json, 201, #{}, blog_api_json:post(Post)};
        {error, #kura_changeset{} = CS} -> unprocessable(CS)
    end;
create(_Req) ->
    {json, 400, #{}, #{error => ~"expected a JSON body"}}.

update(#{bindings := #{~"id" := Id}, json := Params, auth_data := Actor}) ->
    case authorize(Actor, update_post, blog_posts:get_post(Id)) of
        {ok, Post} ->
            case blog_posts:update_post(Post, Params) of
                {ok, Updated} -> {json, blog_api_json:post(Updated)};
                {error, stale} -> {json, 409, #{}, #{error => ~"post was changed by someone else"}};
                {error, #kura_changeset{} = CS} -> unprocessable(CS)
            end;
        Denied ->
            Denied
    end;
update(_Req) ->
    {json, 400, #{}, #{error => ~"expected a JSON body"}}.

delete(#{bindings := #{~"id" := Id}, auth_data := Actor}) ->
    case authorize(Actor, delete_post, blog_posts:get_post(Id)) of
        {ok, Post} ->
            {ok, _} = blog_posts:delete_post(Post),
            {status, 204};
        Denied ->
            Denied
    end.

create_comment(#{bindings := #{~"id" := Id}, json := Params, auth_data := Actor}) ->
    case blog_posts:get_post(Id) of
        {ok, Post} ->
            case blog_posts:add_comment(Post, Actor, Params) of
                {ok, Comment} -> {json, 201, #{}, blog_api_json:comment(Comment)};
                {error, #kura_changeset{} = CS} -> unprocessable(CS)
            end;
        {error, not_found} ->
            not_found()
    end.

update_tags(#{bindings := #{~"id" := Id}, json := #{~"tags" := Names}, auth_data := Actor}) when
    is_list(Names)
->
    case authorize(Actor, update_post, blog_posts:get_post(Id)) of
        {ok, Post} ->
            case blog_posts:tag_post(Post, Names) of
                {ok, Tagged} -> {json, blog_api_json:post(Tagged)};
                {error, #kura_changeset{} = CS} -> unprocessable(CS)
            end;
        Denied ->
            Denied
    end;
update_tags(_Req) ->
    {json, 400, #{}, #{error => ~"expected a tags list"}}.

authorize(Actor, Action, {ok, Post}) ->
    case blog_policy:can(Actor, Action, Post) of
        true -> {ok, Post};
        false -> {json, 403, #{}, #{error => ~"you can only change your own posts"}}
    end;
authorize(_Actor, _Action, {error, not_found}) ->
    not_found().

not_found() ->
    {json, 404, #{}, #{error => ~"post not found"}}.

unprocessable(CS) ->
    {json, 422, #{}, #{errors => blog_api_json:errors(CS)}}.
