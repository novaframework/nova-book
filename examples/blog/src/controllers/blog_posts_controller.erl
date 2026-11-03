-module(blog_posts_controller).
-include_lib("kura/include/kura.hrl").

-export([
    index/1,
    show/1,
    create/1,
    update/1,
    delete/1
]).

index(Req) ->
    Params = maps:from_list(cowboy_req:parse_qs(Req)),
    case blog_posts:list_posts(Params) of
        {ok, #{entries := Posts} = Page} ->
            {json, #{
                posts => [blog_json:post(Post) || Post <- Posts],
                page => maps:get(page, Page),
                total_pages => maps:get(total_pages, Page),
                total_entries => maps:get(total_entries, Page)
            }};
        {error, #kura_changeset{} = CS} ->
            {json, 400, #{}, #{errors => blog_json:errors(CS)}}
    end.

show(#{bindings := #{~"id" := Id}}) ->
    case blog_posts:get_post(Id) of
        {ok, Post} -> {json, blog_json:post(Post)};
        {error, not_found} -> not_found()
    end.

create(#{json := Params} = Req) ->
    case current_author(Req) of
        {ok, Author} ->
            case blog_posts:create_post(Author, Params) of
                {ok, Post} -> {json, 201, #{}, blog_json:post(Post)};
                {error, #kura_changeset{} = CS} -> unprocessable(CS)
            end;
        {error, not_found} ->
            {json, 422, #{}, #{errors => #{user_id => [~"does not exist"]}}}
    end;
create(_Req) ->
    {json, 400, #{}, #{error => ~"expected a JSON body"}}.

update(#{bindings := #{~"id" := Id}, json := Params}) ->
    case blog_posts:get_post(Id) of
        {ok, Post} ->
            case blog_posts:update_post(Post, Params) of
                {ok, Updated} -> {json, blog_json:post(Updated)};
                {error, #kura_changeset{} = CS} -> unprocessable(CS)
            end;
        {error, not_found} ->
            not_found()
    end;
update(_Req) ->
    {json, 400, #{}, #{error => ~"expected a JSON body"}}.

delete(#{bindings := #{~"id" := Id}}) ->
    case blog_posts:get_post(Id) of
        {ok, Post} ->
            {ok, _} = blog_posts:delete_post(Post),
            {status, 204};
        {error, not_found} ->
            not_found()
    end.

current_author(#{json := #{~"user_id" := UserId}}) when is_integer(UserId) ->
    blog_repo:get(blog_user, UserId);
current_author(_Req) ->
    {error, not_found}.

not_found() ->
    {json, 404, #{}, #{error => ~"post not found"}}.

unprocessable(CS) ->
    {json, 422, #{}, #{errors => blog_json:errors(CS)}}.
