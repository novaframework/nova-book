-module(blog_posts_html_controller).
-include_lib("kura/include/kura.hrl").

-export([index/1, show/1, new/1, create/1, edit/1, update/1, delete/1]).

-define(FIELDS, [title, body, status]).

index(Req) ->
    Query = maps:from_list(cowboy_req:parse_qs(Req)),
    Params = #{~"status" => ~"published", ~"page" => maps:get(~"page", Query, ~"1")},
    case blog_posts:list_posts(Params) of
        {ok, #{entries := Posts, page := Page, total_pages := Pages}} ->
            WithAuthors = blog_repo:preload(blog_post, Posts, [author]),
            Vars = pager(Page, Pages, #{posts => [blog_view:post(P) || P <- WithAuthors]}),
            blog_view:render(Req, blog_post_index, Vars);
        {error, #kura_changeset{}} ->
            {status, 400}
    end.

show(#{bindings := #{~"id" := Id}} = Req) ->
    Actor = maps:get(auth_data, Req, undefined),
    case blog_posts:get_post(Id, [author]) of
        {ok, #{status := published} = Post} ->
            render_post(Req, Actor, Post);
        {ok, Post} ->
            case blog_policy:can(Actor, view_draft, Post) of
                true -> render_post(Req, Actor, Post);
                false -> blog_error_controller:not_found(Req)
            end;
        {error, not_found} ->
            blog_error_controller:not_found(Req)
    end.

new(Req) ->
    Vars = #{form => blog_view:form(?FIELDS, #{status => draft}), errors => #{}},
    blog_view:render(Req, blog_post_new, Vars).

create(#{auth_data := Actor} = Req) ->
    Params = maps:get(params, Req, #{}),
    case blog_posts:create_post(Actor, Params) of
        {ok, #{id := Id}} ->
            ok = blog_flash:put(Req, info, ~"Post created."),
            {redirect, post_path(Id)};
        {error, #kura_changeset{} = CS} ->
            Vars = #{form => blog_view:form(?FIELDS, Params), errors => blog_view:errors(CS)},
            blog_view:render(Req, blog_post_new, Vars, 422)
    end.

edit(Req) ->
    with_own_post(Req, update_post, fun edit_form/2).

update(Req) ->
    with_own_post(Req, update_post, fun save/2).

delete(Req) ->
    with_own_post(Req, delete_post, fun remove/2).

edit_form(Req, Post) ->
    Vars = #{post => blog_view:post(Post), form => blog_view:form(?FIELDS, Post), errors => #{}},
    blog_view:render(Req, blog_post_edit, Vars).

save(Req, #{id := Id} = Post) ->
    Params = maps:get(params, Req, #{}),
    case blog_posts:update_post(Post, Params) of
        {ok, _Updated} ->
            ok = blog_flash:put(Req, info, ~"Post updated."),
            {redirect, post_path(Id)};
        {error, stale} ->
            ok = blog_flash:put(
                Req, error, ~"Someone else changed this post. Reload and try again."
            ),
            {redirect, post_path(Id) ++ "/edit"};
        {error, #kura_changeset{} = CS} ->
            Vars = #{
                post => blog_view:post(Post),
                form => blog_view:form(?FIELDS, Params),
                errors => blog_view:errors(CS)
            },
            blog_view:render(Req, blog_post_edit, Vars, 422)
    end.

remove(Req, Post) ->
    {ok, _} = blog_posts:delete_post(Post),
    ok = blog_flash:put(Req, info, ~"Post deleted."),
    {redirect, "/posts"}.

with_own_post(#{bindings := #{~"id" := Id}, auth_data := Actor} = Req, Action, Next) ->
    case blog_posts:get_post(Id) of
        {ok, Post} ->
            case blog_policy:can(Actor, Action, Post) of
                true -> Next(Req, Post);
                false -> {status, 403, #{~"content-type" => ~"text/html"}, blog_view:forbidden()}
            end;
        {error, not_found} ->
            blog_error_controller:not_found(Req)
    end.

render_post(Req, Actor, Post) ->
    Vars = #{post => blog_view:post(Post), can_edit => blog_policy:can(Actor, update_post, Post)},
    blog_view:render(Req, blog_post_show, Vars).

pager(Page, Pages, Vars) ->
    Prev = [{prev_page, Page - 1} || Page > 1],
    Next = [{next_page, Page + 1} || Page < Pages],
    maps:merge(Vars, maps:from_list(Prev ++ Next)).

post_path(Id) ->
    "/posts/" ++ integer_to_list(Id).
