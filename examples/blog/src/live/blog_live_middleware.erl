-module(blog_live_middleware).
-export([path_bindings/2, current_user/2, require_user/2, published_post/2, own_post/2]).

path_bindings(Req, Bindings) ->
    Extract = arizona_middleware:extract([path_bindings]),
    Extract(Req, Bindings).

current_user(Req, Bindings) ->
    {cont, Req, Bindings#{current_user => session_user(Req)}}.

require_user(Req, Bindings) ->
    case session_user(Req) of
        undefined ->
            {halt, arizona_req:redirect(Req, ~"/login")};
        User ->
            {cont, Req, Bindings#{current_user => User}}
    end.

published_post(Req, #{~"id" := Id} = Bindings) ->
    case blog_posts:get_post(Id, [author]) of
        {ok, #{status := published} = Post} -> {cont, Req, Bindings#{post => Post}};
        _ -> not_found(Req)
    end.

own_post(Req, #{~"id" := Id, current_user := #{id := UserId}} = Bindings) ->
    case blog_posts:get_post(Id, [tags]) of
        {ok, #{user_id := UserId} = Post} -> {cont, Req, Bindings#{post => Post}};
        _ -> not_found(Req)
    end.

not_found(Req) ->
    {halt, arizona_req:put_resp_status(Req, 404)}.

session_user(Req) ->
    case blog_security:session_actor(arizona_req:raw(Req)) of
        {ok, Actor} -> Actor;
        {error, _} -> undefined
    end.
