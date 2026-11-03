-module(blog_live_middleware).
-export([path_bindings/2, current_user/2, require_user/2]).

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

session_user(Req) ->
    case nova_auth_actor:fetch(arizona_req:raw(Req)) of
        {ok, User} -> User;
        {error, not_found} -> undefined
    end.
