-module(blog_session_controller).

-export([new/1, create/1, delete/1]).

new(Req) ->
    blog_view:render(Req, blog_login, #{form => blog_view:form([email], #{})}).

create(Req) ->
    Params = maps:get(params, Req, #{}),
    Email = maps:get(~"email", Params, undefined),
    Password = maps:get(~"password", Params, undefined),
    case blog_accounts:authenticate(Email, Password) of
        {ok, User} ->
            Req1 = blog_session:log_in(Req, User),
            ok = blog_flash:put(Req1, info, ~"Welcome back!"),
            {redirect, "/posts", Req1};
        {error, invalid_credentials} ->
            Vars = #{
                form => blog_view:form([email], Params),
                error => ~"Invalid email or password."
            },
            blog_view:render(Req, blog_login, Vars, 422)
    end.

delete(Req) ->
    Req1 = blog_session:log_out(Req),
    ok = blog_flash:put(Req1, info, ~"You have been logged out."),
    {redirect, "/posts", Req1}.
