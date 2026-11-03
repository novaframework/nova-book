-module(blog_registration_controller).

-export([new/1, create/1]).

-define(FIELDS, [email, username, full_name]).

new(Req) ->
    blog_view:render(Req, blog_register, #{form => blog_view:form(?FIELDS, #{}), errors => #{}}).

create(Req) ->
    Params = maps:get(params, Req, #{}),
    case blog_accounts:register(Params) of
        {ok, User} ->
            Req1 = blog_session:log_in(Req, User),
            ok = blog_flash:put(Req1, info, ~"Welcome to the blog!"),
            {redirect, "/posts", Req1};
        {error, CS} ->
            Vars = #{form => blog_view:form(?FIELDS, Params), errors => blog_view:errors(CS)},
            blog_view:render(Req, blog_register, Vars, 422)
    end.
