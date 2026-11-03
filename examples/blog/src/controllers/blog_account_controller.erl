-module(blog_account_controller).
-include_lib("kura/include/kura.hrl").

-export([confirm_form/1, confirm/1, forgot_form/1, send_reset/1, reset_form/1, reset/1]).

-define(BAD_LINK, ~"That link is invalid or has expired.").

confirm_form(Req) ->
    no_referrer(blog_view:render(Req, blog_confirm, #{token => query_token(Req)})).

confirm(Req) ->
    case blog_accounts:confirm(param(~"token", Req)) of
        {ok, _User} ->
            ok = blog_flash:put(Req, info, ~"Thank you, your email address is confirmed.");
        {error, _} ->
            ok = blog_flash:put(Req, error, ?BAD_LINK)
    end,
    {redirect, "/posts"}.

forgot_form(Req) ->
    blog_view:render(Req, blog_password_forgot, #{form => blog_view:form([email], #{})}).

send_reset(Req) ->
    ok = blog_accounts:request_password_reset(param(~"email", Req)),
    ok = blog_flash:put(Req, info, ~"If that address has an account, a reset link is on its way."),
    {redirect, "/login"}.

reset_form(Req) ->
    Vars = #{token => query_token(Req), errors => #{}},
    no_referrer(blog_view:render(Req, blog_password_reset, Vars)).

reset(Req) ->
    Token = param(~"token", Req),
    case blog_accounts:reset_password(Token, maps:get(params, Req, #{})) of
        {ok, _User} ->
            ok = blog_flash:put(Req, info, ~"Your password has been changed. Please log in again."),
            {redirect, "/login"};
        {error, #kura_changeset{} = CS} ->
            Vars = #{token => Token, errors => blog_view:errors(CS)},
            no_referrer(blog_view:render(Req, blog_password_reset, Vars, 422));
        {error, _} ->
            ok = blog_flash:put(Req, error, ?BAD_LINK),
            {redirect, "/password/forgot"}
    end.

query_token(Req) ->
    proplists:get_value(~"token", cowboy_req:parse_qs(Req), ~"").

param(Name, Req) ->
    maps:get(Name, maps:get(params, Req, #{}), undefined).

no_referrer({ok, Page, Opts}) ->
    Headers = #{~"content-type" => ~"text/html", ~"referrer-policy" => ~"no-referrer"},
    {ok, Page, Opts#{headers => Headers}}.
