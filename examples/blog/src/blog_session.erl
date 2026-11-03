-module(blog_session).

-export([log_in/2, log_out/1, new_id/0, put_cookie/2]).

log_in(Req, User) ->
    {ok, Token} = blog_accounts:issue_token(User),
    Req1 = renew(Req),
    ok = nova_session:set(Req1, ~"user_token", Token),
    ok = nova_auth_actor:store(Req1, blog_accounts:actor(User)),
    Req1.

log_out(Req) ->
    case nova_session:get(Req, ~"user_token") of
        {ok, Token} -> ok = blog_accounts:revoke_token(Token);
        {error, _} -> ok
    end,
    renew(Req).

renew(Req) ->
    {ok, Req1} = nova_session:delete(Req),
    SessionId = new_id(),
    put_cookie(SessionId, Req1#{nova_session_id => SessionId}).

new_id() ->
    base64:encode(crypto:strong_rand_bytes(32), #{mode => urlsafe, padding => false}).

put_cookie(SessionId, Req) ->
    cowboy_req:set_resp_cookie(~"session_id", SessionId, Req, cookie_options()).

cookie_options() ->
    #{
        path => ~"/",
        http_only => true,
        secure => application:get_env(blog, secure_cookies, true),
        same_site => lax
    }.
