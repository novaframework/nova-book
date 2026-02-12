-module(my_first_nova_main_controller).
-export([
         index/1,
         login/1,
         login_post/1,
         logout/1
        ]).

index(#{auth_data := #{authed := true, username := Username}}) ->
    {ok, [{message, <<"Hello ", Username/binary>>}]};
index(_Req) ->
    {redirect, "/login"}.

login(_Req) ->
    {ok, [], #{view => login}}.

login_post(#{auth_data := #{authed := true, username := Username}} = Req) ->
    {ok, SessionId} = nova_session:generate_session_id(),
    Req1 = cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req,
                                       #{path => <<"/">>, http_only => true}),
    nova_session_ets:set_value(SessionId, <<"username">>, Username),
    {redirect, "/"};
login_post(_Req) ->
    {ok, [{error, <<"Invalid username or password">>}], #{view => login}}.

logout(Req) ->
    {ok, _Req1} = nova_session:delete(Req),
    {redirect, "/login"}.
