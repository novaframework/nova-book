-module(blog_security).

-export([current_user/1, require_user/1, require_admin/1, require_bearer/1, bearer_token/1]).

current_user(Req) ->
    case session_actor(Req) of
        {ok, Actor} -> {true, Actor};
        {error, _} -> true
    end.

require_user(Req) ->
    case session_actor(Req) of
        {ok, Actor} ->
            {true, Actor};
        {error, _} ->
            ok = blog_flash:put(Req, error, ~"Please log in first."),
            {redirect, "/login"}
    end.

require_admin(Req) ->
    case require_user(Req) of
        {true, Actor} ->
            case blog_policy:can(Actor, manage_users, #{}) of
                true -> {true, Actor};
                false -> {false, 403, #{~"content-type" => ~"text/html"}, blog_view:forbidden()}
            end;
        Redirect ->
            Redirect
    end.

require_bearer(Req) ->
    case bearer_token(Req) of
        {ok, Token} ->
            case blog_accounts:user_for_token(Token) of
                {ok, User} -> authenticated(User);
                {error, _} -> unauthorized(~"invalid or expired token")
            end;
        error ->
            unauthorized(~"missing bearer token")
    end.

bearer_token(Req) ->
    case cowboy_req:header(~"authorization", Req) of
        <<Scheme:6/binary, " ", Token/binary>> ->
            case string:lowercase(Scheme) of
                ~"bearer" -> {ok, string:trim(Token)};
                _ -> error
            end;
        _ ->
            error
    end.

session_actor(Req) ->
    case nova_session:get(Req, ~"user_token") of
        {ok, Token} ->
            case blog_accounts:user_for_token(Token) of
                {ok, User} ->
                    {true, Actor} = authenticated(User),
                    {ok, Actor};
                {error, _} = Error ->
                    {ok, _} = nova_auth_actor:delete(Req),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

authenticated(#{id := Id} = User) ->
    ok = kura_audit:set_actor(integer_to_binary(Id)),
    {true, blog_accounts:actor(User)}.

unauthorized(Message) ->
    Headers = #{~"content-type" => ~"application/json", ~"www-authenticate" => ~"Bearer"},
    {false, 401, Headers, iolist_to_binary(json:encode(#{error => Message}))}.
