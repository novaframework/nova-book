-module(blog_tokens_controller).

-export([create/1, delete/1]).

create(#{json := #{~"email" := Email, ~"password" := Password}}) ->
    case blog_accounts:authenticate(Email, Password) of
        {ok, User} ->
            {ok, Token} = blog_accounts:issue_token(User),
            ValidFor = nova_auth:config(blog_auth, session_validity_days) * 86400,
            {json, 201, #{}, #{token => Token, token_type => ~"Bearer", expires_in => ValidFor}};
        {error, invalid_credentials} ->
            {json, 401, #{}, #{error => ~"invalid email or password"}}
    end;
create(_Req) ->
    {json, 400, #{}, #{error => ~"expected email and password"}}.

delete(Req) ->
    {ok, Token} = blog_security:bearer_token(Req),
    ok = blog_accounts:revoke_token(Token),
    {status, 204}.
