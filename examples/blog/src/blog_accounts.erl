-module(blog_accounts).

-export([
    register/1,
    confirm/1,
    request_password_reset/1,
    reset_password/2,
    authenticate/2,
    issue_token/1,
    user_for_token/1,
    revoke_token/1,
    actor/1
]).

register(Params) ->
    case nova_auth_accounts:register(blog_auth, fun blog_user:registration_changeset/2, Params) of
        {ok, User} ->
            ok = blog_jobs:account_email(confirm, User),
            {ok, User};
        {error, _} = Error ->
            Error
    end.

confirm(Token) when is_binary(Token) ->
    nova_auth_confirm:confirm_user(blog_auth, Token);
confirm(_Token) ->
    {error, not_found}.

request_password_reset(Email) when is_binary(Email) ->
    maybe
        {ok, Normalized} ?= blog_user:normalize_email(Email),
        {ok, User} ?= blog_repo:get_by(blog_user, [{email, Normalized}]),
        blog_jobs:account_email(reset, User)
    else
        {error, invalid_encoding} -> ok;
        {error, not_found} -> ok
    end;
request_password_reset(_Email) ->
    ok.

reset_password(Token, Params) when is_binary(Token) ->
    nova_auth_reset:reset_password(blog_auth, Token, Params);
reset_password(_Token, _Params) ->
    {error, not_found}.

authenticate(Email, Password) when is_binary(Email), is_binary(Password) ->
    case blog_user:normalize_email(Email) of
        {ok, Normalized} -> nova_auth_accounts:authenticate(blog_auth, Normalized, Password);
        {error, invalid_encoding} -> {error, invalid_credentials}
    end;
authenticate(_Email, _Password) ->
    {error, invalid_credentials}.

issue_token(User) ->
    nova_auth_session:generate_session_token(blog_auth, User).

user_for_token(Token) when is_binary(Token) ->
    nova_auth_session:get_user_by_session_token(blog_auth, Token);
user_for_token(_Token) ->
    {error, not_found}.

revoke_token(Token) ->
    nova_auth_session:delete_session_token(blog_auth, Token).

actor(#{id := Id, username := Username, role := Role}) ->
    #{id => Id, provider => password, username => Username, role => Role}.
