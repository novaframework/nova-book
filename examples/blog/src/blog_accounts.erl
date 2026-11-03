-module(blog_accounts).

-export([
    register/1,
    authenticate/2,
    issue_token/1,
    user_for_token/1,
    revoke_token/1,
    actor/1
]).

register(Params) ->
    nova_auth_accounts:register(
        blog_auth, fun blog_user:registration_changeset/2, normalize_email(Params)
    ).

authenticate(Email, Password) when is_binary(Email), is_binary(Password) ->
    nova_auth_accounts:authenticate(blog_auth, string:casefold(string:trim(Email)), Password);
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

normalize_email(#{~"email" := Email} = Params) when is_binary(Email) ->
    Params#{~"email" := string:casefold(string:trim(Email))};
normalize_email(Params) ->
    Params.
