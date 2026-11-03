-module(blog_account_email_worker).
-behaviour(shigoto_worker).

-export([perform/1, queue/0, max_attempts/0, unique/0]).

queue() -> ~"mailers".

max_attempts() -> 5.

unique() ->
    #{
        keys => [worker, args],
        states => [available, executing, retryable, completed],
        period => 300
    }.

perform(#{~"email" := Kind, ~"user_id" := UserId}) ->
    case blog_repo:get(blog_user, UserId) of
        {ok, User} -> send(Kind, User);
        {error, not_found} -> ok
    end.

send(~"confirm", #{confirmed_at := undefined} = User) ->
    {ok, Token} = nova_auth_confirm:generate_confirm_token(blog_auth, User),
    blog_mailer:deliver_from_job(blog_emails:confirmation(User, Token));
send(~"confirm", _AlreadyConfirmed) ->
    ok;
send(~"reset", User) ->
    {ok, Token} = nova_auth_reset:generate_reset_token(blog_auth, User),
    blog_mailer:deliver_from_job(blog_emails:password_reset(User, Token)).
