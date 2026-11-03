-module(blog_jobs_fixtures).

-export([reset_jobs/0, testing/1, capture_mail/0, mailer/1, receive_email/0, refute_email/0]).

reset_jobs() ->
    {ok, _} = blog_repo:query(~"DELETE FROM shigoto_jobs", []),
    ok.

testing(disabled) ->
    ok = application:unset_env(shigoto, testing),
    ok = application:unset_env(shigoto, testing_confirm_no_persistence),
    shigoto_testing:reset();
testing(Mode) when Mode =:= inline; Mode =:= manual ->
    ok = application:set_env(shigoto, testing, Mode),
    ok = application:set_env(shigoto, testing_confirm_no_persistence, true),
    shigoto_testing:reset().

capture_mail() ->
    mailer(#{adapter => hikyaku_adapter_test, pid => self()}).

mailer(Config) ->
    application:set_env(blog, mailer, Config).

receive_email() ->
    receive
        {hikyaku_email, Email} -> Email
    after 2000 -> error(no_email_delivered)
    end.

refute_email() ->
    receive
        {hikyaku_email, Email} -> error({unexpected_email, Email})
    after 200 -> ok
    end.
