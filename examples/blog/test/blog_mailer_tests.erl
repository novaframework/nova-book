-module(blog_mailer_tests).
-include_lib("eunit/include/eunit.hrl").

deliver_from_job_test_() ->
    {foreach, fun save_mailer/0, fun restore_mailer/1, [
        fun delivered_mail_is_ok/0,
        fun rate_limited_provider_snoozes/0,
        fun other_failures_are_errors/0
    ]}.

save_mailer() ->
    application:get_env(blog, mailer).

restore_mailer(undefined) ->
    application:unset_env(blog, mailer);
restore_mailer({ok, Config}) ->
    application:set_env(blog, mailer, Config).

delivered_mail_is_ok() ->
    ok = application:set_env(blog, mailer, #{adapter => hikyaku_adapter_test, pid => self()}),
    ?assertEqual(ok, blog_mailer:deliver_from_job(email())),
    receive
        {hikyaku_email, Email} -> ?assertEqual(email(), Email)
    after 1000 -> ?assert(false)
    end.

rate_limited_provider_snoozes() ->
    reply_with({error, #{status_code => 429}}),
    ?assertEqual({snooze, 60}, blog_mailer:deliver_from_job(email())).

other_failures_are_errors() ->
    reply_with({error, smtp_down}),
    ?assertEqual({error, smtp_down}, blog_mailer:deliver_from_job(email())).

reply_with(Reply) ->
    ok = application:set_env(blog, mailer, #{adapter => blog_test_mail_adapter, reply => Reply}).

email() ->
    E0 = hikyaku_email:from(hikyaku_email:new(), ~"noreply@blog.test"),
    hikyaku_email:subject(hikyaku_email:to(E0, ~"alice@example.com"), ~"Hi").
