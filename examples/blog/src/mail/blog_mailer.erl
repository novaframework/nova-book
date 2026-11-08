-module(blog_mailer).
-behaviour(hikyaku_mailer).

-export([config/0, deliver/1, deliver_from_job/1]).

config() ->
    application:get_env(blog, mailer, #{adapter => hikyaku_adapter_logger}).

deliver(Email) ->
    hikyaku_mailer:deliver(?MODULE, Email).

deliver_from_job(Email) ->
    case deliver(Email) of
        {ok, _Receipt} -> ok;
        {error, #{status_code := 429}} -> {snooze, 60};
        {error, Reason} -> {error, Reason}
    end.
