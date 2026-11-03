-module(blog_purge_worker).
-behaviour(shigoto_worker).

-include_lib("kernel/include/logger.hrl").

-export([perform/1, queue/0, unique/0, schedule_next/0, next_run/1]).

-define(RETENTION_DAYS, 30).

queue() -> ~"maintenance".

unique() -> #{keys => [worker], states => [available, retryable], period => infinity}.

perform(#{~"days" := Days}) ->
    {ok, _} = schedule_next(),
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Before = calendar:gregorian_seconds_to_datetime(Now - Days * 86400),
    {ok, Purged} = blog_posts:purge_deleted(Before),
    ?LOG_INFO(#{msg => ~"purged soft-deleted posts", count => Purged}),
    {ok, #{~"purged" => Purged}}.

schedule_next() ->
    shigoto:insert(#{
        worker => ?MODULE,
        args => #{~"days" => ?RETENTION_DAYS},
        scheduled_at => next_run(calendar:universal_time())
    }).

next_run({Date, Time}) when Time < {3, 0, 0} ->
    {Date, {3, 0, 0}};
next_run({Date, _Time}) ->
    Tomorrow = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + 1),
    {Tomorrow, {3, 0, 0}}.
