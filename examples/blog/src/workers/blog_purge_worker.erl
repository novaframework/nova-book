-module(blog_purge_worker).
-behaviour(shigoto_worker).

-include_lib("kernel/include/logger.hrl").

-export([perform/1, queue/0, cutoff/2]).

queue() -> ~"maintenance".

perform(#{~"days" := Days}) ->
    {ok, Purged} = blog_posts:purge_deleted(cutoff(calendar:universal_time(), Days)),
    ?LOG_INFO(#{msg => ~"purged soft-deleted posts", count => Purged}),
    {ok, #{~"purged" => Purged}}.

cutoff(Now, Days) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) - Days * 86400
    ).
