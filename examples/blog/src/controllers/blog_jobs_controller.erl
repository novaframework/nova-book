-module(blog_jobs_controller).

-export([health/1]).

health(_Req) ->
    case shigoto:health() of
        {ok, #{status := Status, counts := Counts, queues := Queues}} ->
            {json, #{status => Status, counts => Counts, queues => Queues}};
        {error, #{status := Status}} ->
            {json, 503, #{}, #{status => Status}}
    end.
