-module(blog_fallback_controller).

-include_lib("kernel/include/logger.hrl").

-export([resolve/2]).

resolve(_Req, {error, invalid_name}) ->
    {json, 422, #{}, #{error => ~"invalid_name"}};
resolve(_Req, {error, missing_name}) ->
    {json, 400, #{}, #{error => ~"missing_name"}};
resolve(_Req, {error, not_found}) ->
    {status, 404, #{}, #{error => ~"not_found"}};
resolve(_Req, Unexpected) ->
    ?LOG_ERROR(#{msg => ~"unexpected controller return", return => Unexpected}),
    {status, 500}.
