-module(blog_api_fallback_controller).
-include_lib("kernel/include/logger.hrl").
-include_lib("kura/include/kura.hrl").

-export([resolve/2]).

resolve(_Req, {error, not_found}) ->
    {json, 404, #{}, #{error => ~"not_found"}};
resolve(_Req, {error, #kura_changeset{} = CS}) ->
    {json, 422, #{}, #{errors => blog_api_json:errors(CS)}};
resolve(_Req, Unexpected) ->
    ?LOG_ERROR(#{msg => ~"unexpected controller return", return => Unexpected}),
    {json, 500, #{}, #{error => ~"internal_error"}}.
