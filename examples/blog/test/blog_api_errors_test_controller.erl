-module(blog_api_errors_test_controller).
-fallback_controller(blog_api_fallback_controller).

-export([crash/1, bare_status/1, unexpected/1, echo/1, crashing_security/1, refusing_security/1]).

crash(#{bindings := #{~"id" := Id}}) ->
    {json, #{id => binary_to_integer(Id)}}.

bare_status(_Req) ->
    {status, 404}.

unexpected(_Req) ->
    {error, teapot}.

echo(#{json := Json}) ->
    {json, Json}.

crashing_security(_Req) ->
    error(security_failed).

refusing_security(_Req) ->
    false.
