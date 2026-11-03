-module(blog_api_docs_controller).

-export([openapi/1]).

openapi(_Req) ->
    {ok, Spec} = file:read_file(filename:join(code:priv_dir(blog), "openapi/openapi.json")),
    #{~"paths" := Paths} = Decoded = json:decode(Spec),
    {json, Decoded#{~"paths" := #{P => Ops || P := Ops <- Paths, is_api_path(P)}}}.

is_api_path(<<"/api/", _/binary>>) -> true;
is_api_path(_) -> false.
