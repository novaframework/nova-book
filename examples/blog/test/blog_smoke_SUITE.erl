-module(blog_smoke_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([heartbeat_returns_200/1, repo_runs_a_query/1]).

all() -> [heartbeat_returns_200, repo_runs_a_query].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(blog),
    {ok, _} = application:ensure_all_started(inets),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

heartbeat_returns_200(_Config) ->
    {ok, {{_, 200, _}, _, _}} = httpc:request(get, {url("/heartbeat"), []}, [], []).

repo_runs_a_query(_Config) ->
    {ok, [#{one := 1}]} = blog_repo:query(~"SELECT 1 AS one", []).

url(Path) ->
    {ok, #{port := Port}} = application:get_env(nova, cowboy_configuration),
    "http://localhost:" ++ integer_to_list(Port) ++ Path.
