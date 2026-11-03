-module(blog_prod_deploy_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    health_checks_the_database/1,
    health_is_503_without_the_database/1,
    missing_encryption_key_stops_the_boot/1
]).

all() ->
    [
        health_checks_the_database,
        health_is_503_without_the_database,
        missing_encryption_key_stops_the_boot
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

health_checks_the_database(_Config) ->
    {200, #{~"status" := ~"ok"}} = blog_data_fixtures:request(get, "/health").

health_is_503_without_the_database(_Config) ->
    ok = meck:new(blog_repo, [passthrough]),
    try
        ok = meck:expect(blog_repo, query, fun(_SQL, _Params) -> {error, closed} end),
        {503, #{~"status" := ~"unavailable"}} = blog_data_fixtures:request(get, "/health")
    after
        meck:unload(blog_repo)
    end.

missing_encryption_key_stops_the_boot(_Config) ->
    ok = application:stop(blog),
    {ok, Encryption} = application:get_env(kura, encryption),
    ok = application:set_env(kura, encryption, Encryption#{keys => [{1, ~""}]}),
    try
        {error, {blog, {bad_return, {_Start, {'EXIT', {Reason, _Stack}}}}}} =
            application:ensure_all_started(blog),
        ?assertEqual({missing_config, kura_encryption_key}, Reason)
    after
        ok = application:set_env(kura, encryption, Encryption),
        {ok, _} = application:ensure_all_started(blog)
    end.
