-module(blog_prod_config_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    every_config_parses/1,
    every_config_declares_the_repo/1,
    prod_secrets_come_from_the_environment/1,
    prod_numbers_have_defaults/1,
    prod_is_hardened/1,
    prod_serves_the_tested_live_routes/1,
    prod_cookie_comes_from_the_environment/1,
    unquoted_variable_without_default_is_rejected/1
]).

all() ->
    [
        every_config_parses,
        every_config_declares_the_repo,
        prod_secrets_come_from_the_environment,
        prod_numbers_have_defaults,
        prod_is_hardened,
        prod_serves_the_tested_live_routes,
        prod_cookie_comes_from_the_environment,
        unquoted_variable_without_default_is_rejected
    ].

every_config_parses(_Config) ->
    Files = config_files(),
    ?assertEqual(
        ["dev_sys.config.src", "prod_sys.config.src", "test_sys.config"],
        [filename:basename(File) || File <- Files]
    ),
    [{ok, [_]} = consult(File) || File <- Files].

every_config_declares_the_repo(_Config) ->
    [declares_repo(repo(File)) || File <- config_files()].

declares_repo(#{backend := kura_backend_postgres, host := _, database := _, port := Port}) when
    is_integer(Port)
->
    ok.

prod_secrets_come_from_the_environment(_Config) ->
    File = prod(),
    #{host := "@DB_HOST@", user := "@DB_USER@", password := "@DB_PASSWORD@"} = repo(File),
    [{1, ~"@KURA_ENCRYPTION_KEY@"}] = maps:get(keys, env(File, kura, encryption)),
    ~"@BLOG_OPS_TOKEN@" = env(File, blog, ops_token).

prod_numbers_have_defaults(_Config) ->
    File = prod(),
    #{port := 5432} = repo(File),
    #{port := 8080} = env(File, nova, cowboy_configuration),
    9464 = env(File, blog, metrics_port).

prod_is_hardened(_Config) ->
    File = prod(),
    prod = env(File, nova, environment),
    false = env(File, nova, use_stacktrace),
    #{stream_handlers := Handlers} = env(File, nova, cowboy_configuration),
    [nova_stream_h, blog_session_h] = [
        H
     || H <- Handlers, lists:member(H, [nova_stream_h, blog_session_h])
    ],
    [] = [Route || {mcp, _, _, _} = Route <- env(File, arizona_nova, routes)].

prod_serves_the_tested_live_routes(_Config) ->
    Tested = [
        Route
     || Route <- env(test_config(), arizona_nova, routes), element(1, Route) =/= mcp
    ],
    ?assertEqual(Tested, env(prod(), arizona_nova, routes)).

prod_cookie_comes_from_the_environment(_Config) ->
    {ok, VmArgs} = file:read_file(filename:join(config_dir(), "prod_vm.args.src")),
    ?assertMatch({match, _}, re:run(VmArgs, ~"(?m)^-setcookie \\$\\{RELEASE_COOKIE\\}$")).

unquoted_variable_without_default_is_rejected(_Config) ->
    ?assertMatch({error, _}, parse(substitute(~"[{blog, [{port, ${PORT}}]}]."))),
    {ok, [[{blog, [{port, 80}]}]]} = parse(substitute(~"[{blog, [{port, ${PORT:-80}}]}].")).

config_files() ->
    lists:sort(filelib:wildcard(filename:join(config_dir(), "*sys.config*"))).

config_dir() ->
    filename:join([code:lib_dir(blog), "src", "..", "config"]).

prod() ->
    filename:join(config_dir(), "prod_sys.config.src").

test_config() ->
    filename:join(config_dir(), "test_sys.config").

repo(File) ->
    maps:get(blog_repo, env(File, kura, repos)).

env(File, App, Key) ->
    {ok, [Terms]} = consult(File),
    {App, AppEnv} = lists:keyfind(App, 1, Terms),
    {Key, Value} = lists:keyfind(Key, 1, AppEnv),
    Value.

consult(File) ->
    {ok, Text} = file:read_file(File),
    parse(substitute(Text)).

substitute(Text) ->
    re:replace(Text, ~"\\$\\{([A-Z_]+)(:-)?([^}]*)\\}", fun replacement/2, [
        global, {return, binary}
    ]).

replacement(_Match, [Name, ~"", _]) -> <<"@", Name/binary, "@">>;
replacement(_Match, [_Name, ~":-", Default]) -> Default.

parse(Text) ->
    case erl_scan:string(unicode:characters_to_list(Text)) of
        {ok, Tokens, _End} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> {ok, [Term]};
                {error, _} = Error -> Error
            end;
        {error, _, _} = Error ->
            {error, Error}
    end.
