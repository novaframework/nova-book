-module(blog_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = require_encryption_key(),
    {ok, _} = kura_migrator:migrate(blog_repo),
    ok = migrate_jobs(),
    ok = blog_jobs:schedule_maintenance(),
    ok = nova_handlers:register_handler(text, fun blog_text_handler:handle/3),
    ok = opentelemetry_nova:setup(),
    ok = opentelemetry_kura:setup(),
    blog_sup:start_link().

stop(_State) ->
    ok.

require_encryption_key() ->
    case kura_keyring_env:active() of
        {_KeyId, _Key} -> ok;
        undefined -> error({missing_config, kura_encryption_key})
    end.

migrate_jobs() ->
    ok = shigoto_migration:up(blog_repo),
    case whereis(shigoto_migrator) of
        undefined ->
            ok;
        Migrator ->
            Ref = monitor(process, Migrator),
            _ = shigoto_migrator:retry_now(),
            receive
                {'DOWN', Ref, process, Migrator, _Reason} -> ok
            after 10000 ->
                error(shigoto_queues_not_started)
            end
    end.
