-module(m20261103073500_create_audit_log).
-moduledoc false.
-behaviour(kura_migration).
-export([up/0, down/0]).

-spec up() -> [kura_migration:operation()].
up() -> kura_audit:migration_up().

-spec down() -> [kura_migration:operation()].
down() -> kura_audit:migration_down().
