-module(blog_purge_worker_tests).
-include_lib("eunit/include/eunit.hrl").

next_run_is_the_next_3am_test_() ->
    [
        ?_assertEqual(
            {{2026, 11, 3}, {3, 0, 0}}, blog_purge_worker:next_run({{2026, 11, 3}, {1, 15, 0}})
        ),
        ?_assertEqual(
            {{2026, 11, 4}, {3, 0, 0}}, blog_purge_worker:next_run({{2026, 11, 3}, {3, 0, 0}})
        ),
        ?_assertEqual(
            {{2027, 1, 1}, {3, 0, 0}}, blog_purge_worker:next_run({{2026, 12, 31}, {23, 59, 59}})
        )
    ].
