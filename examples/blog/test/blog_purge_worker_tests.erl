-module(blog_purge_worker_tests).
-include_lib("eunit/include/eunit.hrl").

cutoff_is_days_before_now_test_() ->
    [
        ?_assertEqual(
            {{2026, 10, 4}, {3, 0, 0}}, blog_purge_worker:cutoff({{2026, 11, 3}, {3, 0, 0}}, 30)
        ),
        ?_assertEqual(
            {{2026, 12, 16}, {3, 0, 0}}, blog_purge_worker:cutoff({{2027, 1, 15}, {3, 0, 0}}, 30)
        ),
        ?_assertEqual(
            {{2028, 2, 29}, {3, 0, 0}}, blog_purge_worker:cutoff({{2028, 3, 1}, {3, 0, 0}}, 1)
        )
    ].
