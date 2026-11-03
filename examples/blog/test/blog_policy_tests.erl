-module(blog_policy_tests).
-include_lib("eunit/include/eunit.hrl").

-define(ALICE, #{id => 1, role => author}).
-define(BOB, #{id => 2, role => author}).
-define(ADMIN, #{id => 3, role => admin}).
-define(ALICES_POST, #{id => 10, user_id => 1}).

anonymous_readers_can_do_nothing_test_() ->
    [
        ?_assertNot(blog_policy:can(undefined, Action, ?ALICES_POST))
     || Action <- [create_post, update_post, delete_post, view_draft, manage_users]
    ].

any_author_can_create_test() ->
    ?assert(blog_policy:can(?BOB, create_post, #{})).

only_the_owner_or_an_admin_can_edit_test_() ->
    [
        ?_assert(blog_policy:can(?ALICE, update_post, ?ALICES_POST)),
        ?_assertNot(blog_policy:can(?BOB, update_post, ?ALICES_POST)),
        ?_assert(blog_policy:can(?ADMIN, delete_post, ?ALICES_POST)),
        ?_assertNot(blog_policy:can(?BOB, view_draft, ?ALICES_POST))
    ].

only_admins_manage_users_test_() ->
    [
        ?_assert(blog_policy:can(?ADMIN, manage_users, #{})),
        ?_assertNot(blog_policy:can(?ALICE, manage_users, #{}))
    ].

unknown_actions_are_denied_test() ->
    ?assertNot(blog_policy:can(?ADMIN, launch_rockets, #{})).
