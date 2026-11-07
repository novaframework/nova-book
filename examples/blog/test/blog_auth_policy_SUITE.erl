-module(blog_auth_policy_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    policy_rules/1,
    owner_policy_scopes_reads/1,
    api_author_cannot_change_others_posts/1,
    api_admin_can_change_any_post/1,
    html_author_cannot_edit_others_posts/1,
    html_owner_can_edit_and_delete/1,
    drafts_are_visible_to_owner_only/1,
    admin_pages_need_admin_role/1,
    audit_log_records_the_actor/1
]).

-define(PASSWORD, ~"correct horse battery").

all() ->
    [
        policy_rules,
        owner_policy_scopes_reads,
        api_author_cannot_change_others_posts,
        api_admin_can_change_any_post,
        html_author_cannot_edit_others_posts,
        html_owner_can_edit_and_delete,
        drafts_are_visible_to_owner_only,
        admin_pages_need_admin_role,
        audit_log_records_the_actor
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(Config).

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    blog_data_fixtures:reset_rate_limit(),
    Alice = blog_data_fixtures:user(~"alice"),
    Bob = blog_data_fixtures:user(~"bob"),
    {ok, Admin} = blog_repo:update(
        blog_user:role_changeset(blog_data_fixtures:user(~"carol"), #{role => admin})
    ),
    Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Alice writes", ~"status" => ~"published"}),
    [{alice, Alice}, {bob, Bob}, {admin, Admin}, {post, Post} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

policy_rules(Config) ->
    Alice = blog_accounts:actor(?config(alice, Config)),
    Bob = blog_accounts:actor(?config(bob, Config)),
    Admin = blog_accounts:actor(?config(admin, Config)),
    Post = ?config(post, Config),
    true = blog_policy:can(Alice, update_post, Post),
    false = blog_policy:can(Bob, update_post, Post),
    true = blog_policy:can(Admin, delete_post, Post),
    true = blog_policy:can(Bob, create_post, #{}),
    false = blog_policy:can(undefined, create_post, #{}),
    false = blog_policy:can(Alice, manage_users, #{}),
    true = blog_policy:can(Admin, manage_users, #{}),
    false = blog_policy:can(Admin, launch_rockets, #{}).

owner_policy_scopes_reads(Config) ->
    #{id := BobId} = Bob = ?config(bob, Config),
    _ = blog_data_fixtures:post(Bob, #{~"title" => ~"Bob writes"}),
    #{condition := Condition} = nova_auth_policy:allow_owner(user_id),
    Scope = Condition(#{id => BobId}, #{type => read}),
    {ok, [#{title := ~"Bob writes"}]} = blog_repo:all(Scope(kura_query:from(blog_post))).

api_author_cannot_change_others_posts(Config) ->
    #{id := Id} = ?config(post, Config),
    Path = "/api/posts/" ++ integer_to_list(Id),
    Token = blog_data_fixtures:token(?config(bob, Config)),
    Forbidden = #{~"error" => ~"you can only change your own posts"},
    {403, Forbidden} = blog_data_fixtures:request_as(Token, put, Path, #{title => ~"Bob was here"}),
    {403, Forbidden} = blog_data_fixtures:request_as(Token, delete, Path),
    {403, Forbidden} =
        blog_data_fixtures:request_as(Token, put, Path ++ "/tags", #{tags => [~"spam"]}),
    {404, _} = blog_data_fixtures:request_as(Token, delete, "/api/posts/0"),
    {200, #{~"title" := ~"Alice writes"}} = blog_data_fixtures:request(get, Path).

api_admin_can_change_any_post(Config) ->
    #{id := Id} = ?config(post, Config),
    Path = "/api/posts/" ++ integer_to_list(Id),
    Token = blog_data_fixtures:token(?config(admin, Config)),
    {200, #{~"title" := ~"Moderated"}} =
        blog_data_fixtures:request_as(Token, put, Path, #{title => ~"Moderated"}),
    {204, <<>>} = blog_data_fixtures:request_as(Token, delete, Path).

html_author_cannot_edit_others_posts(Config) ->
    #{id := Id} = ?config(post, Config),
    Path = "/posts/" ++ integer_to_list(Id),
    Bob = blog_data_fixtures:log_in(Config, ~"bob@example.com", ?PASSWORD),
    {ok, Show} = nova_test:get(Path, Bob),
    ?assertEqual(nomatch, binary:match(nova_test:body(Show), ~"/edit\"")),
    {ok, Edit} = nova_test:get(Path ++ "/edit", Bob),
    ?assertStatus(403, Edit),
    ?assertHeader("content-type", "text/html", Edit),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(Edit), ~"<h1>Forbidden</h1>")),
    Token = nova_test:csrf_token(Show),
    {ok, Delete} = nova_test:post(Path ++ "/delete", #{form => [{~"_csrf_token", Token}]}, Bob),
    ?assertStatus(403, Delete),
    Update = #{form => [{~"_csrf_token", Token}, {~"title", ~"Mine now"}]},
    {ok, Updated} = nova_test:post(Path, Update, Bob),
    ?assertStatus(403, Updated),
    {ok, #{title := ~"Alice writes", deleted_at := undefined}} = blog_repo:get(blog_post, Id).

html_owner_can_edit_and_delete(Config) ->
    #{id := Id} = ?config(post, Config),
    Path = "/posts/" ++ integer_to_list(Id),
    Alice = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, Show} = nova_test:get(Path, Alice),
    ?assertNotEqual(
        nomatch,
        binary:match(nova_test:body(Show), <<"href=\"", (list_to_binary(Path))/binary, "/edit\"">>)
    ),
    {ok, #{status := 200}} = nova_test:get(Path ++ "/edit", Alice),
    {ok, #{status := 302}} = nova_test:post(
        Path ++ "/delete", #{form => [{~"_csrf_token", nova_test:csrf_token(Show)}]}, Alice
    ),
    {ok, #{status := 404}} = nova_test:get(Path, Alice).

drafts_are_visible_to_owner_only(Config) ->
    #{id := Id} = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Secret draft"}),
    Path = "/posts/" ++ integer_to_list(Id),
    LogIn = fun(Email) -> blog_data_fixtures:log_in(Config, Email, ?PASSWORD) end,
    {ok, #{status := 404}} = nova_test:get(Path, Config),
    {ok, #{status := 404}} = nova_test:get(Path, LogIn(~"bob@example.com")),
    {ok, #{status := 200, body := Body}} = nova_test:get(Path, LogIn(~"alice@example.com")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<span class=\"badge\">draft</span>")),
    {ok, #{status := 200}} = nova_test:get(Path, LogIn(~"carol@example.com")).

admin_pages_need_admin_role(Config) ->
    {ok, Anonymous} = nova_test:get("/admin/users", Config),
    ?assertStatus(302, Anonymous),
    ?assertHeader("location", "/login", Anonymous),
    Alice = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, #{status := 403}} = nova_test:get("/admin/users", Alice),
    Carol = blog_data_fixtures:log_in(Config, ~"carol@example.com", ?PASSWORD),
    {ok, #{status := 200, body := Body}} = nova_test:get("/admin/users", Carol),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<td>bob@example.com</td>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"href=\"/admin/users\"")).

audit_log_records_the_actor(Config) ->
    #{id := Id} = ?config(post, Config),
    #{id := AliceId} = ?config(alice, Config),
    Token = blog_data_fixtures:token(?config(alice, Config)),
    {200, _} = blog_data_fixtures:request_as(
        Token, put, "/api/posts/" ++ integer_to_list(Id), #{title => ~"Edited"}
    ),
    Query = kura_query:where(kura_query:from(kura_audit_log), {action, ~"update"}),
    {ok, [#{actor := Actor}]} = blog_repo:all(Query),
    ?assertEqual(integer_to_binary(AliceId), Actor).
