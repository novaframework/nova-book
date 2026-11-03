-module(blog_data_safety_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kura/include/kura.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    soft_delete_hides_and_restore_returns/1,
    optimistic_lock_detects_stale_writes/1,
    api_returns_409_on_stale_update/1,
    audit_log_records_changes/1,
    before_delete_protects_last_admin/1,
    encrypted_field_is_ciphertext_at_rest/1,
    encrypted_field_cannot_be_filtered/1
]).

all() ->
    [
        soft_delete_hides_and_restore_returns,
        optimistic_lock_detects_stale_writes,
        api_returns_409_on_stale_update,
        audit_log_records_changes,
        before_delete_protects_last_admin,
        encrypted_field_is_ciphertext_at_rest,
        encrypted_field_cannot_be_filtered
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    kura_audit:clear_actor(),
    blog_data_fixtures:checkin().

soft_delete_hides_and_restore_returns(Config) ->
    #{id := Id} = Post = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Oops"}),
    {ok, #{deleted_at := {{_, _, _}, _}}} = blog_posts:delete_post(Post),
    {error, not_found} = blog_repo:get(blog_post, Id),
    {ok, []} = blog_repo:all(kura_query:from(blog_post)),
    {ok, 1} = blog_repo:count(kura_query:from(blog_post)),
    {ok, 0} = blog_repo:count(kura_query:where(kura_query:from(blog_post), {deleted_at, is_nil})),
    {ok, [#{id := Id}]} = blog_repo:all(kura_query:only_deleted(kura_query:from(blog_post))),
    {ok, [#{rows := 1}]} = blog_repo:query(~"SELECT count(*) AS rows FROM posts", []),
    {ok, #{deleted_at := undefined}} = blog_posts:restore_post(Id),
    {ok, #{id := Id}} = blog_repo:get(blog_post, Id),
    {error, not_found} = blog_posts:restore_post(Id).

optimistic_lock_detects_stale_writes(Config) ->
    Post = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Shared draft"}),
    #{lock_version := 1} = Post,
    {ok, #{lock_version := 2}} = blog_posts:update_post(Post, #{~"title" => ~"Edited by Alice"}),
    {error, stale} = blog_posts:update_post(Post, #{~"title" => ~"Edited by Bob"}),
    {ok, #{title := ~"Edited by Alice"}} = blog_repo:get(blog_post, maps:get(id, Post)).

api_returns_409_on_stale_update(Config) ->
    Alice = ?config(alice, Config),
    Token = blog_data_fixtures:token(Alice),
    #{id := Id} = blog_data_fixtures:post(Alice, #{~"title" => ~"Shared draft"}),
    Path = "/api/posts/" ++ integer_to_list(Id),
    {200, #{~"lock_version" := 2}} =
        blog_data_fixtures:request_as(Token, put, Path, #{title => ~"First edit", lock_version => 1}),
    {409, #{~"error" := ~"post was changed by someone else"}} =
        blog_data_fixtures:request_as(Token, put, Path, #{
            title => ~"Second edit", lock_version => 1
        }),
    {200, #{~"lock_version" := 3}} =
        blog_data_fixtures:request_as(Token, put, Path, #{title => ~"Retry"}).

audit_log_records_changes(Config) ->
    kura_audit:set_actor(~"alice"),
    #{id := Id} = Post = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Audited"}),
    {ok, Updated} = blog_posts:update_post(Post, #{~"title" => ~"Audited, edited"}),
    {ok, _} = blog_posts:delete_post(Updated),
    Q = kura_query:where(kura_query:from(kura_audit_log), {record_id, integer_to_binary(Id)}),
    {ok, Entries} = blog_repo:all(kura_query:order_by(Q, [{id, asc}])),
    [~"insert", ~"update", ~"update"] = [maps:get(action, E) || E <- Entries],
    [#{actor := ~"alice"} | _] = Entries,
    #{changes := #{~"title" := #{~"old" := ~"Audited", ~"new" := ~"Audited, edited"}}} =
        lists:nth(2, Entries).

before_delete_protects_last_admin(Config) ->
    Alice = ?config(alice, Config),
    {ok, Admin} = blog_repo:update(blog_user:role_changeset(Alice, #{role => ~"admin"})),
    {error, last_admin} = blog_repo:delete(kura_changeset:cast(blog_user, Admin, #{}, [])),
    Bob = blog_data_fixtures:user(~"bob"),
    {ok, _} = blog_repo:update(blog_user:role_changeset(Bob, #{role => ~"admin"})),
    {ok, _} = blog_repo:delete(kura_changeset:cast(blog_user, Admin, #{}, [])).

encrypted_field_is_ciphertext_at_rest(Config) ->
    Alice = ?config(alice, Config),
    {ok, #{full_name := ~"Alice Liddell"}} =
        blog_repo:update(blog_user:profile_changeset(Alice, #{~"full_name" => ~"Alice Liddell"})),
    {ok, #{full_name := ~"Alice Liddell"}} = blog_repo:get(blog_user, maps:get(id, Alice)),
    {ok, [#{full_name := Stored}]} = blog_repo:query(~"SELECT full_name FROM users", []),
    true = is_binary(Stored),
    nomatch = binary:match(Stored, ~"Alice"),
    {ok, [#{bytes := 1}]} = blog_repo:query(
        ~"SELECT get_byte(full_name, 0) AS bytes FROM users", []
    ).

encrypted_field_cannot_be_filtered(_Config) ->
    try kura_query:where(kura_query:from(blog_user), {full_name, ~"Alice Liddell"}) of
        _ -> ct:fail(encrypted_filter_allowed)
    catch
        error:{kura, {encrypted_field_in_where, full_name}} -> ok
    end.
