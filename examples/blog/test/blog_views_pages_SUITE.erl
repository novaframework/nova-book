-module(blog_views_pages_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    index_lists_published_posts/1,
    index_paginates/1,
    index_rejects_bad_page/1,
    post_page_escapes_and_formats/1,
    unknown_post_is_404/1,
    stylesheet_is_self_hosted/1,
    new_post_requires_login/1,
    create_post_via_form/1,
    create_post_without_csrf_token_is_403/1,
    invalid_post_rerenders_form_with_errors/1,
    edit_form_is_prefilled/1,
    update_post_via_form/1,
    stale_edit_is_rejected/1
]).

-define(PASSWORD, "correct horse battery").

all() ->
    [
        index_lists_published_posts,
        index_paginates,
        index_rejects_bad_page,
        post_page_escapes_and_formats,
        unknown_post_is_404,
        stylesheet_is_self_hosted,
        new_post_requires_login,
        create_post_via_form,
        create_post_without_csrf_token_is_403,
        invalid_post_rerenders_form_with_errors,
        edit_form_is_prefilled,
        update_post_via_form,
        stale_edit_is_rejected
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    blog_web:reset_rate_limit(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

index_lists_published_posts(Config) ->
    Alice = ?config(alice, Config),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Hello Nova", ~"status" => ~"published"}),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Unfinished"}),
    {200, Headers, Body, _} = blog_web:get(#{}, "/posts"),
    "text/html" = blog_web:header("content-type", Headers),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<title>Posts - Nova Blog</title>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"href=\"/login\"")),
    ?assertNotEqual(nomatch, binary:match(Body, ~">Hello Nova</a>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"by alice on ")),
    ?assertEqual(nomatch, binary:match(Body, ~"Unfinished")),
    assert_no_erlang_terms(Body).

index_paginates(Config) ->
    Alice = ?config(alice, Config),
    [
        blog_data_fixtures:post(Alice, #{
            ~"title" => <<"Post ", (integer_to_binary(N))/binary>>, ~"status" => ~"published"
        })
     || N <- lists:seq(1, 12)
    ],
    {200, _, Page1, _} = blog_web:get(#{}, "/posts"),
    ?assertNotEqual(nomatch, binary:match(Page1, ~"href=\"/posts?page=2\">Older")),
    ?assertEqual(nomatch, binary:match(Page1, ~"Newer")),
    {200, _, Page2, _} = blog_web:get(#{}, "/posts?page=2"),
    ?assertNotEqual(nomatch, binary:match(Page2, ~"href=\"/posts?page=1\">Newer")),
    ?assertNotEqual(nomatch, binary:match(Page2, ~">Post 1</a>")).

index_rejects_bad_page(_Config) ->
    {400, _, _, _} = blog_web:get(#{}, "/posts?page=abc").

post_page_escapes_and_formats(Config) ->
    #{id := Id} = blog_data_fixtures:post(?config(alice, Config), #{
        ~"title" => ~"<script>alert(1)</script>",
        ~"body" => ~"First paragraph.\n\nSecond & last.",
        ~"status" => ~"published"
    }),
    {200, _, Body, _} = blog_web:get(#{}, "/posts/" ++ integer_to_list(Id)),
    ?assertEqual(nomatch, binary:match(Body, ~"<script>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<h1>&lt;script&gt;alert(1)&lt;/script&gt;</h1>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<p>First paragraph.</p>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<p>Second &amp; last.</p>")),
    assert_no_erlang_terms(Body).

unknown_post_is_404(_Config) ->
    {404, _, Body, _} = blog_web:get(#{}, "/posts/999999", [{"accept", "text/html"}]),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<h1>Not found</h1>")),
    {404, _, _, _} = blog_web:get(#{}, "/posts/not-a-number").

stylesheet_is_self_hosted(_Config) ->
    {200, Headers, Css, _} = blog_web:get(#{}, "/assets/css/blog.css"),
    "text/css" = blog_web:header("content-type", Headers),
    ?assertNotEqual(nomatch, binary:match(Css, ~".post-card")),
    {200, _, Page, _} = blog_web:get(#{}, "/posts"),
    ?assertEqual(nomatch, binary:match(Page, [~"https://", ~"http://"])).

new_post_requires_login(_Config) ->
    {302, Headers, _, _} = blog_web:get(#{}, "/posts/new"),
    "/login" = blog_web:header("location", Headers),
    {403, _, _, _} = blog_web:post_form(#{}, "/posts", [{"title", "Sneaky"}]).

create_post_via_form(_Config) ->
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {200, _, Form, _} = blog_web:get(Jar, "/posts/new"),
    ?assertNotEqual(nomatch, binary:match(Form, ~"<option value=\"draft\" selected>")),
    {302, Headers, _, _} = blog_web:post_form(Jar, "/posts", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"title", "Written in a browser"},
        {"body", "Forms still work."},
        {"status", "published"}
    ]),
    "/posts/" ++ Id = blog_web:header("location", Headers),
    {200, _, Page, _} = blog_web:get(Jar, "/posts/" ++ Id),
    ?assertNotEqual(nomatch, binary:match(Page, ~"Post created.")),
    ?assertNotEqual(nomatch, binary:match(Page, ~"<h1>Written in a browser</h1>")),
    {200, _, Again, _} = blog_web:get(Jar, "/posts/" ++ Id),
    ?assertEqual(nomatch, binary:match(Again, ~"Post created.")).

create_post_without_csrf_token_is_403(_Config) ->
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {403, _, _, _} = blog_web:post_form(Jar, "/posts", [{"title", "Forged"}, {"body", "x"}]),
    {ok, []} = blog_repo:all(kura_query:from(blog_post)).

invalid_post_rerenders_form_with_errors(_Config) ->
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {200, _, Form, _} = blog_web:get(Jar, "/posts/new"),
    {422, _, Body, _} = blog_web:post_form(Jar, "/posts", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"title", "Hi"},
        {"body", ""},
        {"status", "published"}
    ]),
    ?assertNotEqual(nomatch, binary:match(Body, ~"value=\"Hi\"")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"should be at least 3 character(s)")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"can&#039;t be blank")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<option value=\"published\" selected>")),
    assert_no_erlang_terms(Body).

edit_form_is_prefilled(Config) ->
    #{id := Id} = blog_data_fixtures:post(?config(alice, Config), #{
        ~"title" => ~"Old title", ~"status" => ~"published"
    }),
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {200, _, Body, _} = blog_web:get(Jar, "/posts/" ++ integer_to_list(Id) ++ "/edit"),
    ?assertNotEqual(nomatch, binary:match(Body, ~"value=\"Old title\"")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<option value=\"published\" selected>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"name=\"lock_version\" value=\"1\"")),
    assert_no_erlang_terms(Body).

update_post_via_form(Config) ->
    #{id := Id} = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Old title"}),
    Path = "/posts/" ++ integer_to_list(Id),
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {200, _, Form, _} = blog_web:get(Jar, Path ++ "/edit"),
    {302, Headers, _, _} = blog_web:post_form(Jar, Path, [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"lock_version", "1"},
        {"title", "New title"},
        {"body", "Body text"},
        {"status", "draft"}
    ]),
    Path = blog_web:header("location", Headers),
    {200, _, Page, _} = blog_web:get(Jar, Path),
    ?assertNotEqual(nomatch, binary:match(Page, ~"Post updated.")),
    ?assertNotEqual(nomatch, binary:match(Page, ~"<h1>New title</h1>")).

stale_edit_is_rejected(Config) ->
    Post = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Shared"}),
    #{id := Id} = Post,
    Path = "/posts/" ++ integer_to_list(Id),
    Jar = blog_web:log_in("alice@example.com", ?PASSWORD),
    {200, _, Form, _} = blog_web:get(Jar, Path ++ "/edit"),
    {ok, _} = blog_posts:update_post(Post, #{~"title" => ~"Changed elsewhere"}),
    {302, Headers, _, _} = blog_web:post_form(Jar, Path, [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"lock_version", "1"},
        {"title", "My edit"},
        {"body", "Body text"}
    ]),
    "/posts/" ++ _ = Edit = blog_web:header("location", Headers),
    {200, _, Page, _} = blog_web:get(Jar, Edit),
    ?assertNotEqual(nomatch, binary:match(Page, ~"Someone else changed this post.")),
    {ok, #{title := ~"Changed elsewhere"}} = blog_repo:get(blog_post, Id).

assert_no_erlang_terms(Html) ->
    [
        ?assertEqual(nomatch, binary:match(Html, Term), Term)
     || Term <- [~"undefined", ~"\"null\"", ~">null<", ~"nil"]
    ].
