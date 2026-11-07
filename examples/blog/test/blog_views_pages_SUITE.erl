-module(blog_views_pages_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").

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

-define(PASSWORD, ~"correct horse battery").

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
    blog_data_fixtures:start(Config).

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    blog_data_fixtures:reset_rate_limit(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

index_lists_published_posts(Config) ->
    Alice = ?config(alice, Config),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Hello Nova", ~"status" => ~"published"}),
    _ = blog_data_fixtures:post(Alice, #{~"title" => ~"Unfinished"}),
    {ok, Resp} = nova_test:get("/posts", Config),
    ?assertStatus(200, Resp),
    ?assertHeader("content-type", "text/html", Resp),
    Body = nova_test:body(Resp),
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
    {ok, #{status := 200, body := Page1}} = nova_test:get("/posts", Config),
    ?assertNotEqual(nomatch, binary:match(Page1, ~"href=\"/posts?page=2\">Older")),
    ?assertEqual(nomatch, binary:match(Page1, ~"Newer")),
    {ok, #{status := 200, body := Page2}} = nova_test:get("/posts?page=2", Config),
    ?assertNotEqual(nomatch, binary:match(Page2, ~"href=\"/posts?page=1\">Newer")),
    ?assertNotEqual(nomatch, binary:match(Page2, ~">Post 1</a>")).

index_rejects_bad_page(Config) ->
    {ok, Resp} = nova_test:get("/posts?page=abc", Config),
    ?assertStatus(400, Resp).

post_page_escapes_and_formats(Config) ->
    #{id := Id} = blog_data_fixtures:post(?config(alice, Config), #{
        ~"title" => ~"<script>alert(1)</script>",
        ~"body" => ~"First paragraph.\n\nSecond & last.",
        ~"status" => ~"published"
    }),
    {ok, #{status := 200, body := Body}} = nova_test:get("/posts/" ++ integer_to_list(Id), Config),
    ?assertEqual(nomatch, binary:match(Body, ~"<script>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<h1>&lt;script&gt;alert(1)&lt;/script&gt;</h1>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<p>First paragraph.</p>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<p>Second &amp; last.</p>")),
    assert_no_erlang_terms(Body).

unknown_post_is_404(Config) ->
    Html = #{headers => [{~"accept", ~"text/html"}]},
    {ok, #{status := 404, body := Body}} = nova_test:get("/posts/999999", Html, Config),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<h1>Not found</h1>")),
    {ok, #{status := 404}} = nova_test:get("/posts/not-a-number", Config).

stylesheet_is_self_hosted(Config) ->
    {ok, Css} = nova_test:get("/assets/css/blog.css", Config),
    ?assertStatus(200, Css),
    ?assertHeader("content-type", "text/css", Css),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(Css), ~".post-card")),
    {ok, #{status := 200, body := Page}} = nova_test:get("/posts", Config),
    ?assertEqual(nomatch, binary:match(Page, [~"https://", ~"http://"])).

new_post_requires_login(Config) ->
    {ok, Resp} = nova_test:get("/posts/new", Config),
    ?assertStatus(302, Resp),
    ?assertHeader("location", "/login", Resp),
    {ok, #{status := 403}} = nova_test:post("/posts", #{form => [{~"title", ~"Sneaky"}]}, Config).

create_post_via_form(Config) ->
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, #{status := 200, body := Form}} = nova_test:get("/posts/new", Session),
    ?assertNotEqual(nomatch, binary:match(Form, ~"<option value=\"draft\" selected>")),
    {ok, Created} = nova_test:post(
        "/posts",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"title", ~"Written in a browser"},
                {~"body", ~"Forms still work."},
                {~"status", ~"published"}
            ]
        },
        Session
    ),
    ?assertStatus(302, Created),
    "/posts/" ++ _ = Path = nova_test:header("location", Created),
    {ok, #{status := 200, body := Page}} = nova_test:get(Path, Session),
    ?assertNotEqual(nomatch, binary:match(Page, ~"Post created.")),
    ?assertNotEqual(nomatch, binary:match(Page, ~"<h1>Written in a browser</h1>")),
    {ok, #{status := 200, body := Again}} = nova_test:get(Path, Session),
    ?assertEqual(nomatch, binary:match(Again, ~"Post created.")).

create_post_without_csrf_token_is_403(Config) ->
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    Forged = #{form => [{~"title", ~"Forged"}, {~"body", ~"x"}]},
    {ok, #{status := 403}} = nova_test:post("/posts", Forged, Session),
    {ok, []} = blog_repo:all(kura_query:from(blog_post)).

invalid_post_rerenders_form_with_errors(Config) ->
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, Form} = nova_test:get("/posts/new", Session),
    {ok, #{status := 422, body := Body}} = nova_test:post(
        "/posts",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"title", ~"Hi"},
                {~"body", ~""},
                {~"status", ~"published"}
            ]
        },
        Session
    ),
    ?assertNotEqual(nomatch, binary:match(Body, ~"value=\"Hi\"")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"should be at least 3 character(s)")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"can&#039;t be blank")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<option value=\"published\" selected>")),
    assert_no_erlang_terms(Body).

edit_form_is_prefilled(Config) ->
    #{id := Id} = blog_data_fixtures:post(?config(alice, Config), #{
        ~"title" => ~"Old title", ~"status" => ~"published"
    }),
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, #{status := 200, body := Body}} =
        nova_test:get("/posts/" ++ integer_to_list(Id) ++ "/edit", Session),
    ?assertNotEqual(nomatch, binary:match(Body, ~"value=\"Old title\"")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<option value=\"published\" selected>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"name=\"lock_version\" value=\"1\"")),
    assert_no_erlang_terms(Body).

update_post_via_form(Config) ->
    #{id := Id} = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Old title"}),
    Path = "/posts/" ++ integer_to_list(Id),
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, Form} = nova_test:get(Path ++ "/edit", Session),
    {ok, Updated} = nova_test:post(
        Path,
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"lock_version", ~"1"},
                {~"title", ~"New title"},
                {~"body", ~"Body text"},
                {~"status", ~"draft"}
            ]
        },
        Session
    ),
    ?assertStatus(302, Updated),
    ?assertHeader("location", Path, Updated),
    {ok, #{status := 200, body := Page}} = nova_test:get(Path, Session),
    ?assertNotEqual(nomatch, binary:match(Page, ~"Post updated.")),
    ?assertNotEqual(nomatch, binary:match(Page, ~"<h1>New title</h1>")).

stale_edit_is_rejected(Config) ->
    Post = blog_data_fixtures:post(?config(alice, Config), #{~"title" => ~"Shared"}),
    #{id := Id} = Post,
    Path = "/posts/" ++ integer_to_list(Id),
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, Form} = nova_test:get(Path ++ "/edit", Session),
    {ok, _} = blog_posts:update_post(Post, #{~"title" => ~"Changed elsewhere"}),
    {ok, Stale} = nova_test:post(
        Path,
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"lock_version", ~"1"},
                {~"title", ~"My edit"},
                {~"body", ~"Body text"}
            ]
        },
        Session
    ),
    ?assertStatus(302, Stale),
    "/posts/" ++ _ = Edit = nova_test:header("location", Stale),
    {ok, #{status := 200, body := Page}} = nova_test:get(Edit, Session),
    ?assertNotEqual(nomatch, binary:match(Page, ~"Someone else changed this post.")),
    {ok, #{title := ~"Changed elsewhere"}} = blog_repo:get(blog_post, Id).

assert_no_erlang_terms(Html) ->
    [
        ?assertEqual(nomatch, binary:match(Html, Term), Term)
     || Term <- [~"undefined", ~"\"null\"", ~">null<", ~"nil"]
    ].
