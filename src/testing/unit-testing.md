# Unit Testing

Every chapter so far ended with a Common Test suite that boots the blog, talks to PostgreSQL and makes real HTTP requests. That is the right test for routing, plugins, sessions and queries, and the [next chapter](integration-testing.md) looks at how those suites are built. But a lot of the blog is plain functions: changesets, policies, JSON helpers, email builders, the pure parts of workers, even most controllers. Those can be tested with EUnit, without starting anything, in milliseconds.

The rule this Part follows: **if a function does not need the database, a process or the network, test it with EUnit.** Everything else goes to Common Test.

## Where the tests live

EUnit tests are modules named `<module>_tests` in `test/`. rebar3 finds them by name and runs `blog_post_tests` together with `blog_post`. The test profile in `rebar.config` already puts `test/` on the compile path and adds the test-only dependencies:

```erlang
{test, [
    {erl_opts, [nowarn_export_all, warnings_as_errors, {i, "test/util"}]},
    {deps, [
        {meck, "1.2.0"},
        {nova_test, "~> 0.3"},
        {gun, "2.5.0"}
    ]},
    {ct_opts, [{sys_config, "./config/test_sys.config"}]},
    {extra_src_dirs, [{"test", [{recursive, true}]}]},
    {xref_extra_paths, ["test"]}
]}
```

`nova_test` gives request builders and assertion macros for controllers, `meck` replaces modules in a test, and `gun` is the WebSocket client used in [Testing Real-Time](testing-realtime.md).

Run every EUnit test with:

```shell
$ rebar3 eunit
===> Performing EUnit tests...
.........................................................
Finished in 1.316 seconds
57 tests, 0 failures
```

No database container is needed for this. Narrow it down while you work:

```shell
rebar3 eunit --module=blog_policy_tests
rebar3 eunit --test=blog_policy_tests:any_author_can_create_test
rebar3 eunit --generator=blog_publish_worker_tests:perform_test_
rebar3 eunit --verbose --module=blog_purge_worker_tests
```

`--test` runs one plain `*_test` function. A `*_test_` function is a generator that returns a list of tests, so run it with `--generator`; given to `--test` it counts as one test and runs none of the ones it returns.

## Changesets

A changeset takes data and params and returns a `#kura_changeset{}` record. Nothing is written until a repo function is called, so validation is pure. `test/blog_post_tests.erl`:

```erlang
-module(blog_post_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

valid_post_test() ->
    CS = blog_post:changeset(#{}, #{~"title" => ~"Hello", ~"body" => ~"World"}),
    ?assert(CS#kura_changeset.valid),
    ?assertEqual(~"Hello", kura_changeset:get_change(CS, title)).

title_and_body_are_required_test() ->
    CS = blog_post:changeset(#{}, #{}),
    ?assertNot(CS#kura_changeset.valid),
    ?assertEqual(
        [{title, ~"can't be blank"}, {body, ~"can't be blank"}], CS#kura_changeset.errors
    ).
```

Boundaries are a good fit for a test generator. A function ending in `_test_` returns a list of tests, and `?_assertEqual` builds one without running it, so each case is reported on its own:

```erlang
title_length_test_() ->
    Errors = fun(Title) ->
        CS = blog_post:changeset(#{}, #{~"title" => Title, ~"body" => ~"x"}),
        CS#kura_changeset.errors
    end,
    [
        ?_assertEqual([{title, ~"should be at least 3 character(s)"}], Errors(~"Hi")),
        ?_assertEqual([], Errors(~"Hey")),
        ?_assertEqual([], Errors(binary:copy(~"a", 200))),
        ?_assertEqual(
            [{title, ~"should be at most 200 character(s)"}], Errors(binary:copy(~"a", 201))
        )
    ].
```

The embedded metadata from [Many-to-Many & Embedded Schemas](../data-layer/advanced-data.md) is validated by the same call:

```erlang
metadata_is_validated_as_an_embed_test() ->
    Params = #{
        ~"title" => ~"Hello",
        ~"body" => ~"World",
        ~"metadata" => #{~"cover_image_url" => ~"http://example.com/a.png", ~"reading_minutes" => 0}
    },
    CS = blog_post:changeset(#{}, Params),
    ?assertNot(CS#kura_changeset.valid),
    ?assertEqual(
        #{
            cover_image_url => [~"has invalid format"],
            reading_minutes => [~"must be greater than 0"]
        },
        blog_api_json:errors(CS)
    ).
```

The test pins a detail worth knowing: Kura reports an embed's errors under the embed's own field names, at the top level of the changeset, not under `metadata`. An API client cannot tell a post field from an embed field with the same name.

Some changeset cases still belong in Common Test. `unique_email_becomes_changeset_error` in `blog_data_changeset_SUITE` needs the database's unique index, so it stays there, and so do `email_is_unique_after_normalisation` (`Victim@B.com` then `victim@b.com`), `trailing_newline_is_trimmed` and `email_change_clears_confirmed_at`, which reads the row back after the update.

## Fixtures: `setup` and `foreach`

Registration hashes the password with PBKDF2, and the production iteration count makes each hash take a noticeable fraction of a second. `test/blog_user_tests.erl` lowers it for the duration of its tests and puts the old value back afterwards:

```erlang
registration_test_() ->
    {setup, fun fast_hashing/0, fun restore_hashing/1, [
        fun hashes_a_valid_password/0,
        fun never_casts_the_role/0,
        fun rejects_bad_input/0,
        fun skips_hashing_when_invalid/0,
        fun scrubs_the_password_when_valid/0,
        fun scrubs_the_password_when_invalid/0,
        fun counts_password_characters_not_bytes/0
    ]}.

fast_hashing() ->
    Previous = application:get_env(nova_auth, pbkdf2_iterations),
    ok = application:set_env(nova_auth, pbkdf2_iterations, 1000),
    Previous.

restore_hashing(undefined) ->
    application:unset_env(nova_auth, pbkdf2_iterations);
restore_hashing({ok, Iterations}) ->
    application:set_env(nova_auth, pbkdf2_iterations, Iterations).

never_casts_the_role() ->
    CS = blog_user:registration_changeset(#{}, registration(#{~"role" => ~"admin"})),
    ?assertEqual(undefined, kura_changeset:get_change(CS, role)).
```

`{setup, Setup, Cleanup, Tests}` runs `Setup` once around all the tests, and its result is passed to `Cleanup`. `{foreach, Setup, Cleanup, Tests}` runs them around each test. The tests are named functions (`fun hashes_a_valid_password/0`), which keeps the fixture readable and lets a failure point at a real function name.

`never_casts_the_role` is a security test: registration must ignore a `role` in the form, or anyone could sign up as an admin.

The password tests are security tests too. A changeset that comes back as an error is exactly the one that gets logged, so the scrub is checked on the invalid path as well as the valid one:

```erlang
scrubs_the_password_when_invalid() ->
    BadEmail = blog_user:registration_changeset(#{}, registration(#{~"email" => ~"nope"})),
    ?assertNot(BadEmail#kura_changeset.valid),
    assert_no_password(BadEmail),
    Mismatch = blog_user:password_changeset(#{id => 1}, #{
        ~"password" => ~"a brand new passphrase", ~"password_confirmation" => ~"a different one"
    }),
    ?assertNot(Mismatch#kura_changeset.valid),
    assert_no_password(Mismatch).

assert_no_password(#kura_changeset{changes = Changes, params = Params}) ->
    Secrets = [password, password_confirmation],
    ?assertEqual(#{}, maps:with(Secrets, Changes)),
    ?assertEqual(#{}, maps:with(Secrets, Params)).
```

`counts_password_characters_not_bytes` pins the length rule with inputs where bytes and characters disagree: four emoji (16 bytes) are too short, and a 40-character Japanese passphrase (120 bytes) is accepted. The email rules have their own generator, `email_test_()`, and `email_change_test_()` checks that a new address clears `confirmed_at` while retyping the same one in capitals changes nothing.

## Policies

`blog_policy:can/3` from [Authorization](../auth-sessions/authorization.md) only looks at the actor and the resource, so every rule can be checked without a session. `test/blog_policy_tests.erl`:

```erlang
-define(ALICE, #{id => 1, role => author}).
-define(BOB, #{id => 2, role => author}).
-define(ADMIN, #{id => 3, role => admin}).
-define(ALICES_POST, #{id => 10, user_id => 1}).

anonymous_readers_can_do_nothing_test_() ->
    [
        ?_assertNot(blog_policy:can(undefined, Action, ?ALICES_POST))
     || Action <- [create_post, update_post, delete_post, view_draft, manage_users]
    ].

only_the_owner_or_an_admin_can_edit_test_() ->
    [
        ?_assert(blog_policy:can(?ALICE, update_post, ?ALICES_POST)),
        ?_assertNot(blog_policy:can(?BOB, update_post, ?ALICES_POST)),
        ?_assert(blog_policy:can(?ADMIN, delete_post, ?ALICES_POST)),
        ?_assertNot(blog_policy:can(?BOB, view_draft, ?ALICES_POST))
    ].

unknown_actions_are_denied_test() ->
    ?assertNot(blog_policy:can(?ADMIN, launch_rockets, #{})).
```

The last test is the one that matters most: an action nobody wrote a rule for is denied, even for an admin.

## JSON helpers

`blog_api_json` turns records into the maps the API returns. Its tests check the formats clients depend on:

```erlang
errors_group_messages_by_field_test() ->
    CS = blog_post:changeset(#{}, #{~"title" => ~"Hi"}),
    ?assertEqual(
        #{title => [~"should be at least 3 character(s)"], body => [~"can't be blank"]},
        blog_api_json:errors(CS)
    ).

datetimes_are_rfc3339_utc_test_() ->
    [
        ?_assertEqual(~"2026-11-03T08:30:00Z", blog_api_json:datetime({{2026, 11, 3}, {8, 30, 0}})),
        ?_assertEqual(null, blog_api_json:datetime(undefined))
    ].
```

`post_includes_only_loaded_associations_test` in the same file builds a post map by hand, renders it, and encodes the result with OTP's `json` module, so a value `json:encode/1` cannot handle fails here and not in a request.

## Emails

The email builders in `blog_emails` take maps and return a `#hikyaku_email{}` record. Hikyaku has no accessor functions, so tests match on the record from `hikyaku/include/hikyaku.hrl`. A comment notification only needs the preloaded shape, which a test can write out by hand. `test/blog_emails_tests.erl`:

```erlang
comment_notification_test() ->
    Comment = #{
        body => ~"<script>alert(1)</script>",
        author => #{username => ~"bob"},
        post => #{
            id => 42,
            title => ~"Tom & Jerry",
            author => #{email => ~"alice@example.com", username => ~"alice"}
        }
    },
    #hikyaku_email{
        to = [#hikyaku_address{address = ~"alice@example.com"}],
        subject = Subject,
        text_body = Text,
        html_body = Html
    } = blog_emails:comment_notification(Comment),
    ?assertEqual(~"bob commented on \"Tom & Jerry\"", Subject),
    ?assertMatch({_, _}, binary:match(Text, ~"http://localhost:8080/api/posts/42")),
    ?assertMatch({_, _}, binary:match(Html, ~"&lt;script&gt;")),
    ?assertEqual(nomatch, binary:match(Html, ~"<script>")).
```

The templates are compiled modules, so they render here too, and the test proves the HTML body escapes what a commenter wrote. `blog_email_SUITE` has the database-backed version of the same check, which loads a real comment with its author and post.

### Delivery and the test adapters

`blog_mailer:deliver_from_job/1` turns a delivery result into a job result: `ok`, `{snooze, 60}` when the provider rate-limits, or the error. The adapter comes from the `mailer` key of the blog's environment, so a test can swap it. Hikyaku's own `hikyaku_adapter_test` sends the email to a process; the blog's `test/blog_test_mail_adapter.erl` answers whatever the config says:

```erlang
-module(blog_test_mail_adapter).
-behaviour(hikyaku_adapter).

-export([deliver/2]).

deliver(_Email, #{reply := Reply}) ->
    Reply.
```

`test/blog_mailer_tests.erl` uses a `foreach` fixture so each test starts from the saved config:

```erlang
deliver_from_job_test_() ->
    {foreach, fun save_mailer/0, fun restore_mailer/1, [
        fun delivered_mail_is_ok/0,
        fun rate_limited_provider_snoozes/0,
        fun other_failures_are_errors/0
    ]}.

delivered_mail_is_ok() ->
    ok = application:set_env(blog, mailer, #{adapter => hikyaku_adapter_test, pid => self()}),
    ?assertEqual(ok, blog_mailer:deliver_from_job(email())),
    receive
        {hikyaku_email, Email} -> ?assertEqual(email(), Email)
    after 1000 -> ?assert(false)
    end.

rate_limited_provider_snoozes() ->
    reply_with({error, #{status_code => 429}}),
    ?assertEqual({snooze, 60}, blog_mailer:deliver_from_job(email())).

reply_with(Reply) ->
    ok = application:set_env(blog, mailer, #{adapter => blog_test_mail_adapter, reply => Reply}).
```

An adapter is the seam Hikyaku was designed with, so no mocking library is needed.

## Workers

A Shigoto worker is a module with `perform/1`. Pure helpers in a worker are tested like any function. `blog_purge_worker:cutoff/2` from [Scheduling](../jobs/scheduling.md#the-purge-worker) works out the time before which deleted posts are purged. `test/blog_purge_worker_tests.erl`:

```erlang
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
```

The current time is an argument rather than a call inside the function, so the edge cases (a month boundary, a year boundary, a leap day) are tested without a clock. `perform/1` passes `calendar:universal_time()`.

For `perform/1` itself, `shigoto:perform_job/2,3` runs a worker through the same middleware chain production uses, with no queue, no job row and no retries, and returns what `perform/1` returned. It does not need Shigoto to be started. Most of the blog's workers read the database, so [Transactional Email](../email/transactional-email.md) and [Operations](../jobs/operations.md) call `perform_job` from Common Test with real rows.

### When meck earns its place

`blog_publish_worker` has four outcomes: it publishes a draft, leaves an already published post alone, ignores a deleted post, and reports a post that no longer validates. The last one is hard to produce with real data, because a post in the database passed validation when it was saved. Here replacing `blog_repo` for the test is worth it. `test/blog_publish_worker_tests.erl`:

```erlang
perform_test_() ->
    {foreach, fun mock_repo/0, fun unmock_repo/1, [
        fun publishes_a_draft/0,
        fun leaves_published_posts_alone/0,
        fun ignores_a_deleted_post/0,
        fun reports_a_post_that_no_longer_validates/0
    ]}.

mock_repo() ->
    ok = meck:new(blog_repo).

unmock_repo(_) ->
    ?assert(meck:validate(blog_repo)),
    meck:unload(blog_repo).

publishes_a_draft() ->
    meck:expect(blog_repo, get, [blog_post, 7], {ok, draft()}),
    meck:expect(blog_repo, update, 1, {ok, #{}}),
    ?assertEqual(ok, shigoto:perform_job(blog_publish_worker, #{~"post_id" => 7})),
    CS = meck:capture(first, blog_repo, update, '_', 1),
    ?assertEqual(published, kura_changeset:get_change(CS, status)).

leaves_published_posts_alone() ->
    meck:expect(blog_repo, get, [blog_post, 7], {ok, (draft())#{status => published}}),
    ?assertEqual(ok, shigoto:perform_job(blog_publish_worker, #{~"post_id" => 7})),
    ?assertNot(meck:called(blog_repo, update, '_')).

reports_a_post_that_no_longer_validates() ->
    meck:expect(blog_repo, get, [blog_post, 7], {ok, draft()}),
    meck:expect(blog_repo, update, 1, {error, #kura_changeset{valid = false}}),
    ?assertEqual(
        {error, invalid_post}, shigoto:perform_job(blog_publish_worker, #{~"post_id" => 7})
    ).
```

`meck:expect(blog_repo, get, [blog_post, 7], Result)` answers only that call, so a worker that asked for a different post would crash the test. `meck:capture/5` returns the changeset the worker passed to `update/1`, and `meck:validate/1` in the cleanup fails the test if a mocked function was called with arguments it did not expect.

```admonish warning title="Use meck sparingly"
A mock is a copy of your assumptions about the real module. If `blog_repo:update/1` started returning `{error, stale}` for this post (it can, with optimistic locking), these tests would keep passing. Mock to reach a branch you cannot reach otherwise, and keep a test against the real database for the main path. meck replaces the module for the whole node, so never use it in Common Test suites that share the running app with other tests.
```

## Controllers without HTTP: `nova_test_req`

A Nova controller is a function from a request map to a return tuple, so it can be called directly. `nova_test` builds the request map. From `nova_test_req` (0.3.0):

| Function | What it sets |
|---|---|
| `nova_test_req:new(Method, Path)` | `method`, `path`, `host`, `port`, `scheme`, empty `headers`, `bindings` and `qs`, a local `peer` |
| `nova_test_req:with_bindings(Bindings, Req)` | `bindings` (path parameters, binary keys) |
| `nova_test_req:with_json(Json, Req)` | an encoded `body`, `json` (that body decoded again, so keys are binaries), the `content-type` header |
| `nova_test_req:with_header(Name, Value, Req)` | one request header (lowercase binary name) |
| `nova_test_req:with_query(Params, Req)` | `qs` (URL-encoded) and `parsed_qs` |
| `nova_test_req:with_form(Fields, Req)` | a form `body`, `params`, the `content-type` header |
| `nova_test_req:with_body(Body, Req)` | the raw `body`, not parsed |
| `nova_test_req:with_auth_data(Data, Req)` | `auth_data`, as a security function would |
| `nova_test_req:with_peer(Peer, Req)` | the client address |

The request map is what a controller sees *after* the plugins ran. `with_query/2` and `with_form/2` fill in `parsed_qs` and `params` the way `nova_request_plugin` does with `parse_qs => true` and `read_urlencoded_body => true`: a map with binary keys and values. Fields are a map or a list of pairs. A value of `undefined` raises `nova_test_undefined_form_value` instead of sending the string `"undefined"`.

`with_json/2` encodes with the `json_lib` Nova would use. In EUnit no application environment is loaded, so that is `thoas`, Nova's default and one of Nova's own dependencies. The blog's `{json_lib, blog_json}` in `config/test_sys.config` only applies under Common Test. Either way the controller sees the same decoded map.

`nova_test/include/nova_test.hrl` includes `eunit.hrl` and adds macros that match controller return values:

| Macro | Matches |
|---|---|
| `?assertJsonResponse(Pattern, Result)` | `{json, Pattern}` |
| `?assertJsonResponse(Status, Pattern, Result)` | `{json, Status, _Headers, Pattern}` |
| `?assertStatusResponse(Code, Result)` | `{status, Code}` |
| `?assertRedirect(Target, Result)` | `{redirect, Target}` |
| `?assertOkResponse(Pattern, Result)` | `{ok, Pattern}` (template variables) |

Each macro, and each HTTP macro in the [next chapter](integration-testing.md#json-apis-with-nova_test), also has a generator version, `?_assertJsonResponse` and so on. `test/blog_hello_controller_tests.erl` tests the controller from [Controllers & Responses](../foundations/controllers.md):

```erlang
-module(blog_hello_controller_tests).
-include_lib("nova_test/include/nova_test.hrl").

greet_test() ->
    Req = greet_req(~"Ada", #{}),
    ?assertJsonResponse(#{greeting := ~"Hello, Ada!"}, blog_hello_controller:greet(Req)).

greet_can_shout_test() ->
    Req = greet_req(~"Ada", #{~"shout" => ~"true"}),
    ?assertJsonResponse(#{greeting := ~"HELLO, ADA!"}, blog_hello_controller:greet(Req)).

greet_rejects_bad_names_test() ->
    Req = greet_req(~"<script>", #{}),
    ?assertEqual({error, invalid_name}, blog_hello_controller:greet(Req)).

create_from_json_test() ->
    Req = nova_test_req:with_json(#{~"name" => ~"Ada"}, nova_test_req:new(post, "/hello")),
    ?assertJsonResponse(201, #{name := ~"Ada"}, blog_hello_controller:create(Req)),
    {json, 201, Headers, _} = blog_hello_controller:create(Req),
    ?assertEqual(#{~"location" => ~"/hello/Ada"}, Headers).

create_from_form_test() ->
    Req = nova_test_req:with_form(#{~"name" => ~"Ada"}, nova_test_req:new(post, "/hello")),
    ?assertJsonResponse(201, #{name := ~"Ada"}, blog_hello_controller:create(Req)).

greet_req(Name, Query) ->
    Req = nova_test_req:new(get, <<"/hello/", Name/binary>>),
    nova_test_req:with_query(Query, nova_test_req:with_bindings(#{~"name" => Name}, Req)).
```

The response map keys are atoms (`greeting`), because the test sees the term before it is encoded to JSON. An invalid name returns `{error, invalid_name}`, which Nova hands to the controller's fallback. The fallback is a module too, so test it the same way. With the generator versions each error is reported as its own test:

```erlang
fallback_turns_errors_into_responses_test_() ->
    Req = nova_test_req:new(get, "/hello/x"),
    [
        ?_assertJsonResponse(
            422,
            #{error := ~"invalid_name"},
            blog_fallback_controller:resolve(Req, {error, invalid_name})
        ),
        ?_assertJsonResponse(
            400,
            #{error := ~"missing_name"},
            blog_fallback_controller:resolve(Req, {error, missing_name})
        )
    ].
```

### Security functions

Security functions take the same request map. The parts of `blog_security` and `blog_ops_security` that only read headers are unit-testable. `test/blog_security_tests.erl`:

```erlang
bearer_token_test_() ->
    Token = fun(Value) ->
        blog_security:bearer_token(
            nova_test_req:with_header(~"authorization", Value, nova_test_req:new(get, "/api/me"))
        )
    end,
    [
        ?_assertEqual({ok, ~"abc123"}, Token(~"Bearer abc123")),
        ?_assertEqual({ok, ~"abc123"}, Token(~"bearer  abc123 ")),
        ?_assertEqual(error, Token(~"Basic YWxhZGRpbjpvcGVu")),
        ?_assertEqual(error, blog_security:bearer_token(nova_test_req:new(get, "/api/me")))
    ].

missing_bearer_token_is_401_test() ->
    {false, 401, Headers, Body} = blog_security:require_bearer(nova_test_req:new(get, "/api/me")),
    ?assertEqual(~"Bearer", maps:get(~"www-authenticate", Headers)),
    ?assertEqual(#{~"error" => ~"missing bearer token"}, json:decode(Body)).

ops_token_test_() ->
    {setup, fun set_ops_token/0, fun unset_ops_token/1, [
        ?_assertEqual({true, #{role => ops}}, blog_ops_security:require_token(ops_req(~"s3cret"))),
        ?_assertMatch({false, 401, _, _}, blog_ops_security:require_token(ops_req(~"guess"))),
        ?_assertMatch(
            {false, 401, _, _}, blog_ops_security:require_token(nova_test_req:new(get, "/ops"))
        )
    ]}.
```

## Where unit tests stop

Not every controller can be called this way. A controller or security function that reads the session (`nova_session:get/2`) needs Nova's session manager and a session cookie, one that queries the database needs a repo, and a plugin's effect only shows when Nova runs the plugin chain. `blog_security:require_user/1`, the posts controllers and the CSRF plugin are therefore tested over HTTP. `nova_test_req` is for the logic in between: parsing, validation, choosing a response.

The live views from Part VI are modules with pure callbacks as well; [Testing Real-Time](testing-realtime.md#live-views-as-functions) unit-tests the counter.

---

Next: [Integration Testing](integration-testing.md), running the whole blog against PostgreSQL.
