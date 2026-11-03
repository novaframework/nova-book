-module(blog_user_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

registration_test_() ->
    {setup, fun fast_hashing/0, fun restore_hashing/1, [
        fun hashes_a_valid_password/0,
        fun never_casts_the_role/0,
        fun rejects_bad_input/0,
        fun skips_hashing_when_invalid/0
    ]}.

fast_hashing() ->
    Previous = application:get_env(nova_auth, pbkdf2_iterations),
    ok = application:set_env(nova_auth, pbkdf2_iterations, 1000),
    Previous.

restore_hashing(undefined) ->
    application:unset_env(nova_auth, pbkdf2_iterations);
restore_hashing({ok, Iterations}) ->
    application:set_env(nova_auth, pbkdf2_iterations, Iterations).

hashes_a_valid_password() ->
    CS = blog_user:registration_changeset(#{}, registration(#{})),
    ?assert(CS#kura_changeset.valid),
    Hash = kura_changeset:get_change(CS, hashed_password),
    ?assertMatch(<<"$pbkdf2-sha256$", _/binary>>, Hash),
    ?assert(nova_auth_password:verify(~"correct horse battery", Hash)).

never_casts_the_role() ->
    CS = blog_user:registration_changeset(#{}, registration(#{~"role" => ~"admin"})),
    ?assertEqual(undefined, kura_changeset:get_change(CS, role)).

rejects_bad_input() ->
    Errors = fun(Overrides) ->
        blog_api_json:errors(blog_user:registration_changeset(#{}, registration(Overrides)))
    end,
    ?assertEqual(#{email => [~"has invalid format"]}, Errors(#{~"email" => ~"alice.example.com"})),
    ?assertEqual(#{username => [~"has invalid format"]}, Errors(#{~"username" => ~"Alice Smith"})),
    ?assertEqual(
        #{password => [~"should be at least 12 character(s)"]},
        Errors(#{~"password" => ~"short", ~"password_confirmation" => ~"short"})
    ),
    ?assertEqual(
        #{password_confirmation => [~"does not match"]},
        Errors(#{~"password_confirmation" => ~"something else"})
    ).

skips_hashing_when_invalid() ->
    CS = blog_user:registration_changeset(#{}, registration(#{~"email" => ~""})),
    ?assertNot(CS#kura_changeset.valid),
    ?assertEqual(undefined, kura_changeset:get_change(CS, hashed_password)).

registration(Overrides) ->
    maps:merge(
        #{
            ~"email" => ~"alice@example.com",
            ~"username" => ~"alice",
            ~"password" => ~"correct horse battery",
            ~"password_confirmation" => ~"correct horse battery"
        },
        Overrides
    ).
