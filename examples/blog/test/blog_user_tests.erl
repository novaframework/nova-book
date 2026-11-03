-module(blog_user_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

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
    ?assertEqual(#{username => [~"has invalid format"]}, Errors(#{~"username" => ~"alice\n"})),
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

scrubs_the_password_when_valid() ->
    Registration = blog_user:registration_changeset(#{}, registration(#{})),
    ?assert(Registration#kura_changeset.valid),
    assert_no_password(Registration),
    Change = blog_user:password_changeset(#{id => 1}, #{
        ~"password" => ~"a brand new passphrase",
        ~"password_confirmation" => ~"a brand new passphrase"
    }),
    ?assert(Change#kura_changeset.valid),
    assert_no_password(Change).

scrubs_the_password_when_invalid() ->
    BadEmail = blog_user:registration_changeset(#{}, registration(#{~"email" => ~"nope"})),
    ?assertNot(BadEmail#kura_changeset.valid),
    assert_no_password(BadEmail),
    Mismatch = blog_user:password_changeset(#{id => 1}, #{
        ~"password" => ~"a brand new passphrase", ~"password_confirmation" => ~"a different one"
    }),
    ?assertNot(Mismatch#kura_changeset.valid),
    assert_no_password(Mismatch).

counts_password_characters_not_bytes() ->
    Emoji = ~"🔑🔑🔑🔑",
    ?assertEqual(16, byte_size(Emoji)),
    ?assertEqual(
        #{password => [~"should be at least 12 character(s)"]},
        password_errors(Emoji)
    ),
    Japanese = ~"ふるいけやかわずとびこむみずのおとしずかなよるにつきがのぼるのをみていたよずっと",
    ?assertEqual(40, string:length(Japanese)),
    ?assertEqual(120, byte_size(Japanese)),
    ?assertEqual(#{}, password_errors(Japanese)),
    ?assertEqual(
        #{password => [~"should be at most 1024 character(s)"]},
        password_errors(binary:copy(~"a", 1025))
    ),
    ?assertEqual(
        #{password => [~"is not valid UTF-8"]}, password_errors(<<"correct horse ", 255>>)
    ).

assert_no_password(#kura_changeset{changes = Changes, params = Params}) ->
    Secrets = [password, password_confirmation],
    ?assertEqual(#{}, maps:with(Secrets, Changes)),
    ?assertEqual(#{}, maps:with(Secrets, Params)).

password_errors(Password) ->
    blog_api_json:errors(
        blog_user:registration_changeset(
            #{}, registration(#{~"password" => Password, ~"password_confirmation" => Password})
        )
    ).

email_test_() ->
    [
        ?_assertEqual({ok, ~"victim@b.com"}, blog_user:normalize_email(~"Victim@B.com")),
        ?_assertEqual({ok, ~"victim@b.com"}, blog_user:normalize_email(~"victim@b.com\n")),
        ?_assertEqual(
            blog_user:normalize_email(~"jos\x{E9}@example.com"),
            blog_user:normalize_email(~"JOSE\x{301}@example.com")
        ),
        ?_assertEqual({error, invalid_encoding}, blog_user:normalize_email(<<"a", 255, "@b.com">>)),
        ?_assertEqual(~"victim@b.com", registered_email(~"Victim@B.com\n")),
        ?_assertEqual(
            #{email => [~"is not valid UTF-8"]},
            blog_api_json:errors(email_change(#{}, <<"a", 255, "@b.com">>))
        )
    ].

email_change_test_() ->
    Confirmed = #{
        id => 1, email => ~"alice@example.com", confirmed_at => {{2026, 11, 1}, {9, 0, 0}}
    },
    Changed = email_change(Confirmed, ~"mallory@example.com"),
    SameAddress = email_change(Confirmed, ~"Alice@Example.com"),
    [
        ?_assert(Changed#kura_changeset.valid),
        ?_assertEqual(~"mallory@example.com", kura_changeset:get_change(Changed, email)),
        ?_assertEqual({ok, undefined}, maps:find(confirmed_at, Changed#kura_changeset.changes)),
        ?_assert(SameAddress#kura_changeset.valid),
        ?_assertEqual(#{}, SameAddress#kura_changeset.changes)
    ].

registered_email(Email) ->
    kura_changeset:get_change(
        blog_user:registration_changeset(#{}, registration(#{~"email" => Email})), email
    ).

email_change(User, Email) ->
    blog_user:email_changeset(User, #{~"email" => Email}).

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
