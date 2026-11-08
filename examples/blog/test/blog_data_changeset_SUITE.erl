-module(blog_data_changeset_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kura/include/kura.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    post_changeset_requires_title_and_body/1,
    post_changeset_checks_title_length/1,
    post_changeset_ignores_fields_not_cast/1,
    post_changeset_casts_status_enum/1,
    publishing_stamps_published_at/1,
    registration_hashes_password/1,
    registration_validates_fields/1,
    registration_needs_matching_confirmation/1,
    unique_email_becomes_changeset_error/1,
    email_is_unique_after_normalisation/1,
    trailing_newline_is_trimmed/1,
    email_change_clears_confirmed_at/1,
    errors_render_as_json_map/1,
    schemaless_changeset_validates_query_params/1
]).

all() ->
    [
        post_changeset_requires_title_and_body,
        post_changeset_checks_title_length,
        post_changeset_ignores_fields_not_cast,
        post_changeset_casts_status_enum,
        publishing_stamps_published_at,
        registration_hashes_password,
        registration_validates_fields,
        registration_needs_matching_confirmation,
        unique_email_becomes_changeset_error,
        email_is_unique_after_normalisation,
        trailing_newline_is_trimmed,
        email_change_clears_confirmed_at,
        errors_render_as_json_map,
        schemaless_changeset_validates_query_params
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    Config.

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

post_changeset_requires_title_and_body(_Config) ->
    CS = blog_post:changeset(#{}, #{}),
    false = CS#kura_changeset.valid,
    [{title, ~"can't be blank"}, {body, ~"can't be blank"}] = CS#kura_changeset.errors.

post_changeset_checks_title_length(_Config) ->
    CS = blog_post:changeset(#{}, #{~"title" => ~"Hi", ~"body" => ~"Hello"}),
    [{title, ~"should be at least 3 character(s)"}] = CS#kura_changeset.errors.

post_changeset_ignores_fields_not_cast(_Config) ->
    CS = blog_post:changeset(#{}, #{
        ~"title" => ~"Hello", ~"body" => ~"World", ~"user_id" => 42, ~"id" => 7
    }),
    true = CS#kura_changeset.valid,
    #{title := ~"Hello", body := ~"World"} = Changes = CS#kura_changeset.changes,
    false = maps:is_key(user_id, Changes),
    false = maps:is_key(id, Changes).

post_changeset_casts_status_enum(_Config) ->
    CS = blog_post:changeset(#{}, #{~"title" => ~"Hello", ~"body" => ~"x", ~"status" => ~"draft"}),
    draft = kura_changeset:get_change(CS, status),
    Bad = blog_post:changeset(#{}, #{~"title" => ~"Hello", ~"body" => ~"x", ~"status" => ~"deleted"}),
    [{status, ~"is not a valid enum value"}] = Bad#kura_changeset.errors.

publishing_stamps_published_at(_Config) ->
    Draft = #{id => 1, title => ~"Hello", body => ~"x", status => draft, published_at => undefined},
    CS = blog_post:changeset(Draft, #{~"status" => ~"published"}),
    {{_, _, _}, {_, _, _}} = kura_changeset:get_change(CS, published_at),
    Again = blog_post:changeset(
        Draft#{status => archived, published_at => {{2026, 1, 1}, {0, 0, 0}}},
        #{~"status" => ~"published"}
    ),
    undefined = kura_changeset:get_change(Again, published_at).

registration_hashes_password(_Config) ->
    CS = blog_user:registration_changeset(#{}, registration(#{})),
    true = CS#kura_changeset.valid,
    <<"$pbkdf2-sha256$", _/binary>> = Hash = kura_changeset:get_change(CS, hashed_password),
    true = nova_auth_password:verify(~"correct horse battery", Hash),
    {ok, User} = blog_repo:insert(CS),
    author = maps:get(role, User),
    false = maps:is_key(password, User).

registration_validates_fields(_Config) ->
    CS = blog_user:registration_changeset(
        #{},
        registration(#{~"email" => ~"not-an-email", ~"username" => ~"A", ~"role" => ~"admin"})
    ),
    false = CS#kura_changeset.valid,
    #{
        email := [~"has invalid format"],
        username := [~"should be at least 2 character(s)", ~"has invalid format"]
    } = blog_api_json:errors(CS),
    false = maps:is_key(role, CS#kura_changeset.changes).

registration_needs_matching_confirmation(_Config) ->
    CS = blog_user:registration_changeset(
        #{}, registration(#{~"password_confirmation" => ~"something else!"})
    ),
    [{password_confirmation, ~"does not match"}] = CS#kura_changeset.errors,
    undefined = kura_changeset:get_change(CS, hashed_password).

unique_email_becomes_changeset_error(_Config) ->
    {ok, _} = blog_repo:insert(blog_user:registration_changeset(#{}, registration(#{}))),
    {error, CS} = blog_repo:insert(
        blog_user:registration_changeset(#{}, registration(#{~"username" => ~"alice2"}))
    ),
    [{email, ~"has already been taken"}] = CS#kura_changeset.errors,
    Secrets = [password, password_confirmation],
    0 = map_size(maps:with(Secrets, CS#kura_changeset.changes)),
    0 = map_size(maps:with(Secrets, CS#kura_changeset.params)).

email_is_unique_after_normalisation(_Config) ->
    {ok, #{email := ~"victim@b.com"}} = insert_user(~"Victim@B.com", ~"victim"),
    {error, CS} = insert_user(~"victim@b.com", ~"mallory"),
    [{email, ~"has already been taken"}] = CS#kura_changeset.errors.

trailing_newline_is_trimmed(_Config) ->
    {ok, #{email := ~"victim@b.com"}} = insert_user(~"victim@b.com\n", ~"victim"),
    {error, CS} = insert_user(~"victim@b.com", ~"mallory"),
    [{email, ~"has already been taken"}] = CS#kura_changeset.errors.

email_change_clears_confirmed_at(_Config) ->
    ConfirmedAt = {{2026, 11, 1}, {9, 0, 0}},
    User = blog_data_fixtures:user(~"carol"),
    {ok, Confirmed} = blog_repo:update(
        kura_changeset:cast(blog_user, User, #{confirmed_at => ConfirmedAt}, [confirmed_at])
    ),
    {ok, #{confirmed_at := ConfirmedAt}} =
        blog_repo:update(blog_user:email_changeset(Confirmed, #{~"email" => ~"CAROL@example.com"})),
    {ok, #{email := ~"carol@elsewhere.example", confirmed_at := undefined}} =
        blog_repo:update(
            blog_user:email_changeset(Confirmed, #{~"email" => ~"carol@elsewhere.example"})
        ),
    {ok, #{confirmed_at := undefined}} = blog_repo:get(blog_user, maps:get(id, User)).

errors_render_as_json_map(_Config) ->
    CS = blog_post:changeset(#{}, #{~"title" => ~"Hi"}),
    #{title := [~"should be at least 3 character(s)"], body := [~"can't be blank"]} =
        blog_api_json:errors(CS),
    {error, #kura_changeset{action = insert}} = blog_repo:insert(CS).

schemaless_changeset_validates_query_params(_Config) ->
    {error, CS} = blog_posts:list_posts(#{~"page" => ~"0", ~"page_size" => ~"abc"}),
    #{page := [~"must be greater than 0"], page_size := [_]} = blog_api_json:errors(CS),
    {ok, #{page := 1, page_size := 10}} = blog_posts:list_posts(#{}).

insert_user(Email, Username) ->
    blog_repo:insert(
        blog_user:registration_changeset(
            #{}, registration(#{~"email" => Email, ~"username" => Username})
        )
    ).

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
