-module(blog_user).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0, associations/0, before_delete/1]).
-export([
    registration_changeset/2,
    profile_changeset/2,
    password_changeset/2,
    email_changeset/2,
    role_changeset/2,
    normalize_email/1
]).

table() -> ~"users".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = email, type = string, nullable = false},
        #kura_field{name = username, type = string, nullable = false},
        #kura_field{name = hashed_password, type = string, nullable = false},
        #kura_field{
            name = role, type = {enum, [author, admin]}, nullable = false, default = author
        },
        #kura_field{name = full_name, type = {encrypted, string}},
        #kura_field{name = confirmed_at, type = utc_datetime},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime},
        #kura_field{name = password, type = string, virtual = true},
        #kura_field{name = password_confirmation, type = string, virtual = true}
    ].

indexes() ->
    [
        {[email], #{unique => true}},
        {[username], #{unique => true}}
    ].

associations() ->
    [
        #kura_assoc{name = posts, type = has_many, schema = blog_post, foreign_key = user_id},
        #kura_assoc{name = comments, type = has_many, schema = blog_comment, foreign_key = user_id}
    ].

registration_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [
        email, username, full_name, password, password_confirmation
    ]),
    CS1 = kura_changeset:validate_required(CS, [email, username, password]),
    CS2 = validate_email(CS1),
    CS3 = validate_username(CS2),
    CS4 = validate_password(CS3),
    scrub_password(hash_password(CS4)).

profile_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [username, full_name]),
    CS1 = kura_changeset:validate_required(CS, [username]),
    validate_username(CS1).

password_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [password, password_confirmation]),
    CS1 = kura_changeset:validate_required(CS, [password]),
    scrub_password(hash_password(validate_password(CS1))).

email_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [email]),
    CS1 = kura_changeset:validate_required(CS, [email]),
    reset_confirmation(validate_email(CS1)).

role_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [role]),
    kura_changeset:validate_required(CS, [role]).

before_delete(#{role := admin, id := Id}) ->
    Others = kura_query:where(
        kura_query:from(?MODULE), {'and', [{role, ~"admin"}, {id, '!=', Id}]}
    ),
    case blog_repo:exists(Others) of
        {ok, true} -> ok;
        {ok, false} -> {error, last_admin}
    end;
before_delete(_User) ->
    ok.

normalize_email(Email) ->
    case unicode:characters_to_nfc_binary(Email) of
        Normalized when is_binary(Normalized) ->
            {ok, string:casefold(string:trim(Normalized))};
        _ ->
            {error, invalid_encoding}
    end.

validate_email(CS) ->
    CS1 = put_normalized_email(CS, kura_changeset:get_change(CS, email)),
    CS2 = kura_changeset:validate_format(CS1, email, ~"^[^@\\s]+@[^@\\s]+\\.[^@\\s]+\\z"),
    kura_changeset:validate_length(CS2, email, [{max, 160}]).

put_normalized_email(CS, undefined) ->
    CS;
put_normalized_email(CS, Email) ->
    case normalize_email(Email) of
        {ok, Normalized} -> put_email(CS, Normalized);
        {error, invalid_encoding} -> kura_changeset:add_error(CS, email, ~"is not valid UTF-8")
    end.

put_email(#kura_changeset{data = #{email := Email}, changes = Changes} = CS, Email) ->
    CS#kura_changeset{changes = maps:remove(email, Changes)};
put_email(CS, Email) ->
    kura_changeset:put_change(CS, email, Email).

reset_confirmation(CS) ->
    case kura_changeset:get_change(CS, email) of
        undefined -> CS;
        _NewEmail -> kura_changeset:put_change(CS, confirmed_at, undefined)
    end.

validate_username(CS) ->
    CS1 = kura_changeset:validate_length(CS, username, [{min, 2}, {max, 30}]),
    kura_changeset:validate_format(CS1, username, ~"^[a-z0-9_]+\\z").

validate_password(CS) ->
    CS1 = validate_password_length(CS, kura_changeset:get_change(CS, password)),
    kura_changeset:validate_confirmation(CS1, password).

validate_password_length(CS, undefined) ->
    CS;
validate_password_length(CS, Password) ->
    case unicode:characters_to_binary(Password) of
        Password -> check_password_length(CS, length(unicode:characters_to_list(Password)));
        _ -> kura_changeset:add_error(CS, password, ~"is not valid UTF-8")
    end.

check_password_length(CS, Length) when Length < 12 ->
    kura_changeset:add_error(CS, password, ~"should be at least 12 character(s)");
check_password_length(CS, Length) when Length > 1024 ->
    kura_changeset:add_error(CS, password, ~"should be at most 1024 character(s)");
check_password_length(CS, _Length) ->
    CS.

hash_password(#kura_changeset{valid = true} = CS) ->
    case kura_changeset:get_change(CS, password) of
        undefined ->
            CS;
        Password ->
            kura_changeset:put_change(CS, hashed_password, nova_auth_password:hash(Password))
    end;
hash_password(CS) ->
    CS.

scrub_password(#kura_changeset{changes = Changes, params = Params} = CS) ->
    Secrets = [password, password_confirmation],
    CS#kura_changeset{
        changes = maps:without(Secrets, Changes),
        params = maps:without(Secrets, Params)
    }.
