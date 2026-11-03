-module(blog_user).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0, associations/0]).
-export([
    registration_changeset/2,
    profile_changeset/2,
    password_changeset/2,
    email_changeset/2,
    role_changeset/2
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
        email, username, password, password_confirmation
    ]),
    CS1 = kura_changeset:validate_required(CS, [email, username, password]),
    CS2 = validate_email(CS1),
    CS3 = validate_username(CS2),
    CS4 = validate_password(CS3),
    hash_password(CS4).

profile_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [username]),
    CS1 = kura_changeset:validate_required(CS, [username]),
    validate_username(CS1).

password_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [password, password_confirmation]),
    CS1 = kura_changeset:validate_required(CS, [password]),
    hash_password(validate_password(CS1)).

email_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [email]),
    CS1 = kura_changeset:validate_required(CS, [email]),
    validate_email(CS1).

role_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [role]),
    kura_changeset:validate_required(CS, [role]).

validate_email(CS) ->
    CS1 = kura_changeset:validate_format(CS, email, ~"^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"),
    kura_changeset:validate_length(CS1, email, [{max, 160}]).

validate_username(CS) ->
    CS1 = kura_changeset:validate_length(CS, username, [{min, 2}, {max, 30}]),
    kura_changeset:validate_format(CS1, username, ~"^[a-z0-9_]+$").

validate_password(CS) ->
    CS1 = kura_changeset:validate_length(CS, password, [{min, 12}, {max, 72}]),
    kura_changeset:validate_confirmation(CS1, password).

hash_password(#kura_changeset{valid = true} = CS) ->
    case kura_changeset:get_change(CS, password) of
        undefined ->
            CS;
        Password ->
            kura_changeset:put_change(CS, hashed_password, nova_auth_password:hash(Password))
    end;
hash_password(CS) ->
    CS.
