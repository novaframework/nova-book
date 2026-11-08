# Changesets and Validation

Data from the outside world (a JSON body, a form, a query string) never goes straight into the database. It goes through a **changeset**: a record that holds the original data, the changes you allowed, their types, and any validation errors. The repo only writes a changeset that is valid.

The flow is always the same:

1. **Cast**: pick the fields you allow from the params and convert them to the schema types.
2. **Validate**: check the changes (required, length, format, ...).
3. **Constrain**: declare which database constraints may fail, so they come back as errors instead of crashes.
4. **Persist**: hand the changeset to `blog_repo:insert/1` or `update/1`.

## A changeset for posts

Changeset functions live in the schema module. `blog_post:changeset/2`:

```erlang
-export([changeset/2]).

changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [title, body, status]),
    CS1 = kura_changeset:validate_required(CS, [title, body]),
    CS2 = kura_changeset:validate_length(CS1, title, [{min, 3}, {max, 200}]),
    stamp_published_at(CS2).

stamp_published_at(CS) ->
    case {kura_changeset:get_change(CS, status), kura_changeset:get_field(CS, published_at)} of
        {published, undefined} ->
            kura_changeset:put_change(CS, published_at, calendar:universal_time());
        _ ->
            CS
    end.
```

`Data` is the current record: `#{}` for a new post, the loaded post map for an update. `Params` is whatever arrived, usually a map with binary keys from a JSON body. `cast/4` keeps only `title`, `body` and `status`, converting each to its field type; anything else in `Params` is ignored. That allow-list is your protection against mass assignment: a client cannot set `user_id` or `inserted_at` by adding them to the JSON.

`stamp_published_at/1` shows the tools for reading and changing a changeset:

- `get_change(CS, Field)` returns the new value, or `undefined` if the field did not change.
- `get_field(CS, Field)` returns the new value if there is one, otherwise the value in `Data`.
- `put_change(CS, Field, Value)` sets a change directly, without casting. Use it for values your code computes, never for user input.

## Trying it in the shell

Let's give the changeset a title that is too short, no body, and a `user_id` it should ignore:

```erlang
1> rr(kura_changeset).
[kura_assoc,kura_changeset,kura_column,kura_constraint,
 kura_embed,kura_field,kura_query,kura_ref]
2> Bad = blog_post:changeset(#{}, #{~"title" => ~"Hi", ~"user_id" => 42}).
...
3> Bad#kura_changeset.valid.
false
4> Bad#kura_changeset.errors.
[{body,<<"can't be blank">>},
 {title,<<"should be at least 3 character(s)">>}]
5> Bad#kura_changeset.changes.
#{title => <<"Hi">>}
```

Errors are `{Field, Message}` tuples. `user_id` is nowhere in the changes. Now a valid one that publishes:

```erlang
6> Good = blog_post:changeset(#{}, #{~"title" => ~"Draft ideas", ~"body" => ~"Notes.", ~"status" => ~"published"}).
...
7> Good#kura_changeset.changes.
#{status => published,title => <<"Draft ideas">>,
  body => <<"Notes.">>,
  published_at => {{2026,11,3},{7,48,10}}}
```

The binary `~"published"` was cast to the atom `published`, because `status` is an enum, and `published_at` was stamped. `apply_action/2` gives you the result as a plain map without touching the database, which is handy for previews and for validating input that is not stored at all:

```erlang
8> kura_changeset:apply_action(Good, insert).
{ok,#{status => published,title => <<"Draft ideas">>,
      body => <<"Notes.">>,
      published_at => {{2026,11,3},{7,48,10}}}}
```

`apply_action(Bad, insert)` returns `{error, Bad}`.

## Registering users

Users need more validation, and a password hash. `blog_user.erl` gains these functions:

```erlang
-export([
    registration_changeset/2,
    profile_changeset/2,
    password_changeset/2,
    email_changeset/2,
    role_changeset/2,
    normalize_email/1
]).

registration_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [
        email, username, password, password_confirmation
    ]),
    CS1 = kura_changeset:validate_required(CS, [email, username, password]),
    CS2 = validate_email(CS1),
    CS3 = validate_username(CS2),
    CS4 = validate_password(CS3),
    scrub_password(hash_password(CS4)).

profile_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [username]),
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
```

There is one changeset per use case. Registration may set email, username and password; a profile update may only touch the username; changing the role is a separate, admin-only operation. Splitting them keeps each allow-list as small as it can be. The private helpers they share are where the rules live. We go through them one group at a time.

### One address, one account

```erlang
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
        {error, invalid_encoding} -> CS
    end.

put_email(#kura_changeset{data = #{email := Email}} = CS, Email) ->
    kura_changeset:delete_change(CS, email);
put_email(CS, Email) ->
    kura_changeset:put_change(CS, email, Email).

reset_confirmation(CS) ->
    case kura_changeset:get_change(CS, email) of
        undefined -> CS;
        _NewEmail -> kura_changeset:put_change(CS, confirmed_at, undefined)
    end.
```

The unique index on `email` compares bytes. `Victim@B.com`, `victim@b.com` and `victim@b.com` followed by a newline are three different strings to PostgreSQL, but one mailbox to a person. If the blog stored them as typed, an attacker could register a second account for someone else's address. So the changeset rewrites the address into one canonical form before the database sees it:

- `unicode:characters_to_nfc_binary/1` puts the text in Unicode's composed form (NFC). An `é` can arrive as one code point or as `e` plus a combining accent; after NFC both are the same bytes.
- `string:casefold/1` is Unicode-aware lowercasing meant for comparisons, so it also folds letters outside ASCII.
- `string:trim/1` drops surrounding whitespace, including a trailing newline. We trim rather than reject because a pasted address often brings a newline or a space with it, and the person meant the address without it. Login trims too, so both sides agree.
- `string:casefold/1` raises `badarg` on bytes that are not valid UTF-8, and a request body can contain anything. `characters_to_nfc_binary/1` returns an error tuple for those instead. The changeset then keeps the value as it came, and `validate_length/3` reports it: it measures text in code points, and bytes that are not UTF-8 get the error `is not valid UTF-8`. The client sees an ordinary `email` error rather than a crash.

The normalisation lives in the schema, not in the code that handles the registration form, because every changeset caller gets it then: the form, the JSON API, test fixtures and `email_changeset/2`. `normalize_email/1` is exported so that login can call the same function ([Authentication](../auth-sessions/authentication.md#accounts)). Two copies of the rule would drift apart, and the first difference would be an account nobody can log in to.

The regex ends in `\z`, not `$`. In PCRE, `$` also matches just before a newline at the end of the string, so `^...$` accepts `alice@example.com\n`. `\z` matches only at the very end. Trimming already removed the newline here, but the anchor should say what it means: a format check written with `$` passes the newline through for any caller that forgets to trim.

`put_email/2` drops the change (with `kura_changeset:delete_change/2`) when the normalised address is the one already stored, so typing `Alice@Example.com` over `alice@example.com` changes nothing. Any real change goes through `reset_confirmation/1` in `email_changeset/2`: `confirmed_at` says "we proved this person owns this address", and that proof was for the old address. Without the reset, a confirmed user could switch to someone else's address and still look confirmed.

### Usernames

```erlang
validate_username(CS) ->
    CS1 = kura_changeset:validate_length(CS, username, [{min, 2}, {max, 30}]),
    kura_changeset:validate_format(CS1, username, ~"^[a-z0-9_]+\\z").
```

Usernames appear in URLs and next to every post, so the blog keeps them to lowercase ASCII letters, digits and underscores. That rules out look-alike names built from other scripts.

The `\z` matters here too. With `$`, `alice` followed by a newline passes the format check and becomes a second `alice` that looks identical on the page.

### Passwords

```erlang
validate_password(CS) ->
    CS1 = kura_changeset:validate_length(CS, password, [{min, 12}, {max, 1024}]),
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

scrub_password(#kura_changeset{changes = Changes, params = Params} = CS) ->
    Secrets = [password, password_confirmation],
    CS#kura_changeset{
        changes = maps:without(Secrets, Changes),
        params = maps:without(Secrets, Params)
    }.
```

- The length rule counts characters. On a text field `validate_length/3` counts Unicode code points, the way NIST SP 800-63B does, not bytes: in UTF-8 a Japanese character is three bytes and an emoji four, so with bytes four emoji would pass a 12 minimum and a 40-character Japanese passphrase would fail a 72 maximum. Code points are also the right unit against abuse. `string:length/1` counts grapheme clusters instead, and an attacker could then hide thousands of combining marks behind one visible letter, sending a 40 KB "12-character" password to the hash function. Bytes that are not valid UTF-8 get `is not valid UTF-8`.
- The minimum is 12 because length is what makes a password hard to guess. The maximum is not about security: 72 is a limit of bcrypt, which ignores everything after the 72nd byte. `nova_auth` hashes with PBKDF2, which uses the whole password, so the blog allows long passphrases and only caps them at 1024 characters so a single request cannot hand the hasher megabytes.
- `validate_confirmation(CS, password)` requires `password_confirmation` in the params to equal `password`.
- `hash_password/1` only runs on a valid changeset, so we never spend time hashing a password that is about to be rejected. `nova_auth_password:hash/1` comes from `nova_auth` (add it to `deps` and to `applications` in `blog.app.src`) and produces a salted PBKDF2-SHA256 hash.
- `scrub_password/1` runs last, on every path. `password` is virtual, so it is never written, but `cast/4` put the plain text in `changes`, and `params` keeps everything the client sent. A changeset travels: `blog_repo:insert/1` hands it back as `{error, CS}` for "email already taken", a controller may log it, and a crash report prints it whole. Once the hash is computed (or the changeset is known to be invalid) nothing needs the plain text, so the scrub removes both keys from both maps. `kura_changeset:delete_change/2` would clear `changes`, but nothing in Kura clears `params`, so the scrub updates the `#kura_changeset{}` record directly.

## Constraints

Validations run in Erlang before the database is involved. Some rules can only be checked by the database: "this email is not taken yet" is true only at the moment of the insert. Those are **constraints**. Kura reads the unique indexes from `indexes/0` and registers a unique constraint for each, so a duplicate email comes back as a changeset error:

```erlang
9> {error, Taken} = blog_repo:insert(blog_user:registration_changeset(#{}, #{~"email" => ~"alice@example.com", ~"username" => ~"alice2", ~"password" => ~"correct horse battery", ~"password_confirmation" => ~"correct horse battery"})).
...
10> Taken#kura_changeset.errors.
[{email,<<"has already been taken">>}]
```

The constraint Kura registered, as it appears in the changeset:

```erlang
#kura_constraint{
    type = unique,constraint = <<"users_email_index">>,
    field = email,message = <<"has already been taken">>}
```

For a constraint Kura cannot infer, declare it in the changeset: `unique_constraint/2,3`, `foreign_key_constraint/2,3` (used in the [next chapters](associations.md)), `check_constraint/3,4` and `exclusion_constraint/2,3`. Any other database error is returned as `{error, Reason}`, not as a changeset.

## Rendering errors in JSON responses

An API should return errors grouped by field. `src/blog_api_json.erl` is the blog's helper for turning records into JSON-ready maps; its `errors/1`:

```erlang
errors(#kura_changeset{errors = Errors}) ->
    Fields = lists:usort([Field || {Field, _} <- Errors]),
    #{Field => [Message || {F, Message} <- Errors, F =:= Field] || Field <- Fields}.
```

```erlang
11> blog_api_json:errors(Bad).
#{title => [<<"should be at least 3 character(s)">>],
  body => [<<"can't be blank">>]}
```

`kura_changeset:traverse_errors/2` does the same grouping and also lets you rewrite each message with a function, for example for translations.

## Validation functions

| Function | Checks |
|---|---|
| `validate_required(CS, Fields)` | Field present and not blank |
| `validate_format(CS, Field, Regex)` | Value matches a regex |
| `validate_length(CS, Field, Opts)` | `{min, N}`, `{max, N}`, `{is, N}` |
| `validate_number(CS, Field, Opts)` | `{greater_than, N}`, `{less_than, N}`, `{greater_than_or_equal_to, N}`, `{less_than_or_equal_to, N}`, `{equal_to, N}` |
| `validate_inclusion(CS, Field, List)` | Value is in the list |
| `validate_exclusion(CS, Field, List)` | Value is not in the list |
| `validate_subset(CS, Field, List)` | Every element of a list value is in the list |
| `validate_confirmation(CS, Field)` | `<Field>_confirmation` equals the field |
| `validate_change(CS, Field, Fun)` | Your own check |
| `add_error(CS, Field, Message)` | Adds an error by hand |

## Schemaless changesets

Changesets are not only for tables. Pass a map of field types instead of a schema module and you get casting and validation for any input. The posts API uses one for its query string (`?page=2&page_size=20&status=published`), in `src/blog_posts.erl`:

```erlang
list_posts(Params) ->
    Types = #{
        page => integer, page_size => integer, status => {enum, [draft, published, archived]}
    },
    CS = kura_changeset:cast(Types, #{}, Params, [page, page_size, status]),
    CS1 = kura_changeset:validate_number(CS, page, [{greater_than, 0}]),
    CS2 = kura_changeset:validate_number(CS1, page_size, [
        {greater_than, 0}, {less_than_or_equal_to, 50}
    ]),
    case kura_changeset:apply_action(CS2, list) of
        {ok, Filters} ->
            ...
        {error, _} = Error ->
            Error
    end.
```

Query string values are always binaries; the cast turns `~"2"` into `2` and `~"published"` into `published`, and rejects everything else:

```erlang
12> blog_posts:list_posts(#{~"page" => ~"two"}).
{error,#kura_changeset{valid = false,schema = undefined,
                       data = #{},
                       params = #{page => <<"two">>},
                       changes = #{},
                       errors = [{page,<<"is not a valid integer">>}],
                       ...}}
```

The `status` enum matters for security too: the client's string is matched against the three allowed values, never turned into a new atom.

We will finish `list_posts/1` in the next chapter.

## Testing changesets

Every rule above has a test in `test/blog_data_changeset_SUITE.erl`. Changeset tests that never hit the database are plain function calls:

```erlang
post_changeset_ignores_fields_not_cast(_Config) ->
    CS = blog_post:changeset(#{}, #{
        ~"title" => ~"Hello", ~"body" => ~"World", ~"user_id" => 42, ~"id" => 7
    }),
    true = CS#kura_changeset.valid,
    #{title := ~"Hello", body := ~"World"} = Changes = CS#kura_changeset.changes,
    false = maps:is_key(user_id, Changes),
    false = maps:is_key(id, Changes).
```

Tests that insert (like the unique email check) run inside a sandbox transaction that is rolled back after each test; the [next chapter](crud.md#testing-against-the-database) shows the setup. The unique email test also checks that the `{error, CS}` it gets back holds no password:

```erlang
unique_email_becomes_changeset_error(_Config) ->
    {ok, _} = blog_repo:insert(blog_user:registration_changeset(#{}, registration(#{}))),
    {error, CS} = blog_repo:insert(
        blog_user:registration_changeset(#{}, registration(#{~"username" => ~"alice2"}))
    ),
    [{email, ~"has already been taken"}] = CS#kura_changeset.errors,
    Secrets = [password, password_confirmation],
    0 = map_size(maps:with(Secrets, CS#kura_changeset.changes)),
    0 = map_size(maps:with(Secrets, CS#kura_changeset.params)).
```

```admonish tip title="One failing insert per test"
A failed statement aborts the transaction it runs in, and in a sandboxed test that is the test's own transaction: every later query in the same test fails with "current transaction is aborted". That is why `email_is_unique_after_normalisation` and `trailing_newline_is_trimmed` are two tests rather than one.
```

---

Next: putting changesets to work with [CRUD and the Query Builder](crud.md).
