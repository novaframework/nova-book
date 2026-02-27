# Changesets and Validation

In the previous chapter we defined schemas and generated migrations. Before we can insert or update data, we need to validate it. Kura uses **changesets** — a data structure that tracks what fields changed, validates them, and accumulates errors. No exceptions, no side effects — just data in, data out.

## The changeset concept

A changeset takes three inputs:
1. **Data** — the existing record (or `#{}` for a new one)
2. **Params** — the incoming data (typically from a request body)
3. **Allowed fields** — which params are permitted (everything else is ignored)

It produces a `#kura_changeset{}` record with:
- `changes` — a map of field → new value
- `errors` — a list of `{field, message}` tuples
- `valid` — `true` or `false`

## Adding changeset functions to schemas

Let's add a `changeset/2` function to the post schema. Update `src/schemas/post.erl`:

```erlang
-module(post).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0, changeset/2]).

table() -> <<"posts">>.

primary_key() -> id.

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = title, type = string, nullable = false},
        #kura_field{name = body, type = text},
        #kura_field{name = status, type = {enum, [draft, published, archived]}, default = draft},
        #kura_field{name = user_id, type = integer},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(post, Data, Params, [title, body, status, user_id]),
    CS1 = kura_changeset:validate_required(CS, [title, body]),
    CS2 = kura_changeset:validate_length(CS1, title, [{min, 3}, {max, 200}]),
    kura_changeset:validate_inclusion(CS2, status, [draft, published, archived]).
```

Here is what each step does:

1. **`cast/4`** — takes the schema module, existing data, incoming params, and a list of allowed fields. It converts param values to the correct Erlang types (binaries to atoms for enums, binaries to integers for IDs, etc.) and puts them in `changes`.
2. **`validate_required/2`** — ensures the listed fields are present and non-empty.
3. **`validate_length/3`** — checks string length constraints.
4. **`validate_inclusion/3`** — ensures the value is one of the allowed options.

## User changeset with format and unique constraints

Update `src/schemas/user.erl`:

```erlang
-module(user).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0, changeset/2]).

table() -> <<"users">>.

primary_key() -> id.

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = username, type = string, nullable = false},
        #kura_field{name = email, type = string, nullable = false},
        #kura_field{name = password_hash, type = string, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(user, Data, Params, [username, email, password_hash]),
    CS1 = kura_changeset:validate_required(CS, [username, email, password_hash]),
    CS2 = kura_changeset:validate_format(CS1, email, <<"^[^@]+@[^@]+\\.[^@]+$">>),
    CS3 = kura_changeset:validate_length(CS2, username, [{min, 2}, {max, 50}]),
    CS4 = kura_changeset:unique_constraint(CS3, email),
    kura_changeset:unique_constraint(CS4, username).
```

New validations:

- **`validate_format/3`** — checks the value against a regex. The email regex ensures it has `@` and a domain.
- **`unique_constraint/2`** — declares that this field has a unique index in the database. If an insert/update violates the constraint, Kura maps the PostgreSQL error to a friendly changeset error instead of crashing.

```admonish info
`unique_constraint` does not check uniqueness in Erlang — it tells Kura how to handle the PostgreSQL unique violation error. You still need a unique index on the column, which you would add to a migration.
```

## Changeset errors as structured data

Errors are a list of `{Field, Message}` tuples on the changeset:

```erlang
1> CS = post:changeset(#{}, #{}).
#kura_changeset{valid = false, errors = [{title, <<"can't be blank">>},
                                          {body, <<"can't be blank">>}], ...}

2> CS#kura_changeset.valid.
false

3> CS#kura_changeset.errors.
[{title, <<"can't be blank">>}, {body, <<"can't be blank">>}]
```

```erlang
4> CS2 = post:changeset(#{}, #{<<"title">> => <<"Hi">>, <<"body">> => <<"Hello">>}).
#kura_changeset{valid = false, errors = [{title, <<"should be at least 3 character(s)">>}], ...}
```

## Working with changeset fields

Kura exports helper functions for reading and modifying changeset data programmatically. These are essential when building multi-step changeset pipelines.

| Function | Purpose |
|---|---|
| `get_field(CS, Field)` | Returns value from `changes`, falling back to `data` |
| `get_change(CS, Field)` | Returns value only if it is in `changes` |
| `put_change(CS, Field, Value)` | Adds or overwrites a value in `changes` |
| `add_error(CS, Field, Msg)` | Appends a custom error and sets `valid = false` |
| `apply_changes(CS)` | Merges `changes` into `data`, returns the merged map (no persistence) |

A common use case is hashing a password before storing it:

```erlang
-export([registration_changeset/2]).

registration_changeset(Data, Params) ->
    CS = kura_changeset:cast(user, Data, Params, [username, email, password]),
    CS1 = kura_changeset:validate_required(CS, [username, email, password]),
    CS2 = kura_changeset:validate_length(CS1, password, [{min, 8}]),
    maybe_hash_password(CS2).

maybe_hash_password(#kura_changeset{valid = true, changes = #{password := Password}} = CS) ->
    Hash = bcrypt:hashpw(Password, bcrypt:gen_salt()),
    kura_changeset:put_change(CS, password_hash, list_to_binary(Hash));
maybe_hash_password(CS) ->
    CS.
```

`apply_changes/1` is useful when you need the merged result without hitting the database — for example, to preview changes or pass data to a template:

```erlang
Preview = kura_changeset:apply_changes(CS),
#{title := Title, body := Body} = Preview.
```

## Rendering errors in JSON responses

Convert changeset errors to a JSON-friendly map. A field can have multiple errors (e.g., too short *and* wrong format), so we group them into lists:

```erlang
changeset_errors_to_json(#kura_changeset{errors = Errors}) ->
    lists:foldl(fun({Field, Msg}, Acc) ->
        Key = atom_to_binary(Field),
        Existing = maps:get(Key, Acc, []),
        Acc#{Key => Existing ++ [Msg]}
    end, #{}, Errors).
```

Use it in controllers:

```erlang
create(#{json := Params}) ->
    CS = post:changeset(#{}, Params),
    case blog_repo:insert(CS) of
        {ok, Post} ->
            {json, 201, #{}, post_to_json(Post)};
        {error, #kura_changeset{} = CS1} ->
            {json, 422, #{}, #{errors => changeset_errors_to_json(CS1)}}
    end.
```

The response looks like:

```json
{
  "errors": {
    "title": ["can't be blank"],
    "body": ["can't be blank"]
  }
}
```

## Available validation functions

| Function | Purpose |
|---|---|
| `validate_required(CS, Fields)` | Fields must be present and non-empty |
| `validate_format(CS, Field, Regex)` | Value must match the regex |
| `validate_length(CS, Field, Opts)` | String length: `[{min,N}, {max,N}, {is,N}]` |
| `validate_number(CS, Field, Opts)` | Number range: `[{greater_than,N}, {less_than,N}, {greater_than_or_equal_to,N}, {less_than_or_equal_to,N}, {equal_to,N}]` |
| `validate_inclusion(CS, Field, List)` | Value must be in the list |
| `validate_change(CS, Field, Fun)` | Custom validation: `fun(Val) -> ok \| {error, Msg}` |
| `unique_constraint(CS, Field)` | Map PG unique violation to a changeset error |
| `foreign_key_constraint(CS, Field)` | Map PG FK violation to a changeset error |
| `check_constraint(CS, Name, Field, Opts)` | Map PG check constraint to a changeset error |

```admonish info
`validate_format`, `validate_length`, `validate_number`, and `validate_inclusion` only run when the field appears in `changes`. If the field was not cast, the validation is skipped. This means update changesets only validate the fields being changed — unchanged fields keep their existing values without re-validation.
```

## Schemaless changesets

For validating data that does not map to a database table (like search filters or contact forms), pass a types map instead of a schema module:

```erlang
Types = #{query => string, page => integer, per_page => integer},
CS = kura_changeset:cast(Types, #{}, Params, [query, page, per_page]),
CS1 = kura_changeset:validate_required(CS, [query]),
CS2 = kura_changeset:validate_number(CS1, per_page, [{greater_than, 0}, {less_than, 101}]).
```

Schemaless changesets cannot be persisted via the repo — they are for validation only.

---

Validations are declarative and composable. Errors are data, not exceptions. Now let's use changesets to perform [CRUD operations with the repository](crud.md).
