# Transactions, Multi & Bulk Operations

For simple CRUD, the repo functions are enough. But some operations need atomicity (all-or-nothing), multi-step pipelines, or bulk efficiency. Kura provides transactions, multi, and bulk operations for these cases.

## Transactions

Wrap multiple operations in a transaction — if any step fails, everything rolls back:

```erlang
blog_repo:transaction(fun() ->
    CS1 = user:changeset(#{}, #{<<"username">> => <<"alice">>,
                                <<"email">> => <<"alice@example.com">>,
                                <<"password_hash">> => <<"hashed">>}),
    {ok, User} = blog_repo:insert(CS1),

    CS2 = post:changeset(#{}, #{<<"title">> => <<"Welcome">>,
                                <<"body">> => <<"Hello world">>,
                                <<"user_id">> => maps:get(id, User)}),
    {ok, _Post} = blog_repo:insert(CS2),
    ok
end).
```

If the second insert fails, the user creation is rolled back too. The transaction function returns `{ok, ReturnValue}` on success or `{error, Reason}` on failure.

## Multi: named transaction pipelines

For complex multi-step operations, `kura_multi` provides a pipeline where each step has a name and can reference results from previous steps:

```erlang
M = kura_multi:new(),

%% Step 1: Create a user
M1 = kura_multi:insert(M, create_user,
    user:changeset(#{}, #{<<"username">> => <<"alice">>,
                          <<"email">> => <<"alice@example.com">>,
                          <<"password_hash">> => <<"hashed">>})),

%% Step 2: Create a first draft, using the user ID from step 1
M2 = kura_multi:insert(M1, create_draft,
    fun(#{create_user := User}) ->
        post:changeset(#{}, #{<<"title">> => <<"My First Draft">>,
                              <<"body">> => <<"Coming soon...">>,
                              <<"user_id">> => maps:get(id, User)})
    end),

%% Step 3: Run a custom function
M3 = kura_multi:run(M2, send_welcome,
    fun(#{create_user := User}) ->
        logger:info("Welcome ~s!", [maps:get(username, User)]),
        {ok, sent}
    end),

%% Execute everything atomically
case blog_repo:multi(M3) of
    {ok, #{create_user := User, create_draft := Post, send_welcome := sent}} ->
        logger:info("User ~p created with draft post ~p",
                    [maps:get(id, User), maps:get(id, Post)]);
    {error, FailedStep, FailedValue, _Completed} ->
        logger:error("Multi failed at step ~p: ~p", [FailedStep, FailedValue])
end.
```

### Multi API

| Function | Purpose |
|---|---|
| `kura_multi:new()` | Create a new multi |
| `kura_multi:insert(M, Name, CS)` | Insert a record (changeset or fun returning changeset) |
| `kura_multi:update(M, Name, CS)` | Update a record |
| `kura_multi:delete(M, Name, CS)` | Delete a record |
| `kura_multi:run(M, Name, Fun)` | Run a custom function |

Steps that take a fun receive a map of all completed steps so far:

```erlang
fun(#{step1 := Result1, step2 := Result2}) -> ...
```

### Error handling

When a multi fails, you get the name of the failed step, the error value, and a map of steps that completed before the failure:

```erlang
case blog_repo:multi(M) of
    {ok, Results} ->
        %% All steps succeeded, Results is a map of step_name => result
        ok;
    {error, FailedStep, FailedValue, CompletedSteps} ->
        %% FailedStep: atom name of the step that failed
        %% FailedValue: the error (e.g., a changeset with errors)
        %% CompletedSteps: map of steps that succeeded (then rolled back)
        ok
end.
```

## Bulk operations

### insert_all — batch inserts

Insert many records at once:

```erlang
Posts = [
    #{title => <<"Post 1">>, body => <<"Body 1">>, status => draft, user_id => 1},
    #{title => <<"Post 2">>, body => <<"Body 2">>, status => draft, user_id => 1},
    #{title => <<"Post 3">>, body => <<"Body 3">>, status => published, user_id => 2}
],
{ok, 3} = blog_repo:insert_all(post, Posts).
```

`insert_all` bypasses changesets — it inserts raw maps directly. Use it for imports and seeding where you trust the data. The return value is the number of rows inserted.

### update_all — batch updates

Update many records matching a query:

```erlang
%% Publish all drafts
Q = kura_query:from(post),
Q1 = kura_query:where(Q, {status, draft}),
{ok, Count} = blog_repo:update_all(Q1, #{status => published}).
```

`update_all` returns the count of rows affected. It applies the updates in a single SQL statement.

### delete_all — batch deletes

Delete all records matching a query:

```erlang
%% Delete all archived posts
Q = kura_query:from(post),
Q1 = kura_query:where(Q, {status, archived}),
{ok, Count} = blog_repo:delete_all(Q1).
```

## Upserts with on_conflict

Import data without failing on duplicates:

```erlang
%% Insert a tag, do nothing if it already exists
CS = tag:changeset(#{}, #{<<"name">> => <<"erlang">>}),
{ok, Tag} = blog_repo:insert(CS, #{on_conflict => {name, nothing}}).
```

The `on_conflict` option controls what happens when a unique constraint is violated:

```erlang
%% Do nothing on conflict (skip the row)
#{on_conflict => {name, nothing}}

%% Replace all fields on conflict
#{on_conflict => {name, replace_all}}

%% Replace specific fields on conflict
#{on_conflict => {name, {replace, [updated_at]}}}

%% Use a named constraint instead of a field
#{on_conflict => {{constraint, <<"tags_name_key">>}, nothing}}
```

### Practical example: importing posts

```erlang
import_posts(Posts) ->
    lists:foreach(fun(PostData) ->
        CS = post:changeset(#{}, PostData),
        blog_repo:insert(CS, #{on_conflict => {title, nothing}})
    end, Posts).
```

## Putting it all together

A controller action that publishes a post and notifies subscribers atomically:

```erlang
publish(#{bindings := #{<<"id">> := Id}}) ->
    case blog_repo:get(post, binary_to_integer(Id)) of
        {ok, #{status := draft} = Post} ->
            M = kura_multi:new(),
            M1 = kura_multi:update(M, publish_post,
                post:changeset(Post, #{<<"status">> => <<"published">>})),
            M2 = kura_multi:run(M1, notify,
                fun(#{publish_post := Published}) ->
                    nova_pubsub:broadcast(posts, "post_published", Published),
                    {ok, notified}
                end),
            case blog_repo:multi(M2) of
                {ok, #{publish_post := Published}} ->
                    {json, post_to_json(Published)};
                {error, _Step, _Value, _} ->
                    {status, 422, #{}, #{error => <<"failed to publish">>}}
            end;
        {ok, _} ->
            {status, 422, #{}, #{error => <<"only drafts can be published">>}};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"post not found">>}}
    end.
```

---

That covers the data layer — schemas, changesets, CRUD, associations, and now transactions and bulk operations. Next we shift to authentication: let's add user [sessions](../auth-sessions/sessions.md) to our application.
